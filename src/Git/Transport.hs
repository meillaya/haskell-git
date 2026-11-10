module Git.Transport
    ( RemoteUrl (..)
    , Service (..)
    , Transport (..)
    , parseRemoteUrl
    , withTemporaryPack
    , withTransport
    ) where

import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.CaseInsensitive as CI
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
    ( BodyReader
    , Manager
    , Request
    , Response
    , applyBasicAuth
    , brRead
    , getUri
    , newManager
    , parseRequest
    , responseBody
    , withResponse
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
    ( getResponseBody
    , getResponseHeaders
    , getResponseStatusCode
    , httpBS
    , setRequestBodyFile
    , setRequestBodyLBS
    , setRequestHeader
    , setRequestMethod
    )
import Network.HTTP.Types.Header (hAccept, hContentType)
import qualified Network.Socket as Socket
import Numeric (readHex)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess), ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO
    ( Handle
    , IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode)
    , hClose
    , hFlush
    , hGetLine
    , hIsTerminalDevice
    , hPutStr
    , openTempFile
    , stderr
    , stdin
    , withBinaryFile
    )
import System.Process
    ( CreateProcess (..)
    , StdStream (CreatePipe, Inherit)
    , createProcess
    , proc
    , ProcessHandle
    , readProcessWithExitCode
    , terminateProcess
    , waitForProcess
    )
import Git.Config (Config, configGet, configGetAll)
import Git.Hash (ObjectId, objectIdToHex)
import Git.Protocol.PacketLine
    ( PacketLine (..)
    , encodePacketLine
    , flushPacket
    , packetLine
    , readPacketLineFromHandle
    )
import Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement
    , SmartHttpEndpoint (..)
    , defaultSmartHttpEndpoint
    , parseDiscoveryResponseFor
    , parseRefAdvertisement
    )

data Service
    = UploadPackService
    | ReceivePackService
    deriving (Eq, Show)

-- | A parsed remote URL. Only HTTP(S), SSH, and git:// are accepted; local
-- paths are deliberately out of scope.
data RemoteUrl
    = HttpUrl String
    | SshUrl
        { remoteUser :: Maybe String
        , remoteHost :: String
        , remotePort :: Maybe Int
        , remotePath :: String
        }
    | GitUrl
        { remoteHost :: String
        , remotePort :: Maybe Int
        , remotePath :: String
        }
    deriving (Eq, Show)

data Transport = Transport
    { transportDiscovery :: Service -> IO DiscoveryAdvertisement
    , transportUploadPackToFile :: [ByteString] -> [ObjectId] -> [ObjectId] -> FilePath -> IO ()
    , transportReceivePack :: ByteString -> Maybe FilePath -> IO ByteString
    }

parseRemoteUrl :: String -> Either String RemoteUrl
parseRemoteUrl url
    | "http://" `isPrefixOf` url || "https://" `isPrefixOf` url =
        Right (HttpUrl url)
    | "ssh://" `isPrefixOf` url = parseSshUrl (drop (length "ssh://") url)
    | "git://" `isPrefixOf` url = parseGitUrl (drop (length "git://") url)
    | looksLikeScpUrl url = parseScpUrl url
    | otherwise = Left ("unsupported remote URL: " ++ url)

looksLikeScpUrl :: String -> Bool
looksLikeScpUrl url =
    case break (== ':') url of
        (before, ':' : after) ->
            not (null before)
                && not (null after)
                && not ("/" `isPrefixOf` after)
                && not ("http" `isPrefixOf` before)
        _ -> False

parseSshUrl :: String -> Either String RemoteUrl
parseSshUrl rest =
    case break (== '/') rest of
        (authority, '/' : path) ->
            case parseAuthority authority of
                Nothing -> Left ("malformed SSH URL: " ++ rest)
                Just (user, host, port) ->
                    Right
                        SshUrl
                            { remoteUser = user
                            , remoteHost = host
                            , remotePort = port
                            , remotePath = path
                            }
        _ -> Left ("malformed SSH URL: " ++ rest)

parseScpUrl :: String -> Either String RemoteUrl
parseScpUrl url =
    case break (== ':') url of
        (authority, ':' : path) ->
            case break (== '@') authority of
                (user, '@' : host) ->
                    Right
                        SshUrl
                            { remoteUser = if null user then Nothing else Just user
                            , remoteHost = host
                            , remotePort = Nothing
                            , remotePath = path
                            }
                _ ->
                    Right
                        SshUrl
                            { remoteUser = Nothing
                            , remoteHost = authority
                            , remotePort = Nothing
                            , remotePath = path
                            }
        _ -> Left ("malformed SSH URL: " ++ url)

parseGitUrl :: String -> Either String RemoteUrl
parseGitUrl rest =
    case break (== '/') rest of
        (authority, '/' : path) ->
            case parseAuthority authority of
                Nothing -> Left ("malformed git URL: " ++ rest)
                Just (_, host, port) ->
                    Right
                        GitUrl
                            { remoteHost = host
                            , remotePort = port
                            , remotePath = path
                            }
        _ -> Left ("malformed git URL: " ++ rest)

parseAuthority :: String -> Maybe (Maybe String, String, Maybe Int)
parseAuthority authority = do
    let (userPart, hostPart) =
            case break (== '@') authority of
                (user, '@' : rest) -> (Just user, rest)
                _ -> (Nothing, authority)
        (host, portPart) =
            case break (== ':') hostPart of
                (name, ':' : portText) ->
                    case reads portText of
                        [(port, "")] -> (name, Just port)
                        _ -> (name, Nothing)
                _ -> (hostPart, Nothing)
    if null host then Nothing else Just (userPart, host, portPart)

withTransport :: Config -> String -> (Transport -> IO a) -> IO a
withTransport config url action =
    case parseRemoteUrl url of
        Left err -> ioError (userError err)
        Right (HttpUrl baseUrl) -> withHttpTransport config baseUrl action
        Right sshUrl@SshUrl {} -> withSshTransport config sshUrl action
        Right gitUrl@GitUrl {} -> withGitTransport config gitUrl action

-- | OpenSSH is the transport. We start it per operation and reuse the same
-- command-line conventions git uses: @ssh [opts] host 'git-upload-pack path'@.
withSshTransport :: Config -> RemoteUrl -> (Transport -> IO a) -> IO a
withSshTransport config remote action = do
    (command, baseArguments) <- sshCommand config
    let transport =
            Transport
                { transportDiscovery =
                    \service -> sshDiscovery command baseArguments remote service
                , transportUploadPackToFile =
                    \caps wants haves destination ->
                        sshUploadPack command baseArguments remote caps wants haves destination
                , transportReceivePack =
                    \commands maybePack ->
                        sshReceivePack command baseArguments remote commands maybePack
                }
    action transport

sshCommand :: Config -> IO (FilePath, [String])
sshCommand config = do
    environmentCommand <- lookupEnv "GIT_SSH_COMMAND"
    environmentSsh <- lookupEnv "GIT_SSH"
    let configured =
            fromMaybe
                (fromMaybe "ssh" environmentSsh)
                (environmentCommand `orElse` configGet config "core.sshcommand")
        parts = words configured
    case parts of
        [] -> ioError (userError "GIT_SSH_COMMAND is empty")
        command : arguments -> pure (command, arguments)
  where
    orElse (Just value) _ = Just value
    orElse Nothing fallback = fallback

sshArguments :: [String] -> RemoteUrl -> String -> [String]
sshArguments baseArguments remote remoteCommand =
    baseArguments
        ++ portArguments
        ++ [userHost]
        ++ [remoteCommand]
  where
    portArguments =
        case remote of
            SshUrl {remotePort = Just port} -> ["-p", show port]
            _ -> []
    userHost =
        case remote of
            SshUrl {remoteUser = Just user, remoteHost = host} -> user ++ "@" ++ host
            SshUrl {remoteHost = host} -> host
            _ -> ""

sshRemotePath :: RemoteUrl -> String
sshRemotePath remote =
    case remote of
        SshUrl {remotePath = path} -> path
        _ -> ""

sshRemoteCommand :: RemoteUrl -> Service -> String
sshRemoteCommand remote service =
    serviceCommand ++ " " ++ shellQuote (sshRemotePath remote)
  where
    serviceCommand =
        case service of
            UploadPackService -> "git-upload-pack"
            ReceivePackService -> "git-receive-pack"
    shellQuote text = "'" ++ concatMap escape text ++ "'"
    escape '\'' = "'\\''"
    escape character = [character]

sshStartProcess :: FilePath -> [String] -> RemoteUrl -> Service -> IO (Handle, Handle, ProcessHandle)
sshStartProcess command arguments remote service = do
    let process =
            (proc command (sshArguments arguments remote (sshRemoteCommand remote service)))
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }
    (maybeInput, maybeOutput, _, processHandle) <- createProcess process
    case (maybeInput, maybeOutput) of
        (Just inputHandle, Just outputHandle) -> pure (inputHandle, outputHandle, processHandle)
        _ -> ioError (userError "failed to connect to the SSH process")

sshDiscovery
    :: FilePath
    -> [String]
    -> RemoteUrl
    -> Service
    -> IO DiscoveryAdvertisement
sshDiscovery command arguments remote service = do
    (inputHandle, outputHandle, processHandle) <-
        sshStartProcess command arguments remote service
    advertisement <- readRefAdvertisementFromHandle outputHandle
    hClose inputHandle
    hClose outputHandle
    terminateProcess processHandle
    _ <- waitForProcess processHandle
    pure advertisement

sshUploadPack
    :: FilePath
    -> [String]
    -> RemoteUrl
    -> [ByteString]
    -> [ObjectId]
    -> [ObjectId]
    -> FilePath
    -> IO ()
sshUploadPack command arguments remote advertisedCapabilities wantedIds haves destination = do
    (inputHandle, outputHandle, processHandle) <-
        sshStartProcess command arguments remote UploadPackService
    _ <- readRefAdvertisementFromHandle outputHandle
    BS.hPut inputHandle (encodeWantRequest advertisedCapabilities wantedIds haves)
    hFlush inputHandle
    hClose inputHandle
    writeUploadResponseFromHandle outputHandle destination
    hClose outputHandle
    exitCode <- waitForProcess processHandle
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code -> ioError (userError ("ssh upload-pack failed with exit code " ++ show code))

sshReceivePack
    :: FilePath
    -> [String]
    -> RemoteUrl
    -> ByteString
    -> Maybe FilePath
    -> IO ByteString
sshReceivePack command arguments remote commands maybePack = do
    (inputHandle, outputHandle, processHandle) <-
        sshStartProcess command arguments remote ReceivePackService
    _ <- readRefAdvertisementFromHandle outputHandle
    BS.hPut inputHandle commands
    case maybePack of
        Just packPath -> copyFileToHandle packPath inputHandle
        Nothing -> pure ()
    hFlush inputHandle
    hClose inputHandle
    response <- readAllHandle outputHandle
    hClose outputHandle
    exitCode <- waitForProcess processHandle
    case exitCode of
        ExitSuccess -> pure response
        ExitFailure code -> ioError (userError ("ssh receive-pack failed with exit code " ++ show code))

copyFileToHandle :: FilePath -> Handle -> IO ()
copyFileToHandle path outputHandle =
    withBinaryFile path ReadMode $ \inputHandle -> loop inputHandle
  where
    loop inputHandle = do
        chunk <- BS.hGetSome inputHandle 32768
        if BS.null chunk
            then pure ()
            else do
                BS.hPut outputHandle chunk
                loop inputHandle

readAllHandle :: Handle -> IO ByteString
readAllHandle handle = go []
  where
    go chunks = do
        chunk <- BS.hGetSome handle 32768
        if BS.null chunk
            then pure (BS.concat (reverse chunks))
            else go (chunk : chunks)

readRefAdvertisementFromHandle :: Handle -> IO DiscoveryAdvertisement
readRefAdvertisementFromHandle handle = go []
  where
    go payloads = do
        packet <- readPacketLineFromHandle handle
        case packet of
            Left err -> ioError (userError err)
            Right FlushPacket ->
                case parseRefAdvertisement (reverse payloads) of
                    Left err -> ioError (userError err)
                    Right advertisement -> pure advertisement
            Right (DataPacket payload) -> go (payload : payloads)

writeUploadResponseFromHandle :: Handle -> FilePath -> IO ()
writeUploadResponseFromHandle handle destination =
    writeUploadResponse (handleSource handle) destination

handleSource :: Handle -> ByteSource
handleSource handle = ByteSource (BS.hGetSome handle 32768)

writeUploadResponse :: ByteSource -> FilePath -> IO ()
writeUploadResponse source destination =
    withBinaryFile destination WriteMode $ \outputHandle -> do
        buffered <- newBufferedSource source
        loop buffered outputHandle
  where
    loop buffered outputHandle = do
        prefix <- peekExact buffered 4
        case prefix of
            Left err -> ioError (userError err)
            Right prefixBytes
                | prefixBytes == BS8.pack "PACK" ->
                    writeBuffered buffered outputHandle
                | otherwise ->
                    case parsePacketLength prefixBytes of
                        Nothing ->
                            ioError
                                ( userError
                                    ( "unexpected upload-pack response: "
                                        ++ BS8.unpack (BS.take 60 prefixBytes)
                                    )
                                )
                        Just 0 -> do
                            _ <- takeExact buffered 4
                            loop buffered outputHandle
                        Just packetLength -> do
                            packet <- takeExact buffered packetLength
                            case packet of
                                Left err -> ioError (userError err)
                                Right packetBytes ->
                                    let payload = BS.drop 4 packetBytes
                                     in if BS8.pack "ERR " `BS8.isPrefixOf` payload
                                            then
                                                ioError
                                                    ( userError
                                                        ( "remote error: "
                                                            ++ BS8.unpack (BS.drop 4 payload)
                                                        )
                                                    )
                                            else
                                                if BS8.pack "ACK " `BS8.isPrefixOf` payload
                                                    || BS8.pack "NAK" `BS8.isPrefixOf` payload
                                                    then loop buffered outputHandle
                                                    else
                                                        ioError
                                                            ( userError
                                                                ( "unexpected upload-pack packet: "
                                                                    ++ BS8.unpack (BS.take 60 payload)
                                                                )
                                                            )

newtype ByteSource = ByteSource (IO ByteString)

data BufferedSource = BufferedSource
    { bufferedSource :: ByteSource
    , bufferedPending :: IORef ByteString
    }

newBufferedSource :: ByteSource -> IO BufferedSource
newBufferedSource source = do
    pending <- newIORef BS.empty
    pure (BufferedSource source pending)

peekExact :: BufferedSource -> Int -> IO (Either String ByteString)
peekExact buffered count = do
    filled <- fillBuffer buffered count
    if filled < count
        then pure (Left "unexpected end of upload-pack response")
        else do
            pending <- readIORef (bufferedPending buffered)
            pure (Right (BS.take count pending))

takeExact :: BufferedSource -> Int -> IO (Either String ByteString)
takeExact buffered count = do
    filled <- fillBuffer buffered count
    if filled < count
        then pure (Left "unexpected end of upload-pack response")
        else do
            pending <- readIORef (bufferedPending buffered)
            let (taken, rest) = BS.splitAt count pending
            writeIORef (bufferedPending buffered) rest
            pure (Right taken)

fillBuffer :: BufferedSource -> Int -> IO Int
fillBuffer buffered count = do
    pending <- readIORef (bufferedPending buffered)
    if BS.length pending >= count
        then pure (BS.length pending)
        else do
            next <- sourceRead (bufferedSource buffered)
            if BS.null next
                then pure (BS.length pending)
                else do
                    writeIORef (bufferedPending buffered) (pending <> next)
                    fillBuffer buffered count

sourceRead :: ByteSource -> IO ByteString
sourceRead (ByteSource action) = action

writeBuffered :: BufferedSource -> Handle -> IO ()
writeBuffered buffered outputHandle = do
    pending <- readIORef (bufferedPending buffered)
    BS.hPut outputHandle pending
    writeIORef (bufferedPending buffered) BS.empty
    copySource (bufferedSource buffered) outputHandle

copySource :: ByteSource -> Handle -> IO ()
copySource source outputHandle = do
    chunk <- sourceRead source
    if BS.null chunk
        then pure ()
        else do
            BS.hPut outputHandle chunk
            copySource source outputHandle

parsePacketLength :: ByteString -> Maybe Int
parsePacketLength bytes
    | BS.length bytes < 4 = Nothing
    | otherwise =
        case readHex (BS8.unpack (BS.take 4 bytes)) of
            [(0, "")] -> Just 0
            [(packetLength, "")]
                | packetLength >= 4 && packetLength <= 65520 -> Just packetLength
            _ -> Nothing

-- | The git:// protocol is a TCP socket with pkt-line framing.
withGitTransport :: Config -> RemoteUrl -> (Transport -> IO a) -> IO a
withGitTransport _config remote action =
    let transport =
            Transport
                { transportDiscovery = gitDiscovery remote
                , transportUploadPackToFile =
                    \caps wants haves destination ->
                        gitUploadPack remote caps wants haves destination
                , transportReceivePack =
                    \commands maybePack ->
                        gitReceivePack remote commands maybePack
                }
     in action transport

gitHostPort :: RemoteUrl -> (String, Int)
gitHostPort remote =
    case remote of
        GitUrl {remoteHost = host, remotePort = port} -> (host, fromMaybe 9418 port)
        _ -> ("127.0.0.1", 9418)

gitDiscovery :: RemoteUrl -> Service -> IO DiscoveryAdvertisement
gitDiscovery remote service = do
    handle <- gitConnect remote service
    advertisement <- readRefAdvertisementFromHandle handle
    hClose handle
    pure advertisement

gitUploadPack
    :: RemoteUrl
    -> [ByteString]
    -> [ObjectId]
    -> [ObjectId]
    -> FilePath
    -> IO ()
gitUploadPack remote advertisedCapabilities wantedIds haves destination = do
    handle <- gitConnect remote UploadPackService
    _ <- readRefAdvertisementFromHandle handle
    BS.hPut handle (encodeWantRequest advertisedCapabilities wantedIds haves)
    hFlush handle
    writeUploadResponseFromHandle handle destination
    hClose handle

gitReceivePack :: RemoteUrl -> ByteString -> Maybe FilePath -> IO ByteString
gitReceivePack remote commands maybePack = do
    (handle, _socketHandle) <- gitConnectWithSocket remote ReceivePackService
    _ <- readRefAdvertisementFromHandle handle
    BS.hPut handle commands
    case maybePack of
        Just packPath -> copyFileToHandle packPath handle
        Nothing -> pure ()
    hFlush handle
    response <- readAllHandle handle
    hClose handle
    pure response

-- | Connect to a git daemon. Closing the returned handle closes the connection.
gitConnect :: RemoteUrl -> Service -> IO Handle
gitConnect remote service = fst <$> gitConnectWithSocket remote service

gitConnectWithSocket :: RemoteUrl -> Service -> IO (Handle, Socket.Socket)
gitConnectWithSocket remote service = do
    let (host, port) = gitHostPort remote
    addresses <-
        Socket.getAddrInfo
            (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
            (Just host)
            (Just (show port))
    address <-
        case addresses of
            candidate : _ -> pure candidate
            [] -> ioError (userError ("cannot resolve git host: " ++ host))
    socketHandle <-
        Socket.socket
            (Socket.addrFamily address)
            (Socket.addrSocketType address)
            (Socket.addrProtocol address)
    Socket.connect socketHandle (Socket.addrAddress address)
    handle <- Socket.socketToHandle socketHandle ReadWriteMode
    let request =
            "/"
                ++ remotePathText remote
        command =
            case service of
                UploadPackService -> "git-upload-pack"
                ReceivePackService -> "git-receive-pack"
    BS.hPut handle (encodePacketLine (packetLine (BS8.pack (command ++ " " ++ request ++ "\0host=" ++ host ++ "\0\0"))))
    hFlush handle
    pure (handle, socketHandle)

remotePathText :: RemoteUrl -> String
remotePathText remote =
    case remote of
        GitUrl {remotePath = path} -> path
        _ -> ""


withHttpTransport :: Config -> String -> (Transport -> IO a) -> IO a
withHttpTransport config baseUrl action = do
    manager <- newManager tlsManagerSettings
    let endpoint = defaultSmartHttpEndpoint (stripUserInfo baseUrl)
        urlAuth = parseUserInfo baseUrl
        transport =
            Transport
                { transportDiscovery = httpDiscovery config endpoint urlAuth
                , transportUploadPackToFile =
                    httpUploadPack manager config endpoint urlAuth
                , transportReceivePack =
                    httpReceivePack manager config endpoint urlAuth
                }
    action transport

withTemporaryPack :: (FilePath -> IO a) -> IO a
withTemporaryPack action = bracket create remove action
  where
    create = do
        temporaryDirectory <- getTemporaryDirectory
        (path, handle) <- openTempFile temporaryDirectory "haskell-git-pack"
        hClose handle
        pure path

    remove path = do
        exists <- doesFileExist path
        when exists (removeFile path)

httpDiscovery
    :: Config
    -> SmartHttpEndpoint
    -> Maybe (ByteString, ByteString)
    -> Service
    -> IO DiscoveryAdvertisement
httpDiscovery config endpoint urlAuth service = do
    let path =
            case service of
                UploadPackService -> discoveryPath endpoint
                ReceivePackService -> receiveDiscoveryPath endpoint
        expected =
            case service of
                UploadPackService ->
                    BS8.pack "application/x-git-upload-pack-advertisement"
                ReceivePackService ->
                    BS8.pack "application/x-git-receive-pack-advertisement"
    baseRequest <- parseRequest path
    let request = applyRequestConfig config urlAuth baseRequest
    response <- getWithAuth config path request
    requireSuccess "GET" path response
    case lookup hContentType (getResponseHeaders response) of
        Just value | expected `BS8.isPrefixOf` value -> pure ()
        _ ->
            ioError
                ( userError
                    ( "unexpected content type from the remote: "
                        ++ maybe "none" BS8.unpack (lookup hContentType (getResponseHeaders response))
                    )
                )
    case
        parseDiscoveryResponseFor
            (case service of
                UploadPackService -> BS8.pack "# service=git-upload-pack\n"
                ReceivePackService -> BS8.pack "# service=git-receive-pack\n")
            (getResponseBody response) of
        Left err -> ioError (userError err)
        Right advertisement -> pure advertisement

httpUploadPack
    :: Manager
    -> Config
    -> SmartHttpEndpoint
    -> Maybe (ByteString, ByteString)
    -> [ByteString]
    -> [ObjectId]
    -> [ObjectId]
    -> FilePath
    -> IO ()
httpUploadPack manager config endpoint urlAuth advertisedCapabilities wantedIds haves destination = do
    let path = uploadPackPath endpoint
        requestBody = BL.fromStrict (encodeWantRequest advertisedCapabilities wantedIds haves)
    baseRequest <- parseRequest path
    let request =
            setRequestMethod (BS8.pack "POST")
                $ setRequestHeader hContentType [BS8.pack uploadPackRequestContentType]
                $ setRequestHeader hAccept [BS8.pack uploadPackResultContentType]
                $ setRequestBodyLBS requestBody
                $ applyRequestConfig config urlAuth baseRequest
    firstAttempt <- withResponse request manager writeAuthenticatedResponse
    case firstAttempt of
        Right () -> pure ()
        Left status -> do
            credentials <- resolveHttpCredentials config (show (getUri baseRequest))
            case credentials of
                Nothing ->
                    ioError
                        (userError ("POST " ++ path ++ " failed with HTTP status " ++ show status))
                Just (user, password) -> do
                    let authenticated = applyBasicAuth user password request
                    retried <- withResponse authenticated manager writeAuthenticatedResponse
                    case retried of
                        Right () -> pure ()
                        Left retryStatus ->
                            ioError
                                ( userError
                                    ("POST " ++ path ++ " failed with HTTP status " ++ show retryStatus)
                                )
  where
    writeAuthenticatedResponse response =
        let status = getResponseStatusCode response
         in if status == 401
                then pure (Left 401)
                else
                    if status >= 200 && status < 300
                        then do
                            writeUploadPackBody (responseBody response) destination
                            pure (Right ())
                        else pure (Left status)

httpReceivePack
    :: Manager
    -> Config
    -> SmartHttpEndpoint
    -> Maybe (ByteString, ByteString)
    -> ByteString
    -> Maybe FilePath
    -> IO ByteString
httpReceivePack manager config endpoint urlAuth commands maybePack =
    withTemporaryPack $ \bodyPath -> do
        BS.writeFile bodyPath commands
        case maybePack of
            Just packPath ->
                withBinaryFile bodyPath AppendMode $ \outputHandle ->
                    copyFileToHandle packPath outputHandle
            Nothing -> pure ()
        baseRequest <- parseRequest (receivePackPath endpoint)
        let request =
                setRequestMethod (BS8.pack "POST")
                    $ setRequestHeader hContentType [BS8.pack receivePackRequestContentType]
                    $ setRequestHeader hAccept [BS8.pack receivePackResultContentType]
                    $ setRequestBodyFile bodyPath
                    $ applyRequestConfig config urlAuth baseRequest
        firstAttempt <- withResponse request manager writeReceiveResponse
        case firstAttempt of
            Right responseBodyBytes -> pure responseBodyBytes
            Left status -> do
                credentials <- resolveHttpCredentials config (show (getUri baseRequest))
                case credentials of
                    Nothing ->
                        ioError
                            ( userError
                                ( "POST "
                                    ++ receivePackPath endpoint
                                    ++ " failed with HTTP status "
                                    ++ show status
                                )
                            )
                    Just (user, password) -> do
                        let authenticated = applyBasicAuth user password request
                        retried <- withResponse authenticated manager writeReceiveResponse
                        case retried of
                            Right responseBodyBytes -> pure responseBodyBytes
                            Left retryStatus ->
                                ioError
                                    ( userError
                                        ( "POST "
                                            ++ receivePackPath endpoint
                                            ++ " failed with HTTP status "
                                            ++ show retryStatus
                                        )
                                    )
  where
    writeReceiveResponse response =
        let status = getResponseStatusCode response
         in if status == 401
                then pure (Left 401)
                else
                    if status >= 200 && status < 300
                        then Right <$> readBodyReader (responseBody response)
                        else pure (Left status)

readBodyReader :: BodyReader -> IO ByteString
readBodyReader reader = go []
  where
    go chunks = do
        chunk <- brRead reader
        if BS.null chunk
            then pure (BS.concat (reverse chunks))
            else go (chunk : chunks)

writeUploadPackBody :: BodyReader -> FilePath -> IO ()
writeUploadPackBody reader destination =
    writeUploadResponse (ByteSource (brRead reader)) destination

encodeWantRequest :: [ByteString] -> [ObjectId] -> [ObjectId] -> ByteString
encodeWantRequest advertisedCapabilities wantedIds haves =
    case wantedIds of
        [] -> BS.empty
        firstId : otherIds ->
            BS.concat
                ( wantLine firstId requestedCapabilities
                    : map (\objectId -> wantLine objectId []) otherIds
                        ++ [ encodePacketLine flushPacket
                           ]
                        ++ map haveLine haves
                        ++ [ encodePacketLine (packetLine (BS8.pack "done\n"))
                           ]
                )
  where
    haveLine objectId =
        encodePacketLine (packetLine (BS8.pack ("have " ++ objectIdToHex objectId ++ "\n")))
    wantLine objectId capabilities =
        encodePacketLine
            ( packetLine
                ( BS8.pack ("want " ++ objectIdToHex objectId)
                    <> capabilitySuffix capabilities
                    <> BS8.singleton '\n'
                )
            )
    capabilitySuffix [] = BS.empty
    capabilitySuffix capabilities =
        BS8.singleton ' ' <> BS8.intercalate (BS8.singleton ' ') capabilities
    requestedCapabilities =
        [ capability
        | capability <- [BS8.pack "ofs-delta", BS8.pack "no-progress", BS8.pack "include-tag"]
        , capability `elem` advertisedCapabilities
        ]

pktLineError :: ByteString -> Maybe String
pktLineError bytes
    | BS.length bytes < 4 = Nothing
    | otherwise =
        case readHex (BS8.unpack (BS.take 4 bytes)) of
            [(packetLength, "")]
                | packetLength >= 4 && packetLength <= BS.length bytes ->
                    let payload = BS.take (packetLength - 4) (BS.drop 4 bytes)
                     in if BS8.isPrefixOf (BS8.pack "ERR ") payload
                            then Just (BS8.unpack (BS.drop 4 payload))
                            else Nothing
            _ -> Nothing

getWithAuth
    :: Config
    -> String
    -> Request
    -> IO (Response ByteString)
getWithAuth config path request = do
    response <- httpBS request
    if getResponseStatusCode response == 401
        then do
            credentials <- resolveHttpCredentials config path
            case credentials of
                Nothing -> pure response
                Just (user, password) -> httpBS (applyBasicAuth user password request)
        else pure response

applyRequestConfig :: Config -> Maybe (ByteString, ByteString) -> Request -> Request
applyRequestConfig config urlAuth baseRequest =
    let withHeaders =
            foldl
                (\requestValue headerValue -> applyExtraHeader headerValue requestValue)
                baseRequest
                (configGetAll config "http.extraHeader")
     in case urlAuth of
            Just (user, password) -> applyBasicAuth user password withHeaders
            Nothing -> withHeaders

applyExtraHeader :: String -> Request -> Request
applyExtraHeader text request =
    case break (== ':') text of
        (name, ':' : value) ->
            setRequestHeader (CI.mk (BS8.pack (trim name))) [BS8.pack (trim value)] request
        _ -> request

resolveHttpCredentials :: Config -> String -> IO (Maybe (ByteString, ByteString))
resolveHttpCredentials config url = do
    askpass <- lookupEnv "GIT_ASKPASS"
    case askpass of
        Just path -> do
            username <- runAskpass path ("Username for '" ++ url ++ "': ")
            password <- runAskpass path ("Password for '" ++ url ++ "': ")
            case (username, password) of
                (Just user, Just secret) -> pure (Just (BS8.pack user, BS8.pack secret))
                _ -> helperCredentials
        Nothing -> helperCredentials
  where
    helperCredentials =
        case configGetAll config "credential.helper" of
            [] -> terminalCredentials
            helper : _ -> do
                let (executable, arguments) =
                        if "!" `isPrefixOf` helper
                            then ("sh", ["-c", drop 1 helper ++ " get"])
                            else (helper, ["get"])
                (exitCode, output, _) <-
                    readProcessWithExitCode executable arguments (credentialInput url)
                case exitCode of
                    ExitSuccess ->
                        case parseCredentialOutput output of
                            Just credentials -> pure (Just credentials)
                            Nothing -> terminalCredentials
                    _ -> terminalCredentials

    terminalCredentials = do
        isTerminal <- hIsTerminalDevice stdin
        if not isTerminal
            then pure Nothing
            else do
                hPutStr stderr ("Username for '" ++ url ++ "': ")
                hFlush stderr
                user <- hGetLine stdin
                hPutStr stderr ("Password for '" ++ url ++ "': ")
                hFlush stderr
                secret <- hGetLine stdin
                pure (Just (BS8.pack user, BS8.pack secret))

runAskpass :: FilePath -> String -> IO (Maybe String)
runAskpass path prompt = do
    (exitCode, output, _) <- readProcessWithExitCode path [prompt] ""
    case exitCode of
        ExitSuccess ->
            let value = trim (takeWhile (/= '\n') output)
             in if null value then pure Nothing else pure (Just value)
        _ -> pure Nothing

credentialInput :: String -> String
credentialInput url =
    "protocol=" ++ scheme ++ "\nhost=" ++ host ++ "\npath=" ++ path ++ "\n\n"
  where
    (scheme, afterScheme) =
        case break (== ':') url of
            (s, ':' : rest) -> (s, rest)
            _ -> ("https", url)
    hostWithPath = dropWhile (== '/') afterScheme
    (host, path) =
        case break (== '/') hostWithPath of
            (h, '/' : p) -> (h, p)
            (h, _) -> (h, "")

parseCredentialOutput :: String -> Maybe (ByteString, ByteString)
parseCredentialOutput output =
    let pairs = map parseLine (lines output)
        findValue key = lookup key pairs
     in case (findValue "username", findValue "password") of
            (Just user, Just secret) -> Just (BS8.pack user, BS8.pack secret)
            _ -> Nothing
  where
    parseLine line =
        case break (== '=') line of
            (key, '=' : value) -> (key, value)
            _ -> ("", "")

parseUserInfo :: String -> Maybe (ByteString, ByteString)
parseUserInfo url = do
    (_, rest) <- breakOn "://" url
    let (authority, _) = break (== '/') rest
    (userInfo, '@' : _) <- pure (break (== '@') authority)
    if null userInfo
        then Nothing
        else case break (== ':') userInfo of
            (user, ':' : password) -> Just (BS8.pack user, BS8.pack password)
            _ -> Nothing

stripUserInfo :: String -> String
stripUserInfo url =
    case breakOn "://" url of
        Nothing -> url
        Just (scheme, rest) ->
            let (authority, suffix) = break (== '/') rest
             in case break (== '@') authority of
                    (_, '@' : host) -> scheme ++ "://" ++ host ++ suffix
                    _ -> url

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack
    | null needle = Nothing
    | otherwise = go "" haystack
  where
    go _ [] = Nothing
    go prefix remaining
        | needle `isPrefixOf` remaining = Just (reverse prefix, drop (length needle) remaining)
        | otherwise = go (head remaining : prefix) (tail remaining)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

uploadPackRequestContentType :: String
uploadPackRequestContentType = "application/x-git-upload-pack-request"

uploadPackResultContentType :: String
uploadPackResultContentType = "application/x-git-upload-pack-result"

receivePackRequestContentType :: String
receivePackRequestContentType = "application/x-git-receive-pack-request"

receivePackResultContentType :: String
receivePackResultContentType = "application/x-git-receive-pack-result"

requireSuccess :: String -> String -> Response body -> IO ()
requireSuccess method url response =
    let status = getResponseStatusCode response
     in if status >= 200 && status < 300
            then pure ()
            else ioError (userError (method ++ " " ++ url ++ " failed with HTTP status " ++ show status))
