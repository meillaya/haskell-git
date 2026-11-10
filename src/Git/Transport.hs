module Git.Transport
    ( RemoteUrl (..)
    , Service (..)
    , Transport (..)
    , parseRemoteUrl
    , transportUploadPackToFileFor
    , withTemporaryPack
    , withTransport
    ) where

import Control.Exception (bracket, onException)
import Control.Monad (when)
import qualified Data.CaseInsensitive as CI
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace, toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
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
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (ExitSuccess), ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO
    ( Handle
    , IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode)
    , hClose
    , hFlush
    , hGetLine
    , hIsClosed
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
import Git.Hash
    ( HashAlgorithm (..)
    , ObjectId
    , hashAlgorithmFromName
    , objectIdFromHex
    , objectIdToHex
    )
import Git.Protocol.PacketLine
    ( PacketLine (..)
    , PacketLineV2 (..)
    , decodePacketLinesV2
    , encodeDelimPacketV2
    , encodePacketLine
    , encodePacketLineV2
    , flushPacket
    , packetLine
    , readPacketLineFromHandle
    )
import Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement (..)
    , DiscoveryRef (..)
    , SmartHttpEndpoint (..)
    , capabilityValue
    , discoveryCapabilities
    , discoveryObjectFormat
    , discoveryProtocolVersion
    , discoveryRefName
    , discoveryRefObjectId
    , discoveryRefs
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
    , transportUploadPackV2ToFile :: Maybe ([ByteString] -> [ObjectId] -> [ObjectId] -> FilePath -> IO ())
    , transportReceivePack :: ByteString -> Maybe FilePath -> IO ByteString
    }

-- | Dispatch to protocol v2 when the advertisement negotiated it and the
-- transport implements it; otherwise use the v0 request path.
transportUploadPackToFileFor
    :: Transport
    -> DiscoveryAdvertisement
    -> [ByteString]
    -> [ObjectId]
    -> [ObjectId]
    -> FilePath
    -> IO ()
transportUploadPackToFileFor transport advertisement capabilities wantedIds haves destination =
    case (discoveryProtocolVersion advertisement, transportUploadPackV2ToFile transport) of
        (2, Just uploadV2) -> uploadV2 capabilities wantedIds haves destination
        _ -> transportUploadPackToFile transport capabilities wantedIds haves destination

-- | The server's first protocol message on a stream transport (SSH or
-- git://). Protocol v0 starts with a ref advertisement; protocol v2 starts
-- with a version banner followed by capability lines.
data VersionAdvertisement
    = VersionAdvertisementV0 [ByteString]
    | VersionAdvertisementV2 [ByteString]

-- | Read the version-independent head of the server's first message. The
-- payload lists are in wire order and exclude the terminating flush packet.
readVersionAdvertisement :: Handle -> IO VersionAdvertisement
readVersionAdvertisement handle = go []
  where
    go payloads = do
        packet <- readPacketLineFromHandle handle
        case packet of
            Left err -> ioError (userError err)
            Right FlushPacket ->
                case reverse payloads of
                    firstLine : remaining
                        | trimTrailingNewline firstLine == BS8.pack "version 2" ->
                            pure (VersionAdvertisementV2 remaining)
                    _ -> pure (VersionAdvertisementV0 (reverse payloads))
            Right (DataPacket payload) -> go (payload : payloads)

readPacketLinesUntilFlush :: Handle -> IO [ByteString]
readPacketLinesUntilFlush handle = go []
  where
    go payloads = do
        packet <- readPacketLineFromHandle handle
        case packet of
            Left err -> ioError (userError err)
            Right FlushPacket -> pure (reverse payloads)
            Right (DataPacket payload) -> go (payload : payloads)

-- | Read the object format out of the v2 capability lines. Advertisement
-- capability values are newline-terminated in v2, so the trailing newline has
-- to be trimmed before the name is compared.
v2ObjectFormat :: [ByteString] -> HashAlgorithm
v2ObjectFormat capabilities =
    case capabilityValue (BS8.pack "object-format") capabilities of
        Just value -> fromMaybe Sha1 (hashAlgorithmFromName (trimTrailingNewline value))
        Nothing -> Sha1

-- | Run the v2 @ls-refs@ command on an already-negotiated stream transport.
discoverRefsV2 :: Handle -> Handle -> [ByteString] -> IO DiscoveryAdvertisement
discoverRefsV2 inputHandle outputHandle capabilities = do
    let objectFormat = v2ObjectFormat capabilities
    BS.hPut inputHandle (encodeV2LsRefsRequest capabilities)
    hFlush inputHandle
    payloads <- readPacketLinesUntilFlush outputHandle
    case parseV2LsRefsPayloads objectFormat capabilities payloads of
        Left err -> ioError (userError err)
        Right advertisement -> pure advertisement

writeV2FetchResponseFromHandle :: Handle -> FilePath -> IO ()
writeV2FetchResponseFromHandle handle destination =
    writeV2FetchResponse (handleSource handle) destination

hCloseIfOpen :: Handle -> IO ()
hCloseIfOpen handle = do
    closed <- hIsClosed handle
    when (not closed) (hClose handle)

configuredProtocolVersion :: Config -> Maybe Int
configuredProtocolVersion config =
    case configGet config "protocol.version" of
        Just value ->
            case reads (trim value) of
                [(version, "")] -> Just (version :: Int)
                _ -> Nothing
        Nothing -> Nothing

-- | Protocol v2 is the default, matching git. @protocol.version=0@ or @1@
-- forces the v0 request path; any other value keeps the v2 default.
protocolVersionAllowsV2 :: Maybe Int -> Bool
protocolVersionAllowsV2 configured =
    case configured of
        Just 0 -> False
        Just 1 -> False
        _ -> True


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
-- Protocol v2 is negotiated by exporting @GIT_PROTOCOL=version=2@ to the ssh
-- child and asking OpenSSH to forward it with @-o SendEnv@; servers that do
-- not forward it answer with a v0 advertisement and we fall back.
withSshTransport :: Config -> RemoteUrl -> (Transport -> IO a) -> IO a
withSshTransport config remote action = do
    (command, baseArguments) <- sshCommand config
    variant <- sshVariant config
    let attemptV2 =
            protocolVersionAllowsV2 (configuredProtocolVersion config)
                && variantSupportsProtocolEnvironment variant
        transport =
            Transport
                { transportDiscovery =
                    \service ->
                        sshDiscovery command baseArguments remote (attemptV2 && service == UploadPackService) service
                , transportUploadPackToFile =
                    \caps wants haves destination ->
                        sshUploadPack command baseArguments remote caps wants haves destination
                , transportUploadPackV2ToFile =
                    if attemptV2
                        then
                            Just
                                ( \caps wants haves destination ->
                                    sshUploadPackV2 command baseArguments remote caps wants haves destination
                                )
                        else Nothing
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

-- | SSH implementations differ in how they forward environment variables.
-- OpenSSH forwards variables named by @SendEnv@, and that is how git passes
-- protocol v2 to the server. Variants that cannot forward the environment
-- keep the v0 path.
data SshVariant
    = SshVariantAuto
    | SshVariantOpenSsh
    | SshVariantSimple
    | SshVariantPlink
    | SshVariantPutty
    | SshVariantTortoisePlink

sshVariant :: Config -> IO SshVariant
sshVariant config = do
    environmentVariant <- lookupEnv "GIT_SSH_VARIANT"
    let configured = firstJust [environmentVariant, configGet config "ssh.variant"]
    pure $ case fmap (map toLower . trim) configured of
        Just "ssh" -> SshVariantOpenSsh
        Just "simple" -> SshVariantSimple
        Just "plink" -> SshVariantPlink
        Just "putty" -> SshVariantPutty
        Just "tortoiseplink" -> SshVariantTortoisePlink
        _ -> SshVariantAuto

variantSupportsProtocolEnvironment :: SshVariant -> Bool
variantSupportsProtocolEnvironment variant =
    case variant of
        SshVariantAuto -> True
        SshVariantOpenSsh -> True
        _ -> False

sshArguments :: [String] -> RemoteUrl -> Bool -> String -> [String]
sshArguments baseArguments remote forwardProtocol remoteCommand =
    baseArguments
        ++ portArguments
        ++ sendEnvArguments
        ++ [userHost]
        ++ [remoteCommand]
  where
    portArguments =
        case remote of
            SshUrl {remotePort = Just port} -> ["-p", show port]
            _ -> []
    sendEnvArguments =
        if forwardProtocol
            then ["-o", "SendEnv=GIT_PROTOCOL"]
            else []
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

sshStartProcess
    :: FilePath
    -> [String]
    -> RemoteUrl
    -> Service
    -> Bool
    -> IO (Handle, Handle, ProcessHandle)
sshStartProcess command arguments remote service requestProtocolV2 = do
    environment <- getEnvironment
    let forwardProtocol = requestProtocolV2 && service == UploadPackService
        process =
            ( proc
                command
                (sshArguments arguments remote forwardProtocol (sshRemoteCommand remote service))
            )
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                , env =
                    if forwardProtocol
                        then Just (protocolEnvironment environment)
                        else Nothing
                }
    (maybeInput, maybeOutput, _, processHandle) <- createProcess process
    case (maybeInput, maybeOutput) of
        (Just inputHandle, Just outputHandle) -> pure (inputHandle, outputHandle, processHandle)
        _ -> ioError (userError "failed to connect to the SSH process")
  where
    protocolEnvironment environment =
        ("GIT_PROTOCOL", "version=2")
            : filter ((/= "GIT_PROTOCOL") . fst) environment

stopSshProcess :: (Handle, Handle, ProcessHandle) -> IO ()
stopSshProcess (inputHandle, outputHandle, processHandle) = do
    hCloseIfOpen inputHandle
    hCloseIfOpen outputHandle
    terminateProcess processHandle
    _ <- waitForProcess processHandle
    pure ()

sshDiscovery
    :: FilePath
    -> [String]
    -> RemoteUrl
    -> Bool
    -> Service
    -> IO DiscoveryAdvertisement
sshDiscovery command arguments remote requestProtocolV2 service =
    bracket
        (sshStartProcess command arguments remote service requestProtocolV2)
        stopSshProcess
        ( \(inputHandle, outputHandle, _) -> do
            versionAdvertisement <- readVersionAdvertisement outputHandle
            case versionAdvertisement of
                VersionAdvertisementV2 capabilities ->
                    discoverRefsV2 inputHandle outputHandle capabilities
                VersionAdvertisementV0 payloads ->
                    case parseRefAdvertisement payloads of
                        Left err -> ioError (userError err)
                        Right advertisement -> pure advertisement
        )

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
        sshStartProcess command arguments remote UploadPackService False
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

-- | The v2 fetch path. A server that answered v2 on the discovery connection
-- but not on this one is impossible in practice; if it happens, the v0 want
-- request is sent instead so the fetch still completes.
sshUploadPackV2
    :: FilePath
    -> [String]
    -> RemoteUrl
    -> [ByteString]
    -> [ObjectId]
    -> [ObjectId]
    -> FilePath
    -> IO ()
sshUploadPackV2 command arguments remote _advertisedCapabilities wantedIds haves destination = do
    (inputHandle, outputHandle, processHandle) <-
        sshStartProcess command arguments remote UploadPackService True
    let cleanup = do
            hCloseIfOpen inputHandle
            hCloseIfOpen outputHandle
            terminateProcess processHandle
            _ <- waitForProcess processHandle
            pure ()
    ( do
        versionAdvertisement <- readVersionAdvertisement outputHandle
        case versionAdvertisement of
            VersionAdvertisementV2 capabilities -> do
                BS.hPut inputHandle (encodeV2FetchRequest capabilities wantedIds haves)
                hFlush inputHandle
                hClose inputHandle
                writeV2FetchResponseFromHandle outputHandle destination
            VersionAdvertisementV0 payloads -> do
                advertisement <-
                    case parseRefAdvertisement payloads of
                        Left err -> ioError (userError err)
                        Right parsed -> pure parsed
                BS.hPut
                    inputHandle
                    (encodeWantRequest (discoveryCapabilities advertisement) wantedIds haves)
                hFlush inputHandle
                hClose inputHandle
                writeUploadResponseFromHandle outputHandle destination
        )
        `onException` cleanup
    hCloseIfOpen outputHandle
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
        sshStartProcess command arguments remote ReceivePackService False
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

-- | The git:// protocol is a TCP socket with pkt-line framing. Protocol v2 is
-- requested with an extra @version=2@ parameter on the initial request; an
-- older daemon ignores it and sends a v0 ref advertisement, in which case the
-- v0 request path is used.
withGitTransport :: Config -> RemoteUrl -> (Transport -> IO a) -> IO a
withGitTransport config remote action = do
    let attemptV2 = protocolVersionAllowsV2 (configuredProtocolVersion config)
        transport =
            Transport
                { transportDiscovery =
                    \service -> gitDiscovery remote (attemptV2 && service == UploadPackService) service
                , transportUploadPackToFile =
                    \caps wants haves destination ->
                        gitUploadPack remote caps wants haves destination
                , transportUploadPackV2ToFile =
                    if attemptV2
                        then Just (gitUploadPackV2 remote)
                        else Nothing
                , transportReceivePack =
                    \commands maybePack ->
                        gitReceivePack remote commands maybePack
                }
    action transport

gitHostPort :: RemoteUrl -> (String, Int)
gitHostPort remote =
    case remote of
        GitUrl {remoteHost = host, remotePort = port} -> (host, fromMaybe 9418 port)
        _ -> ("127.0.0.1", 9418)

gitDiscovery :: RemoteUrl -> Bool -> Service -> IO DiscoveryAdvertisement
gitDiscovery remote requestProtocolV2 service =
    bracket
        (gitConnect remote requestProtocolV2 service)
        hClose
        ( \handle -> do
            versionAdvertisement <- readVersionAdvertisement handle
            case versionAdvertisement of
                VersionAdvertisementV2 capabilities ->
                    discoverRefsV2 handle handle capabilities
                VersionAdvertisementV0 payloads ->
                    case parseRefAdvertisement payloads of
                        Left err -> ioError (userError err)
                        Right advertisement -> pure advertisement
        )

gitUploadPack
    :: RemoteUrl
    -> [ByteString]
    -> [ObjectId]
    -> [ObjectId]
    -> FilePath
    -> IO ()
gitUploadPack remote advertisedCapabilities wantedIds haves destination = do
    handle <- gitConnect remote False UploadPackService
    _ <- readRefAdvertisementFromHandle handle
    BS.hPut handle (encodeWantRequest advertisedCapabilities wantedIds haves)
    hFlush handle
    writeUploadResponseFromHandle handle destination
    hClose handle

-- | The v2 fetch path over git://. A daemon that answered v2 during discovery
-- but not on this connection is not expected; if it happens, the v0 want
-- request is sent instead so the fetch still completes.
gitUploadPackV2
    :: RemoteUrl
    -> [ByteString]
    -> [ObjectId]
    -> [ObjectId]
    -> FilePath
    -> IO ()
gitUploadPackV2 remote _advertisedCapabilities wantedIds haves destination =
    bracket
        (gitConnect remote True UploadPackService)
        hClose
        ( \handle -> do
            versionAdvertisement <- readVersionAdvertisement handle
            case versionAdvertisement of
                VersionAdvertisementV2 capabilities -> do
                    BS.hPut handle (encodeV2FetchRequest capabilities wantedIds haves)
                    hFlush handle
                    writeV2FetchResponseFromHandle handle destination
                VersionAdvertisementV0 payloads -> do
                    advertisement <-
                        case parseRefAdvertisement payloads of
                            Left err -> ioError (userError err)
                            Right parsed -> pure parsed
                    BS.hPut
                        handle
                        (encodeWantRequest (discoveryCapabilities advertisement) wantedIds haves)
                    hFlush handle
                    writeUploadResponseFromHandle handle destination
        )

gitReceivePack :: RemoteUrl -> ByteString -> Maybe FilePath -> IO ByteString
gitReceivePack remote commands maybePack = do
    (handle, _socketHandle) <- gitConnectWithSocket remote False ReceivePackService
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
gitConnect :: RemoteUrl -> Bool -> Service -> IO Handle
gitConnect remote requestProtocolV2 service =
    fst <$> gitConnectWithSocket remote requestProtocolV2 service

gitConnectWithSocket :: RemoteUrl -> Bool -> Service -> IO (Handle, Socket.Socket)
gitConnectWithSocket remote requestProtocolV2 service = do
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
    let request = "/" ++ remotePathText remote
        command =
            case service of
                UploadPackService -> "git-upload-pack"
                ReceivePackService -> "git-receive-pack"
        requestHead =
            command ++ " " ++ request ++ "\0host=" ++ host ++ "\0"
        protocolSuffix =
            if requestProtocolV2 && service == UploadPackService
                then "\0version=2\0"
                else "\0"
    BS.hPut
        handle
        (encodePacketLine (packetLine (BS8.pack (requestHead ++ protocolSuffix))))
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
                , transportUploadPackV2ToFile =
                    Just (httpUploadPackV2 manager config endpoint urlAuth)
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
    let protocolRequest =
            if service == UploadPackService
                then setRequestHeader (CI.mk (BS8.pack "Git-Protocol")) [BS8.pack "version=2"] baseRequest
                else baseRequest
        request = applyRequestConfig config urlAuth protocolRequest
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
    let body = getResponseBody response
    case extractV2Capabilities body of
        Just capabilities
            | service == UploadPackService ->
                httpV2Discovery config endpoint urlAuth capabilities
        _ ->
            case
                parseDiscoveryResponseFor
                    (case service of
                        UploadPackService -> BS8.pack "# service=git-upload-pack\n"
                        ReceivePackService -> BS8.pack "# service=git-receive-pack\n")
                    body of
                Left err -> ioError (userError err)
                Right advertisement -> pure advertisement

extractV2Capabilities :: ByteString -> Maybe [ByteString]
extractV2Capabilities raw =
    case decodePacketLinesV2 raw of
        Left _ -> Nothing
        Right packets -> go packets
  where
    go (DataPacketV2 payload : rest)
        | payload == BS8.pack "version 2\n" = Just (collect rest)
    go (_ : rest) = go rest
    go [] = Nothing
    collect (DataPacketV2 payload : rest) = payload : collect rest
    collect _ = []

httpV2Discovery
    :: Config
    -> SmartHttpEndpoint
    -> Maybe (ByteString, ByteString)
    -> [ByteString]
    -> IO DiscoveryAdvertisement
httpV2Discovery config endpoint urlAuth capabilities = do
    manager <- newManager tlsManagerSettings
    let objectFormat = v2ObjectFormat capabilities
        requestBody = encodeV2LsRefsRequest capabilities
    baseRequest <- parseRequest (uploadPackPath endpoint)
    let request =
            setRequestMethod (BS8.pack "POST")
                $ setRequestHeader hContentType [BS8.pack uploadPackRequestContentType]
                $ setRequestHeader hAccept [BS8.pack uploadPackResultContentType]
                $ setRequestHeader (CI.mk (BS8.pack "Git-Protocol")) [BS8.pack "version=2"]
                $ setRequestBodyLBS (BL.fromStrict requestBody)
                $ applyRequestConfig config urlAuth baseRequest
    response <- getWithAuth config (uploadPackPath endpoint) request
    requireSuccess "POST" (uploadPackPath endpoint) response
    case parseV2LsRefsResponse objectFormat capabilities (getResponseBody response) of
        Left err -> ioError (userError err)
        Right advertisement -> pure advertisement

encodeV2LsRefsRequest :: [ByteString] -> ByteString
encodeV2LsRefsRequest capabilities =
    BS.concat
        ( [ encodePacketLineV2 (DataPacketV2 (BS8.pack "command=ls-refs\n"))
          , encodePacketLineV2 (DataPacketV2 (BS8.pack "agent=haskell-git/0.3\n"))
          ]
            ++ [ encodePacketLineV2 (DataPacketV2 capability)
               | capability <- capabilities
               , BS8.pack "object-format=" `BS8.isPrefixOf` capability
               ]
            ++ [ encodeDelimPacketV2
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "peel\n"))
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "symrefs\n"))
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "unborn\n"))
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "ref-prefix refs/heads/\n"))
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "ref-prefix refs/tags/\n"))
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "ref-prefix HEAD\n"))
               , encodePacketLineV2 FlushPacketV2
               ]
        )

parseV2LsRefsResponse
    :: HashAlgorithm
    -> [ByteString]
    -> ByteString
    -> Either String DiscoveryAdvertisement
parseV2LsRefsResponse objectFormat capabilities raw = do
    packets <- decodePacketLinesV2 raw
    parseV2LsRefsPayloads objectFormat capabilities [payload | DataPacketV2 payload <- packets]

-- | Parse the payload lines of an @ls-refs@ response. Stream transports read
-- the lines straight off the connection instead of decoding a whole response
-- body, so the parser is shared between the HTTP and the stream paths.
parseV2LsRefsPayloads
    :: HashAlgorithm
    -> [ByteString]
    -> [ByteString]
    -> Either String DiscoveryAdvertisement
parseV2LsRefsPayloads objectFormat capabilities payloads =
    foldl addLine (Right (capabilities, [], Nothing :: Maybe ByteString, [])) payloads
        >>= \(caps, refs, symref, peeled) ->
            Right
                ( DiscoveryAdvertisement
                    (caps ++ maybe [] (\target -> [BS8.pack ("symref=HEAD:" ++ BS8.unpack target)]) symref)
                    (reverse refs ++ reverse peeled)
                    objectFormat
                    2
                )
  where
    addLine result payload = do
        (caps, refs, symref, peeled) <- result
        case BS8.words (trimTrailingNewline payload) of
            (word : refName : attributes)
                | word == BS8.pack "unborn" ->
                    Right (caps ++ symrefAttribute attributes, refs, symrefTarget attributes, peeled)
            (objectIdText : refName : attributes) -> do
                objectId <-
                    maybe
                        (Left ("invalid object id in v2 ls-refs response: " ++ BS8.unpack objectIdText))
                        Right
                        (objectIdFromHex (BS8.unpack objectIdText))
                let symrefCapabilities = symrefAttribute attributes
                    peeledRefs =
                        [ DiscoveryRef
                            (refName <> BS8.pack "^{}")
                            peeledId
                        | attribute <- attributes
                        , Just peeledText <- [stripPrefixBytes (BS8.pack "peeled:") attribute]
                        , Just peeledId <- [objectIdFromHex (BS8.unpack peeledText)]
                        ]
                    nextSymref =
                        if refName == BS8.pack "HEAD"
                            then symrefTarget attributes
                            else symref
                Right (caps ++ symrefCapabilities, DiscoveryRef refName objectId : refs, nextSymref, peeledRefs ++ peeled)
            _ -> Right (caps, refs, symref, peeled)
    symrefAttribute attributes =
        [ BS8.pack ("symref=HEAD:" ++ BS8.unpack target)
        | attribute <- attributes
        , Just target <- [stripPrefixBytes (BS8.pack "symref-target:") attribute]
        ]
    symrefTarget attributes =
        case
            [ target
            | attribute <- attributes
            , Just target <- [stripPrefixBytes (BS8.pack "symref-target:") attribute]
            ] of
            target : _ -> Just target
            [] -> Nothing

stripPrefixBytes :: ByteString -> ByteString -> Maybe ByteString
stripPrefixBytes prefix value
    | prefix `BS.isPrefixOf` value = Just (BS.drop (BS.length prefix) value)
    | otherwise = Nothing

trimTrailingNewline :: ByteString -> ByteString
trimTrailingNewline = BS8.reverse . BS8.dropWhile (`elem` "\r\n") . BS8.reverse

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (value : rest) =
    case value of
        Just present -> Just present
        Nothing -> firstJust rest

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

httpUploadPackV2
    :: Manager
    -> Config
    -> SmartHttpEndpoint
    -> Maybe (ByteString, ByteString)
    -> [ByteString]
    -> [ObjectId]
    -> [ObjectId]
    -> FilePath
    -> IO ()
httpUploadPackV2 manager config endpoint urlAuth advertisedCapabilities wantedIds haves destination = do
    let path = uploadPackPath endpoint
        requestBody =
            BL.fromStrict (encodeV2FetchRequest advertisedCapabilities wantedIds haves)
    baseRequest <- parseRequest path
    let request =
            setRequestMethod (BS8.pack "POST")
                $ setRequestHeader hContentType [BS8.pack uploadPackRequestContentType]
                $ setRequestHeader hAccept [BS8.pack uploadPackResultContentType]
                $ setRequestHeader (CI.mk (BS8.pack "Git-Protocol")) [BS8.pack "version=2"]
                $ setRequestBodyLBS requestBody
                $ applyRequestConfig config urlAuth baseRequest
    firstAttempt <- withResponse request manager (writeV2Response destination)
    case firstAttempt of
        Right () -> pure ()
        Left status -> do
            credentials <- resolveHttpCredentials config (show (getUri baseRequest))
            case credentials of
                Nothing ->
                    ioError (userError ("POST " ++ path ++ " failed with HTTP status " ++ show status))
                Just (user, password) -> do
                    let authenticated = applyBasicAuth user password request
                    retried <- withResponse authenticated manager (writeV2Response destination)
                    case retried of
                        Right () -> pure ()
                        Left retryStatus ->
                            ioError
                                ( userError
                                    ("POST " ++ path ++ " failed with HTTP status " ++ show retryStatus)
                                )
  where
    writeV2Response targetPath response =
        let status = getResponseStatusCode response
         in if status == 401
                then pure (Left 401)
                else
                    if status >= 200 && status < 300
                        then do
                            writeV2FetchResponse (ByteSource (brRead (responseBody response))) targetPath
                            pure (Right ())
                        else pure (Left status)

encodeV2FetchRequest :: [ByteString] -> [ObjectId] -> [ObjectId] -> ByteString
encodeV2FetchRequest capabilities wantedIds haves =
    BS.concat
        ( [ encodePacketLineV2 (DataPacketV2 (BS8.pack "command=fetch\n"))
          , encodePacketLineV2 (DataPacketV2 (BS8.pack "agent=haskell-git/0.3\n"))
          ]
            ++ [ encodePacketLineV2 (DataPacketV2 capability)
               | capability <- capabilities
               , BS8.pack "object-format=" `BS8.isPrefixOf` capability
               ]
            ++ [ encodeDelimPacketV2
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "thin-pack\n"))
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "no-progress\n"))
               , encodePacketLineV2 (DataPacketV2 (BS8.pack "ofs-delta\n"))
               ]
            ++ [ encodePacketLineV2 (DataPacketV2 (BS8.pack ("want " ++ objectIdToHex objectId ++ "\n")))
               | objectId <- wantedIds
               ]
            ++ [ encodePacketLineV2 (DataPacketV2 (BS8.pack ("have " ++ objectIdToHex objectId ++ "\n")))
               | objectId <- haves
               ]
            ++ [ encodePacketLineV2 (DataPacketV2 (BS8.pack "done\n"))
               , encodePacketLineV2 FlushPacketV2
               ]
        )

data V2ResponsePacket
    = V2Data ByteString
    | V2Flush
    | V2Delim
    | V2End
    | V2RawPack

readV2ResponsePacket :: BufferedSource -> IO (Either String V2ResponsePacket)
readV2ResponsePacket buffered = do
    prefix <- peekExact buffered 4
    case prefix of
        Left err -> pure (Left err)
        Right prefixBytes
            | prefixBytes == BS8.pack "PACK" -> pure (Right V2RawPack)
            | otherwise ->
                case readHex (BS8.unpack prefixBytes) of
                    [(0, "")] -> do
                        _ <- takeExact buffered 4
                        pure (Right V2Flush)
                    [(1, "")] -> do
                        _ <- takeExact buffered 4
                        pure (Right V2Delim)
                    [(2, "")] -> do
                        _ <- takeExact buffered 4
                        pure (Right V2End)
                    [(packetLength, "")]
                        | packetLength >= 4 && packetLength <= 65520 -> do
                            packet <- takeExact buffered packetLength
                            pure (V2Data . BS.drop 4 <$> packet)
                    _ ->
                        pure
                            ( Left
                                ( "unexpected protocol v2 response: "
                                    ++ BS8.unpack (BS.take 20 prefixBytes)
                                )
                            )

writeV2FetchResponse :: ByteSource -> FilePath -> IO ()
writeV2FetchResponse source destination =
    withBinaryFile destination WriteMode $ \outputHandle -> do
        buffered <- newBufferedSource source
        loop buffered outputHandle False
  where
    loop buffered outputHandle inPack = do
        packet <- readV2ResponsePacket buffered
        case packet of
            Left err -> ioError (userError err)
            Right V2RawPack -> writeBuffered buffered outputHandle
            Right V2Flush ->
                if inPack then pure () else loop buffered outputHandle inPack
            Right V2Delim -> loop buffered outputHandle inPack
            Right V2End -> pure ()
            Right (V2Data payload)
                | payload == BS8.pack "packfile\n" ->
                    loop buffered outputHandle True
                | inPack ->
                    case BS.uncons payload of
                        Just (1, bytes) -> do
                            BS.hPut outputHandle bytes
                            loop buffered outputHandle inPack
                        Just (2, _) -> loop buffered outputHandle inPack
                        Just (3, message) ->
                            ioError (userError ("remote error: " ++ BS8.unpack message))
                        _ -> ioError (userError "unexpected sideband packet in protocol v2 fetch")
                | payload == BS8.pack "acknowledgments\n" ->
                    loop buffered outputHandle inPack
                | otherwise -> loop buffered outputHandle inPack

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
