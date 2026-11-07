module Git.Clone
    ( CloneResult (..)
    , cloneRepository
    ) where

import Control.Exception (onException)
import Control.Monad (forM, forM_, when)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl, isSpace)
import Data.List (find, isPrefixOf, nub)
import qualified Data.Map.Strict as Map
import Network.HTTP.Simple
    ( Response
    , getResponseBody
    , getResponseHeaders
    , getResponseStatusCode
    , httpBS
    , parseRequest
    , setRequestBodyLBS
    , setRequestHeader
    , setRequestMethod
    )
import Network.HTTP.Types.Header (hAccept, hContentType)
import Numeric (readHex)
import System.Directory
    ( Permissions (..)
    , createDirectoryIfMissing
    , createFileLink
    , doesDirectoryExist
    , doesPathExist
    , getPermissions
    , listDirectory
    , removePathForcibly
    , setPermissions
    )
import System.FilePath ((</>), takeDirectory)
import Git.Hash (ObjectId, hashBytes, objectIdBytes, objectIdLength, objectIdToHex)
import Git.Index (IndexEntry, indexEntryFromTree, writeIndex)
import Git.Object
    ( GitObject
    , GitObjectType (..)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Commit (commitFromObject, commitTree)
import Git.Object.Loose (objectIdForObject, writeLooseObject)
import Git.Object.Tree
    ( TreeEntry
    , TreeEntryMode (..)
    , treeEntryMode
    , treeEntryName
    , treeEntryObjectId
    )
import Git.Pack (PackObjectKind (..), packHeaderObjectCount, parsePackHeader)
import Git.Pack.Delta (applyDelta, parseDelta)
import Git.Pack.Read (PackRawObject (..), parsePackObjectAt)
import Git.Protocol.PacketLine (encodePacketLine, flushPacket, packetLine)
import Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement
    , DiscoveryRef
    , SmartHttpEndpoint (..)
    , capabilityValue
    , defaultSmartHttpEndpoint
    , discoveryCapabilities
    , discoveryRefName
    , discoveryRefObjectId
    , discoveryRefs
    , parseDiscoveryResponse
    )
import Git.Refs
    ( symbolicHead
    , writeHeadReference
    , writeRefValue
    , writeSymbolicRefValue
    )
import Git.Repo
    ( Repository (..)
    , initRepositoryWithBranch
    , repositoryConfigPath
    , repositoryIndexPath
    , validateReferenceName
    )
import Git.Store (Store, flattenTree, loadStore, readObjectOrFail)

data CloneResult = CloneResult
    { cloneHead :: Maybe ObjectId
    , cloneLocalBranch :: FilePath
    }
    deriving (Eq, Show)

cloneRepository :: String -> FilePath -> IO CloneResult
cloneRepository remoteUrl destination = do
    validateRemoteUrl remoteUrl
    prepareDestination destination
    cloneInto remoteUrl destination `onException` removePathForcibly destination

cloneInto :: String -> FilePath -> IO CloneResult
cloneInto remoteUrl destination = do
    advertisement <- fetchAdvertisement remoteUrl
    requireSha1Repository advertisement
    let advertisedRefs =
            filter (not . isPeeledRefName . discoveryRefName) (discoveryRefs advertisement)
        branchRefs = filter (isBranchRef . discoveryRefName) advertisedRefs
        tagRefs = filter (isTagRef . discoveryRefName) advertisedRefs
    if null branchRefs
        then cloneEmptyRepository remoteUrl destination advertisement
        else do
            defaultBranchPath <- chooseDefaultBranch advertisement branchRefs
            let branchName = branchShortName defaultBranchPath
            validateBranchPath defaultBranchPath
            validateBranchPath ("refs/heads/" ++ branchName)
            repository <- initRepositoryWithBranch branchName destination
            headObjectId <- requireDefaultHead defaultBranchPath branchRefs
            let wantedIds = nub (map discoveryRefObjectId (branchRefs ++ tagRefs))
            packBytes <- fetchUploadPack remoteUrl (discoveryCapabilities advertisement) wantedIds
            importPack repository packBytes
            store <- loadStore repository
            checkoutTree store repository headObjectId
            writeCheckoutIndex store repository headObjectId
            writeCloneRefs repository defaultBranchPath headObjectId branchRefs tagRefs
            writeRemoteConfig repository remoteUrl defaultBranchPath
            pure (CloneResult (Just headObjectId) defaultBranchPath)

-- | This implementation hashes with SHA-1. A server that advertises another
-- object format is refused before any pack is requested.
requireSha1Repository :: DiscoveryAdvertisement -> IO ()
requireSha1Repository advertisement =
    case capabilityValue (BS8.pack "object-format") (discoveryCapabilities advertisement) of
        Nothing -> pure ()
        Just format
            | format == BS8.pack "sha1" -> pure ()
            | otherwise ->
                ioError
                    ( userError
                        ( "remote uses an unsupported object format: "
                            ++ BS8.unpack format
                        )
                    )

cloneEmptyRepository :: String -> FilePath -> DiscoveryAdvertisement -> IO CloneResult
cloneEmptyRepository remoteUrl destination advertisement = do
    let branchPath = defaultBranchForEmpty advertisement
    validateBranchPath branchPath
    repository <- initRepositoryWithBranch (branchShortName branchPath) destination
    writeHeadReference repository (symbolicHead branchPath)
    writeRemoteConfig repository remoteUrl branchPath
    pure (CloneResult Nothing branchPath)

defaultBranchForEmpty :: DiscoveryAdvertisement -> FilePath
defaultBranchForEmpty advertisement =
    case symrefTarget advertisement of
        Just target -> BS8.unpack target
        Nothing -> "refs/heads/" ++ defaultBranchName

defaultBranchName :: String
defaultBranchName = "main"

requireDefaultHead :: FilePath -> [DiscoveryRef] -> IO ObjectId
requireDefaultHead defaultBranchPath branchRefs =
    case find ((== BS8.pack defaultBranchPath) . discoveryRefName) branchRefs of
        Just branchRef -> pure (discoveryRefObjectId branchRef)
        Nothing -> ioError (userError "remote advertisement lost its default branch")

chooseDefaultBranch :: DiscoveryAdvertisement -> [DiscoveryRef] -> IO FilePath
chooseDefaultBranch advertisement branchRefs =
    case symrefTarget advertisement of
        Just target
            | isBranchRef target && advertisedBy target -> pure (BS8.unpack target)
        _ -> fallback
  where
    advertisedBy target = any ((== target) . discoveryRefName) branchRefs

    fallback =
        case find ((== BS8.pack "HEAD") . discoveryRefName) (discoveryRefs advertisement) of
            Just headRef ->
                case find ((== discoveryRefObjectId headRef) . discoveryRefObjectId) branchRefs of
                    Just branchRef -> pure (BS8.unpack (discoveryRefName branchRef))
                    Nothing -> firstBranch
            Nothing -> firstBranch

    firstBranch =
        case branchRefs of
            branchRef:_ -> pure (BS8.unpack (discoveryRefName branchRef))
            [] -> ioError (userError "remote advertises no branches")

symrefTarget :: DiscoveryAdvertisement -> Maybe ByteString
symrefTarget advertisement = do
    value <- capabilityValue (BS8.pack "symref") (discoveryCapabilities advertisement)
    case BS8.break (== ':') value of
        (headName, rest)
            | headName == BS8.pack "HEAD" && not (BS.null rest) -> Just (BS.tail rest)
        _ -> Nothing

fetchAdvertisement :: String -> IO DiscoveryAdvertisement
fetchAdvertisement remoteUrl = do
    let endpoint = defaultSmartHttpEndpoint remoteUrl
    request <- parseRequest (discoveryPath endpoint)
    response <- httpBS request
    requireSuccess "GET" (discoveryPath endpoint) response
    let contentType = lookup hContentType (getResponseHeaders response)
    case contentType of
        Just value | advertisementContentType `BS8.isPrefixOf` value -> pure ()
        _ ->
            ioError
                ( userError
                    ( "unexpected content type from the remote: "
                        ++ maybe "none" BS8.unpack contentType
                    )
                )
    case parseDiscoveryResponse (getResponseBody response) of
        Left err -> ioError (userError err)
        Right advertisement -> pure advertisement

fetchUploadPack :: String -> [ByteString] -> [ObjectId] -> IO ByteString
fetchUploadPack remoteUrl advertisedCapabilities wantedIds = do
    let endpoint = defaultSmartHttpEndpoint remoteUrl
        requestBody = BL.fromStrict (encodeWantRequest advertisedCapabilities wantedIds)
    baseRequest <- parseRequest (uploadPackPath endpoint)
    let request =
            setRequestMethod (BS8.pack "POST")
                $ setRequestHeader hContentType [BS8.pack uploadPackRequestContentType]
                $ setRequestHeader hAccept [BS8.pack uploadPackResultContentType]
                $ setRequestBodyLBS requestBody baseRequest
    response <- httpBS request
    requireSuccess "POST" (uploadPackPath endpoint) response
    case stripUploadPackPrefix (getResponseBody response) of
        Left err -> ioError (userError err)
        Right packBytes -> pure packBytes

-- | Build the upload-pack request. Capabilities go on the first want line only,
-- and only ones the server advertised.
encodeWantRequest :: [ByteString] -> [ObjectId] -> ByteString
encodeWantRequest advertisedCapabilities wantedIds =
    case wantedIds of
        [] -> BS.empty
        firstId:otherIds ->
            BS.concat
                ( wantLine firstId requestedCapabilities
                    : map (\objectId -> wantLine objectId []) otherIds
                        ++ [ encodePacketLine flushPacket
                           , encodePacketLine (packetLine (BS8.pack "done\n"))
                           ]
                )
  where
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

stripUploadPackPrefix :: ByteString -> Either String ByteString
stripUploadPackPrefix responseBody
    | BS8.isPrefixOf (BS8.pack "PACK") responseBody = Right responseBody
    | BS8.isPrefixOf (BS8.pack "0008NAK\n") responseBody = Right (BS.drop 8 responseBody)
    | Just message <- pktLineError responseBody = Left ("remote error: " ++ message)
    | otherwise =
        Left
            ( "unexpected upload-pack response: "
                ++ BS8.unpack (BS.take 60 responseBody)
            )

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

-- | Import every object in a pack. Entries are parsed in file order, then
-- resolved in passes: a reference delta may name a base that appears later in
-- the pack, so one pass is not enough.
importPack :: Repository -> ByteString -> IO ()
importPack repository packBytes = do
    validatePackChecksum packBytes
    (header, _) <- either (ioError . userError) pure (parsePackHeader packBytes)
    entries <- parseEntries (packHeaderObjectCount header) packHeaderLength []
    resolved <- either (ioError . userError) pure (resolvePackEntries entries)
    forM_ (Map.toList resolved) $ \(objectId, gitObjectValue) -> do
        writtenObjectId <- writeLooseObject repository gitObjectValue
        when (writtenObjectId /= objectId) $
            ioError (userError "pack object content does not hash to its id")
  where
    parseEntries 0 _ accumulated = pure (reverse accumulated)
    parseEntries remaining offset accumulated = do
        (rawObject, consumed) <-
            either (ioError . userError) pure (parsePackObjectAt packBytes offset)
        parseEntries
            (remaining - 1)
            (offset + consumed)
            ((offset, rawObject) : accumulated)

validatePackChecksum :: ByteString -> IO ()
validatePackChecksum packBytes
    | BS.length packBytes < objectIdLength + packHeaderLength =
        ioError (userError "malformed pack: file is too short")
    | objectIdBytes (hashBytes body) /= trailer =
        ioError (userError "malformed pack: checksum mismatch")
    | otherwise = pure ()
  where
    body = BS.take (BS.length packBytes - objectIdLength) packBytes
    trailer = BS.drop (BS.length packBytes - objectIdLength) packBytes

resolvePackEntries :: [(Int, PackRawObject)] -> Either String (Map.Map ObjectId GitObject)
resolvePackEntries entries = go Map.empty Map.empty entries
  where
    go resolvedByOffset resolvedByObjectId pending
        | null pending = Right resolvedByObjectId
        | otherwise = do
            attempted <- traverse (resolveOne resolvedByOffset resolvedByObjectId) pending
            let newlyResolved =
                    [ (offset, gitObjectValue)
                    | ((offset, _), Just gitObjectValue) <- zip pending attempted
                    ]
                stillPending =
                    [ entry
                    | (entry, Nothing) <- zip pending attempted
                    ]
            if null newlyResolved
                then
                    Left
                        ( "pack has "
                            ++ show (length stillPending)
                            ++ " objects whose delta bases are missing"
                        )
                else do
                    let nextByOffset = Map.union (Map.fromList newlyResolved) resolvedByOffset
                        nextByObjectId =
                            Map.union
                                ( Map.fromList
                                    [ (objectIdForObject gitObjectValue, gitObjectValue)
                                    | (_, gitObjectValue) <- newlyResolved
                                    ]
                                )
                                resolvedByObjectId
                    go nextByOffset nextByObjectId stillPending

resolveOne
    :: Map.Map Int GitObject
    -> Map.Map ObjectId GitObject
    -> (Int, PackRawObject)
    -> Either String (Maybe GitObject)
resolveOne resolvedByOffset resolvedByObjectId (offset, rawObject) =
    case rawObject of
        PackRawFull objectKind payload -> Just <$> packObjectToGitObject objectKind payload
        PackRawOffsetDelta baseDistance delta ->
            case Map.lookup (offset - baseDistance) resolvedByOffset of
                Nothing -> Right Nothing
                Just baseObject -> Just <$> applyDeltaToObject baseObject delta
        PackRawRefDelta baseObjectId delta ->
            case Map.lookup baseObjectId resolvedByObjectId of
                Nothing -> Right Nothing
                Just baseObject -> Just <$> applyDeltaToObject baseObject delta

packObjectToGitObject :: PackObjectKind -> ByteString -> Either String GitObject
packObjectToGitObject objectKind payload =
    case objectKind of
        PackCommit -> Right (gitObject CommitObject payload)
        PackTree -> Right (gitObject TreeObject payload)
        PackBlob -> Right (gitObject BlobObject payload)
        PackTag -> Right (gitObject TagObject payload)
        _ -> Left "pack contains a delta entry where a full object was expected"

applyDeltaToObject :: GitObject -> ByteString -> Either String GitObject
applyDeltaToObject baseObject delta =
    case parseDelta delta of
        Left err -> Left err
        Right (baseSize, resultSize, instructions) ->
            case applyDelta baseSize resultSize (gitObjectPayload baseObject) instructions of
                Left err -> Left err
                Right payload -> Right (gitObject (gitObjectType baseObject) payload)

checkoutTree :: Store -> Repository -> ObjectId -> IO ()
checkoutTree store repository headObjectId = do
    flattened <- flattenHeadTree store headObjectId
    forM_ flattened $ \(relativePath, entry) ->
        materializeEntry store repository relativePath entry

writeCheckoutIndex :: Store -> Repository -> ObjectId -> IO ()
writeCheckoutIndex store repository headObjectId = do
    flattened <- flattenHeadTree store headObjectId
    entries <- forM flattened $ \(relativePath, entry) -> do
        size <- case treeEntryMode entry of
            Gitlink -> pure 0
            Directory -> pure 0
            _ -> fromIntegral . BS.length <$> blobPayload store entry
        pure (indexEntryFromTree relativePath entry size)
    writeIndexFile (repositoryIndexPath repository) entries

writeIndexFile :: FilePath -> [IndexEntry] -> IO ()
writeIndexFile path entries = BS.writeFile path (writeIndex entries)

flattenHeadTree :: Store -> ObjectId -> IO [(FilePath, TreeEntry)]
flattenHeadTree store headObjectId = do
    headObject <- readObjectOrFail store headObjectId
    currentCommit <- either (ioError . userError) pure (commitFromObject headObject)
    flattenTree store (commitTree currentCommit)

materializeEntry :: Store -> Repository -> FilePath -> TreeEntry -> IO ()
materializeEntry store repository relativePath entry = do
    validateRelativePath relativePath
    let destinationPath = repositoryRootDir repository </> relativePath
    case treeEntryMode entry of
        Directory -> pure ()
        Gitlink -> createDirectoryIfMissing True destinationPath
        RegularFile -> writeBlob destinationPath False
        ExecutableFile -> writeBlob destinationPath True
        Symlink -> do
            target <- blobPayload store entry
            createDirectoryIfMissing True (takeDirectory destinationPath)
            removePathIfPresent destinationPath
            createFileLink (BS8.unpack target) destinationPath
  where
    writeBlob destinationPath isExecutable = do
        payload <- blobPayload store entry
        createDirectoryIfMissing True (takeDirectory destinationPath)
        BS.writeFile destinationPath payload
        if isExecutable
            then do
                permissions <- getPermissions destinationPath
                setPermissions destinationPath (permissions {executable = True})
            else pure ()

blobPayload :: Store -> TreeEntry -> IO ByteString
blobPayload store entry = do
    gitObjectValue <- readObjectOrFail store (treeEntryObjectId entry)
    case gitObjectType gitObjectValue of
        BlobObject -> pure (gitObjectPayload gitObjectValue)
        _ ->
            ioError
                ( userError
                    ( "tree entry does not point at a blob: "
                        ++ BS8.unpack (treeEntryName entry)
                    )
                )

validateRelativePath :: FilePath -> IO ()
validateRelativePath relativePath =
    case splitOn '/' relativePath of
        [] -> ioError (userError "tree contains an empty path")
        firstComponent : _
            | firstComponent == "" ->
                ioError (userError ("tree contains an absolute path: " ++ relativePath))
        components
            | any isDotComponent components ->
                ioError (userError ("tree contains a dot path component: " ++ relativePath))
            | otherwise -> pure ()
  where
    isDotComponent component = component == "." || component == ".."

writeCloneRefs
    :: Repository
    -> FilePath
    -> ObjectId
    -> [DiscoveryRef]
    -> [DiscoveryRef]
    -> IO ()
writeCloneRefs repository defaultBranchPath headObjectId branchRefs tagRefs = do
    writeHeadReference repository (symbolicHead defaultBranchPath)
    writeRefValue repository defaultBranchPath headObjectId
    forM_ branchRefs $ \branchRef -> do
        let refPath = BS8.unpack (discoveryRefName branchRef)
        validateBranchPath refPath
        writeRefValue
            repository
            (remoteTrackingRefPath refPath)
            (discoveryRefObjectId branchRef)
    forM_ tagRefs $ \tagRef -> do
        let refPath = BS8.unpack (discoveryRefName tagRef)
        validateBranchPath refPath
        writeRefValue repository refPath (discoveryRefObjectId tagRef)
    writeSymbolicRefValue
        repository
        "refs/remotes/origin/HEAD"
        (remoteTrackingRefPath defaultBranchPath)

writeRemoteConfig :: Repository -> String -> FilePath -> IO ()
writeRemoteConfig repository remoteUrl defaultBranchPath = do
    let branchName = branchShortName defaultBranchPath
    appendFile
        (repositoryConfigPath repository)
        ( unlines
            [ ""
            , "[remote \"origin\"]"
            , "\turl = " ++ remoteUrl
            , "\tfetch = +refs/heads/*:refs/remotes/origin/*"
            , "[branch \"" ++ branchName ++ "\"]"
            , "\tremote = origin"
            , "\tmerge = " ++ defaultBranchPath
            ]
        )

remoteTrackingRefPath :: FilePath -> FilePath
remoteTrackingRefPath branchPath = "refs/remotes/origin/" ++ branchShortName branchPath

branchShortName :: FilePath -> FilePath
branchShortName branchPath
    | "refs/heads/" `isPrefixOf` branchPath = drop (length "refs/heads/") branchPath
    | otherwise = branchPath

prepareDestination :: FilePath -> IO ()
prepareDestination destination = do
    destinationExists <- doesPathExist destination
    if not destinationExists
        then pure ()
        else do
            isDirectory <- doesDirectoryExist destination
            if not isDirectory
                then ioError (userError ("destination exists and is not a directory: " ++ destination))
                else do
                    entries <- listDirectory destination
                    if null entries
                        then pure ()
                        else
                            ioError
                                ( userError
                                    ("destination exists and is not empty: " ++ destination)
                                )

removePathIfPresent :: FilePath -> IO ()
removePathIfPresent path = do
    pathExists <- doesPathExist path
    when pathExists (removePathForcibly path)

validateRemoteUrl :: String -> IO ()
validateRemoteUrl remoteUrl
    | null remoteUrl = ioError (userError "remote URL is empty")
    | not (any (`isPrefixOf` remoteUrl) ["http://", "https://"]) =
        ioError (userError "only http and https remotes are supported")
    | any invalidUrlCharacter remoteUrl =
        ioError (userError "remote URL contains whitespace or a control character")
    | otherwise = pure ()
  where
    invalidUrlCharacter character = isSpace character || isControl character

requireSuccess :: String -> String -> Response body -> IO ()
requireSuccess method url response =
    let status = getResponseStatusCode response
     in if status >= 200 && status < 300
            then pure ()
            else
                ioError
                    ( userError
                        ( method
                            ++ " "
                            ++ url
                            ++ " failed with HTTP status "
                            ++ show status
                        )
                    )

isBranchRef :: ByteString -> Bool
isBranchRef name = BS8.isPrefixOf (BS8.pack "refs/heads/") name

isTagRef :: ByteString -> Bool
isTagRef name = BS8.isPrefixOf (BS8.pack "refs/tags/") name

isPeeledRefName :: ByteString -> Bool
isPeeledRefName name = BS8.isSuffixOf (BS8.pack "^{}") name

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn separator text =
    let (before, rest) = break (== separator) text
     in case rest of
            [] -> [before]
            (_:after) -> before : splitOn separator after

packHeaderLength :: Int
packHeaderLength = 12

advertisementContentType :: ByteString
advertisementContentType = BS8.pack "application/x-git-upload-pack-advertisement"

uploadPackRequestContentType :: String
uploadPackRequestContentType = "application/x-git-upload-pack-request"

uploadPackResultContentType :: String
uploadPackResultContentType = "application/x-git-upload-pack-result"

validateBranchPath :: FilePath -> IO ()
validateBranchPath refPath =
    case validateReferenceName refPath of
        Left err -> ioError (userError err)
        Right () -> pure ()
