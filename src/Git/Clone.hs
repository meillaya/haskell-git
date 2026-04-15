module Git.Clone
    ( CloneResult (..)
    , cloneRepository
    ) where

import qualified Codec.Compression.Zlib.Internal as ZlibInternal
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Git.Hash (ObjectId, hashBytes, objectIdBytes, objectIdFromBytes, objectIdToHex)
import Git.Object (GitObject, GitObjectType (BlobObject, CommitObject, TreeObject), encodeGitObject, gitObject)
import Git.Object.Blob (blobContents, blobFromObject)
import Git.Object.Commit (commitFromObject, commitTree)
import Git.Object.Loose (readLooseObject, writeLooseObject)
import Git.Object.Tree (TreeEntry, TreeEntryMode (RegularFile), treeEntryMode, treeEntryName, treeEntryObjectId, treeFromObject)
import Git.Pack (PackObjectKind (..), packHeaderObjectCount, packObjectKind, packObjectSize, parsePackHeader, parsePackObjectHeader)
import Git.Pack.Delta (applyDelta, parseDelta)
import Git.Protocol.PacketLine (encodePacketLine, flushPacket, packetLine)
import Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement,
      SmartHttpEndpoint (..),
      discoveryRefName,
      discoveryRefObjectId,
      discoveryRefs,
      defaultSmartHttpEndpoint,
      parseDiscoveryResponse )
import Git.Refs (symbolicHead, writeHeadReference, writeRefValue)
import Git.Repo (Repository, initRepository, repositoryGitDir, repositoryRootDir)
import Network.HTTP.Simple
    ( getResponseBody,
      httpBS,
      parseRequest,
      setRequestBodyLBS,
      setRequestHeader,
      setRequestMethod )
import Network.HTTP.Types.Header (hContentType)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

data CloneResult = CloneResult
    { cloneHead :: ObjectId
    , cloneLocalBranch :: FilePath
    }
    deriving (Eq, Show)

data ImportedPackObject
    = ImportedObject PackObjectKind ByteString
    | ImportedRefDelta ObjectId Int Int ByteString

cloneRepository :: String -> FilePath -> IO CloneResult
cloneRepository remoteUrl destination = do
    repository <- initRepository destination
    advertisement <- fetchAdvertisement remoteUrl
    (headObjectId, branchRefPath) <- chooseDefaultBranch advertisement
    uploadPackResponse <- fetchUploadPack remoteUrl headObjectId
    importPackResponse repository uploadPackResponse
    writeHeadReference repository (symbolicHead branchRefPath)
    writeRefValue repository branchRefPath headObjectId
    writeRefValue repository (remoteTrackingRef branchRefPath) headObjectId
    appendCloneConfig repository remoteUrl branchRefPath
    materializeHeadTree repository headObjectId
    pure (CloneResult headObjectId branchRefPath)

fetchAdvertisement :: String -> IO DiscoveryAdvertisement
fetchAdvertisement remoteUrl = do
    request <- parseRequest (discoveryPath (defaultSmartHttpEndpoint remoteUrl))
    response <- httpBS request
    case parseDiscoveryResponse (getResponseBody response) of
        Left err -> ioError (userError err)
        Right advertisement -> pure advertisement

chooseDefaultBranch :: DiscoveryAdvertisement -> IO (ObjectId, FilePath)
chooseDefaultBranch advertisement =
    case find ((== BS8.pack "HEAD") . discoveryRefName) (discoveryRefs advertisement) of
        Nothing -> ioError (userError "Smart HTTP advertisement does not include HEAD")
        Just headRef ->
            case find
                (\candidate ->
                    BS8.isPrefixOf (BS8.pack "refs/heads/") (discoveryRefName candidate)
                        && discoveryRefObjectId candidate == discoveryRefObjectId headRef
                )
                (discoveryRefs advertisement) of
                Just branchRef -> pure (discoveryRefObjectId headRef, BS8.unpack (discoveryRefName branchRef))
                Nothing -> pure (discoveryRefObjectId headRef, "refs/heads/master")

fetchUploadPack :: String -> ObjectId -> IO ByteString
fetchUploadPack remoteUrl headObjectId = do
    baseRequest <- parseRequest (uploadPackPath (defaultSmartHttpEndpoint remoteUrl))
    let requestBody =
            BL.fromStrict
                ( BS.concat
                    [ encodePacketLine
                        (packetLine
                            (BS8.pack ("want " ++ objectIdToHex headObjectId ++ " no-progress\n")))
                    , encodePacketLine flushPacket
                    , encodePacketLine (packetLine (BS8.pack "done\n"))
                    ]
                )
        request =
            setRequestMethod (BS8.pack "POST")
                $ setRequestHeader hContentType [BS8.pack "application/x-git-upload-pack-request"]
                $ setRequestBodyLBS requestBody baseRequest
    getResponseBody <$> httpBS request

importPackResponse :: Repository -> ByteString -> IO ()
importPackResponse repository responseBytes =
    case stripUploadPackPrefix responseBytes of
        Left err -> ioError (userError err)
        Right packBytes ->
            case parsePackFile packBytes of
                Left err -> ioError (userError err)
                Right packObjects -> importPackObjects repository packObjects

stripUploadPackPrefix :: ByteString -> Either String ByteString
stripUploadPackPrefix responseBytes
    | BS.isPrefixOf (BS8.pack "0008NAK\n") responseBytes = Right (BS.drop 8 responseBytes)
    | BS.isPrefixOf (BS.pack [80, 65, 67, 75]) responseBytes = Right responseBytes
    | otherwise = Left "Unsupported upload-pack response prefix"

parsePackFile :: ByteString -> Either String [ImportedPackObject]
parsePackFile packBytes
    | BS.length packBytes < 20 = Left "Malformed packfile: missing trailer checksum"
    | otherwise = do
        let (packWithoutTrailer, trailerChecksum) = BS.splitAt (BS.length packBytes - 20) packBytes
        (header, remaining) <- parsePackHeader packWithoutTrailer
        if objectIdBytes (hashBytes packWithoutTrailer) /= trailerChecksum
            then Left "Malformed packfile: checksum mismatch"
            else parseObjects (packHeaderObjectCount header) remaining
  where
    parseObjects 0 remaining
        | BS.null remaining = Right []
        | otherwise = Left "Malformed packfile: trailing bytes after object list"
    parseObjects count remaining = do
        (objectHeader, afterHeader) <- parsePackObjectHeader remaining
        case packObjectKind objectHeader of
            PackBlob -> parseInflatedObject PackBlob (packObjectSize objectHeader) afterHeader count
            PackTree -> parseInflatedObject PackTree (packObjectSize objectHeader) afterHeader count
            PackCommit -> parseInflatedObject PackCommit (packObjectSize objectHeader) afterHeader count
            PackReferenceDelta -> do
                let (baseObjectIdBytes, compressedDeltaBytes) = BS.splitAt 20 afterHeader
                baseObjectId <-
                    maybe
                        (Left "Malformed packfile: invalid ref-delta base object id")
                        Right
                        (objectIdFromBytes baseObjectIdBytes)
                (deltaBytes, rest) <- decompressMember compressedDeltaBytes
                (baseSize, resultSize, _) <- parseDelta deltaBytes
                remainingObjects <- parseObjects (count - 1) rest
                Right (ImportedRefDelta baseObjectId baseSize resultSize deltaBytes : remainingObjects)
            PackOffsetDelta ->
                Left "Offset delta pack entries are not supported in the current clone checkpoint"
            PackTag ->
                Left "Tag objects are not supported in the current clone checkpoint"

    parseInflatedObject objectKind expectedSize compressedBytes count = do
        (payload, rest) <- decompressMember compressedBytes
        if BS.length payload /= expectedSize
            then Left "Malformed packfile: object payload size mismatch"
            else do
                remainingObjects <- parseObjects (count - 1) rest
                Right (ImportedObject objectKind payload : remainingObjects)

decompressMember :: ByteString -> Either String (ByteString, ByteString)
decompressMember compressedBytes =
    case
        ZlibInternal.foldDecompressStreamWithInput
            (\chunk continuation ->
                case continuation of
                    Left err -> Left err
                    Right (chunks, leftover) -> Right (chunk : chunks, leftover))
            (\leftover -> Right ([], BL.toStrict leftover))
            (Left . show)
            (ZlibInternal.decompressST
                ZlibInternal.zlibFormat
                ZlibInternal.defaultDecompressParams {ZlibInternal.decompressAllMembers = False})
            (BL.fromStrict compressedBytes) of
        Left err -> Left ("Failed to decode pack member: " ++ err)
        Right (chunks, leftover) -> Right (BS.concat (reverse chunks), leftover)

importPackObjects :: Repository -> [ImportedPackObject] -> IO ()
importPackObjects repository packObjects = go [] packObjects
  where
    go _ [] = pure ()
    go imported (current:remaining) =
        case current of
            ImportedObject objectKind payload -> do
                let gitObjectValue = toGitObject objectKind payload
                    objectId = hashBytes (encodeGitObject gitObjectValue)
                _ <- writeLooseObject repository gitObjectValue
                go ((objectId, (objectKind, payload)) : imported) remaining
            ImportedRefDelta baseObjectId baseSize resultSize deltaBytes ->
                case lookup baseObjectId imported of
                    Nothing -> ioError (userError "Ref-delta base object was not imported earlier in the pack")
                    Just (baseKind, basePayload) ->
                        case parseDelta deltaBytes of
                            Left err -> ioError (userError err)
                            Right (_, _, instructions) ->
                                case applyDelta baseSize resultSize basePayload instructions of
                                    Left err -> ioError (userError err)
                                    Right payload -> do
                                        let gitObjectValue = toGitObject baseKind payload
                                            objectId = hashBytes (encodeGitObject gitObjectValue)
                                        _ <- writeLooseObject repository gitObjectValue
                                        go ((objectId, (baseKind, payload)) : imported) remaining

toGitObject :: PackObjectKind -> ByteString -> GitObject
toGitObject PackBlob = gitObject BlobObject
toGitObject PackTree = gitObject TreeObject
toGitObject PackCommit = gitObject CommitObject
toGitObject _ = error "Unsupported pack object kind in supported clone scenario"

appendCloneConfig :: Repository -> String -> FilePath -> IO ()
appendCloneConfig repository remoteUrl branchRefPath =
    appendFile
        (repositoryGitDir repository </> "config")
        ( unlines
            [ "[remote \"origin\"]"
            , "\turl = " ++ remoteUrl
            , "\tfetch = +refs/heads/*:refs/remotes/origin/*"
            , "[branch \"" ++ branchShortName branchRefPath ++ "\"]"
            , "\tremote = origin"
            , "\tmerge = " ++ branchRefPath
            ]
        )

materializeHeadTree :: Repository -> ObjectId -> IO ()
materializeHeadTree repository headObjectId = do
    commitObject <- readLooseObject repository headObjectId
    currentCommit <-
        either
            (ioError . userError)
            pure
            (commitFromObject commitObject)
    materializeTree repository (repositoryRootDir repository) (commitTree currentCommit)

materializeTree :: Repository -> FilePath -> ObjectId -> IO ()
materializeTree repository rootPath treeObjectId = do
    treeObject <- readLooseObject repository treeObjectId
    treeEntries <- either (ioError . userError) pure (treeFromObject treeObject)
    mapM_ (materializeEntry repository rootPath) treeEntries

materializeEntry :: Repository -> FilePath -> TreeEntry -> IO ()
materializeEntry repository rootPath entry =
    case treeEntryMode entry of
        RegularFile -> do
            blobObject <- readLooseObject repository (treeEntryObjectId entry)
            currentBlob <- either (ioError . userError) pure (blobFromObject blobObject)
            let relativePath = BS8.unpack (treeEntryName entry)
                destinationPath = rootPath </> relativePath
            createDirectoryIfMissing True (takeDirectory destinationPath)
            BS.writeFile destinationPath (blobContents currentBlob)

branchShortName :: FilePath -> String
branchShortName refPath =
    reverse (takeWhile (/= '/') (reverse refPath))

remoteTrackingRef :: FilePath -> FilePath
remoteTrackingRef refPath = "refs/remotes/origin/" ++ branchShortName refPath
