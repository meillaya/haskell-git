module Git.Store
    ( PackFile (..)
    , Store
    , flattenTree
    , listObjectIds
    , loadStore
    , objectExists
    , readObject
    , readObjectOrFail
    , storePacks
    , storeRepository
    , walkCommits
    ) where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Git.Hash (ObjectId, hashBytes, objectIdBytes, objectIdLength, objectIdToHex)
import Git.Object
    ( GitObject (..)
    , GitObjectType (..)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Commit (Commit, commitFromObject, commitParents)
import Git.Object.Loose
    ( listLooseObjectIds
    , looseObjectPath
    , objectIdForObject
    , readLooseObject
    )
import Git.Object.Tree (TreeEntry, treeEntryMode, treeEntryName, treeEntryObjectId, treeFromObject, TreeEntryMode (Directory))
import Git.Pack (PackObjectKind (..), packHeaderObjectCount, parsePackHeader)
import Git.Pack.Delta (applyDelta, parseDelta)
import Git.Pack.Index (PackIndex, lookupPackIndexOffset, packIndexEntries, packIndexObjectIds, parsePackIndex)
import Git.Pack.Read (PackRawObject (..), parsePackObjectAt)
import Git.Repo (Repository (..), repositoryObjectsDir)

data PackFile = PackFile
    { packFilePath :: FilePath
    , packFileBytes :: ByteString
    , packFileIndex :: PackIndex
    }

data Store = Store
    { storeRepository :: Repository
    , storePacks :: [PackFile]
    , storeMemo :: IORef (Map.Map (FilePath, Int) GitObject)
    }

maximumDeltaDepth :: Int
maximumDeltaDepth = 128

-- | Load the pack files once. Every command that touches an object database
-- holds one of these, so a pack is read from disk at most once per process.
loadStore :: Repository -> IO Store
loadStore repository = do
    let packDirectory = repositoryObjectsDir repository </> "pack"
    packDirectoryExists <- doesDirectoryExist packDirectory
    names <- if packDirectoryExists then listDirectory packDirectory else pure []
    packs <-
        forM [name | name <- names, ".idx" `isSuffixOf` name] $ \indexName -> do
            let indexPath = packDirectory </> indexName
                packPath = take (length indexPath - length ".idx") indexPath ++ ".pack"
            indexBytes <- BS.readFile indexPath
            packIndex <-
                either
                    (ioError . userError . (("pack index " ++ indexPath ++ ": ") ++))
                    pure
                    (parsePackIndex indexBytes)
            packExists <- doesFileExist packPath
            if not packExists
                then ioError (userError ("missing pack file for " ++ indexPath))
                else pure ()
            packBytes <- BS.readFile packPath
            (header, _) <-
                either
                    (ioError . userError . ((packPath ++ ": ") ++))
                    pure
                    (parsePackHeader packBytes)
            validatePackChecksum packPath packBytes
            if packHeaderObjectCount header /= length (packIndexEntries packIndex)
                then ioError (userError ("pack index does not match the pack object count: " ++ packPath))
                else pure ()
            pure
                PackFile
                    { packFilePath = packPath
                    , packFileBytes = packBytes
                    , packFileIndex = packIndex
                    }
    memo <- newIORef Map.empty
    pure (Store repository packs memo)

validatePackChecksum :: FilePath -> ByteString -> IO ()
validatePackChecksum packPath packBytes
    | BS.length packBytes < objectIdLength =
        ioError (userError ("pack file is too short: " ++ packPath))
    | objectIdBytes (hashBytes body) /= trailer =
        ioError (userError ("pack file checksum mismatch: " ++ packPath))
    | otherwise = pure ()
  where
    body = BS.take (BS.length packBytes - objectIdLength) packBytes
    trailer = BS.drop (BS.length packBytes - objectIdLength) packBytes

readObject :: Store -> ObjectId -> IO (Maybe GitObject)
readObject store objectId = do
    result <- readObjectInternal store maximumDeltaDepth objectId
    pure (either (const Nothing) Just result)

readObjectOrFail :: Store -> ObjectId -> IO GitObject
readObjectOrFail store objectId = do
    result <- readObjectInternal store maximumDeltaDepth objectId
    case result of
        Left err -> ioError (userError err)
        Right gitObjectValue -> pure gitObjectValue

objectExists :: Store -> ObjectId -> IO Bool
objectExists store objectId = do
    looseExists <- doesFileExist (looseObjectPath (storeRepository store) objectId)
    if looseExists
        then pure True
        else
            pure
                ( any
                    (isJust . (`lookupPackIndexOffset` objectId) . packFileIndex)
                    (storePacks store)
                )

listObjectIds :: Store -> IO [ObjectId]
listObjectIds store = do
    looseIds <- listLooseObjectIds (storeRepository store)
    let packedIds = concatMap (packIndexObjectIds . packFileIndex) (storePacks store)
    pure (Set.toList (Set.fromList (looseIds ++ packedIds)))

-- | Flatten a tree into slash-separated paths. Directory entries are followed
-- and submodule entries are kept as leaves; the caller decides how to check
-- them out.
flattenTree :: Store -> ObjectId -> IO [(FilePath, TreeEntry)]
flattenTree store rootTreeId = go "" rootTreeId
  where
    go prefix treeId = do
        treeObject <- readObjectOrFail store treeId
        entries <- either (ioError . userError) pure (treeFromObject treeObject)
        fmap concat $
            forM entries $ \entry -> do
                let name = BS8.unpack (treeEntryName entry)
                    path = if null prefix then name else prefix ++ "/" ++ name
                case treeEntryMode entry of
                    Directory -> go path (treeEntryObjectId entry)
                    _ -> pure [(path, entry)]

-- | Every commit reachable from a starting commit, visited depth first with
-- parents in order. The result starts at the given commit and carries the id
-- of each commit alongside it.
walkCommits :: Store -> ObjectId -> IO [(ObjectId, Commit)]
walkCommits store startCommitId = go Set.empty [startCommitId] []
  where
    go _ [] commits = pure (reverse commits)
    go seen (objectId:rest) commits
        | Set.member objectId seen = go seen rest commits
        | otherwise = do
            gitObjectValue <- readObjectOrFail store objectId
            currentCommit <- either (ioError . userError) pure (commitFromObject gitObjectValue)
            go
                (Set.insert objectId seen)
                (commitParents currentCommit ++ rest)
                ((objectId, currentCommit) : commits)

readObjectInternal :: Store -> Int -> ObjectId -> IO (Either String GitObject)
readObjectInternal store depth objectId
    | depth <= 0 = pure (Left "delta chain is too deep")
    | otherwise = do
        looseObjectValue <- readLooseIfPresent store objectId
        case looseObjectValue of
            Just gitObjectValue -> pure (Right gitObjectValue)
            Nothing -> readFromPacks store depth objectId

readLooseIfPresent :: Store -> ObjectId -> IO (Maybe GitObject)
readLooseIfPresent store objectId = do
    let path = looseObjectPath (storeRepository store) objectId
    looseExists <- doesFileExist path
    if not looseExists
        then pure Nothing
        else Just <$> readLooseObject (storeRepository store) objectId

readFromPacks :: Store -> Int -> ObjectId -> IO (Either String GitObject)
readFromPacks store depth objectId = go (storePacks store)
  where
    go [] = pure (Left ("object not found: " ++ objectIdToHex objectId))
    go (pack:rest) =
        case lookupPackIndexOffset (packFileIndex pack) objectId of
            Nothing -> go rest
            Just offset -> do
                result <- readObjectAtOffset store depth pack offset
                case result of
                    Left err -> pure (Left err)
                    Right gitObjectValue ->
                        if objectIdForObject gitObjectValue /= objectId
                            then
                                pure
                                    ( Left
                                        ( "packed object does not hash to its id: "
                                            ++ objectIdToHex objectId
                                        )
                                    )
                            else pure (Right gitObjectValue)

readObjectAtOffset :: Store -> Int -> PackFile -> Int -> IO (Either String GitObject)
readObjectAtOffset store depth pack offset = do
    let memoKey = (packFilePath pack, offset)
    cached <- Map.lookup memoKey <$> readIORef (storeMemo store)
    case cached of
        Just gitObjectValue -> pure (Right gitObjectValue)
        Nothing ->
            case parsePackObjectAt (packFileBytes pack) offset of
                Left err -> pure (Left err)
                Right (rawObject, _) ->
                    case rawObject of
                        PackRawFull objectKind payload ->
                            remember memoKey (toGitObject objectKind payload)
                        PackRawOffsetDelta baseDistance delta -> do
                            baseResult <-
                                readObjectAtOffset store (depth - 1) pack (offset - baseDistance)
                            case baseResult of
                                Left err -> pure (Left err)
                                Right baseObject -> remember memoKey (applyDeltaToObject baseObject delta)
                        PackRawRefDelta baseObjectId delta -> do
                            baseResult <- readObjectInternal store (depth - 1) baseObjectId
                            case baseResult of
                                Left err -> pure (Left err)
                                Right baseObject -> remember memoKey (applyDeltaToObject baseObject delta)
  where
    remember memoKey result =
        case result of
            Left err -> pure (Left err)
            Right gitObjectValue -> do
                modifyIORef' (storeMemo store) (Map.insert memoKey gitObjectValue)
                pure (Right gitObjectValue)

toGitObject :: PackObjectKind -> ByteString -> Either String GitObject
toGitObject objectKind payload =
    case objectKind of
        PackBlob -> Right (gitObject BlobObject payload)
        PackTree -> Right (gitObject TreeObject payload)
        PackCommit -> Right (gitObject CommitObject payload)
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
