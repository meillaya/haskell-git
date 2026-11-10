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
import Git.Hash (ObjectId, objectIdToHex)
import Git.Object
    ( GitObject (..)
    , GitObjectType (..)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Commit (Commit, commitFromObjectFor, commitParents)
import Git.Object.Loose
    ( listLooseObjectIds
    , looseObjectPath
    , objectIdForPayloadFor
    , readLooseObject
    )
import Git.Object.Tree
    ( TreeEntry
    , TreeEntryMode (Directory)
    , treeEntryMode
    , treeEntryName
    , treeEntryObjectId
    , treeFromObjectFor
    )
import Git.Pack (PackObjectKind (..), packHeaderObjectCount)
import Git.Pack.Delta (applyDelta, parseDelta)
import Git.Pack.Index
    ( PackIndex
    , lookupPackIndexOffset
    , packIndexEntries
    , packIndexObjectIds
    , parsePackIndexFor
    )
import Git.Pack.Read (PackRawObject (..))
import Git.Pack.Stream (readPackHeaderFromFile, readPackObjectAtFile)
import Git.Repo (Repository (..), repositoryObjectsDir)

data PackFile = PackFile
    { packFilePath :: FilePath
    , packFileIndex :: PackIndex
    }

data Store = Store
    { storeRepository :: Repository
    , storePacks :: [PackFile]
    , storeMemo :: IORef (Map.Map (FilePath, Int) GitObject)
    }

maximumDeltaDepth :: Int
maximumDeltaDepth = 128

-- | Keep resolved delta bases bounded. A read command may touch thousands of
-- objects; retaining every resolved object would grow with the whole pack.
maximumMemoEntries :: Int
maximumMemoEntries = 1024

-- | Load the pack indexes. Pack contents stay on disk and are read per object,
-- so a read command never holds a whole pack in memory.
loadStore :: Repository -> IO Store
loadStore repository = do
    let objectFormat = repositoryObjectFormat repository
        packDirectory = repositoryObjectsDir repository </> "pack"
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
                    (parsePackIndexFor objectFormat indexBytes)
            packExists <- doesFileExist packPath
            if not packExists
                then ioError (userError ("missing pack file for " ++ indexPath))
                else pure ()
            header <- readPackHeaderFromFile packPath
            if packHeaderObjectCount header /= length (packIndexEntries packIndex)
                then ioError (userError ("pack index does not match the pack object count: " ++ packPath))
                else pure ()
            pure
                PackFile
                    { packFilePath = packPath
                    , packFileIndex = packIndex
                    }
    memo <- newIORef Map.empty
    pure (Store repository packs memo)

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
    objectFormat = repositoryObjectFormat (storeRepository store)

    go prefix treeId = do
        treeObject <- readObjectOrFail store treeId
        entries <- either (ioError . userError) pure (treeFromObjectFor objectFormat treeObject)
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
    objectFormat = repositoryObjectFormat (storeRepository store)

    go _ [] commits = pure (reverse commits)
    go seen (objectId:rest) commits
        | Set.member objectId seen = go seen rest commits
        | otherwise = do
            gitObjectValue <- readObjectOrFail store objectId
            currentCommit <-
                either (ioError . userError) pure (commitFromObjectFor objectFormat gitObjectValue)
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
    objectFormat = repositoryObjectFormat (storeRepository store)

    go [] = pure (Left ("object not found: " ++ objectIdToHex objectId))
    go (pack:rest) =
        case lookupPackIndexOffset (packFileIndex pack) objectId of
            Nothing -> go rest
            Just offset -> do
                result <- readObjectAtOffset store depth pack offset
                case result of
                    Left err -> pure (Left err)
                    Right gitObjectValue ->
                        if objectIdForPayloadFor objectFormat (gitObjectType gitObjectValue) (gitObjectPayload gitObjectValue) /= objectId
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
    let objectFormat = repositoryObjectFormat (storeRepository store)
        memoKey = (packFilePath pack, offset)
    cached <- Map.lookup memoKey <$> readIORef (storeMemo store)
    case cached of
        Just gitObjectValue -> pure (Right gitObjectValue)
        Nothing -> do
            result <- readPackObjectAtFile objectFormat (packFilePath pack) offset
            case result of
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
                modifyIORef' (storeMemo store) (insertBounded memoKey gitObjectValue)
                pure (Right gitObjectValue)

    insertBounded memoKey gitObjectValue currentMemo =
        let capped =
                if Map.size currentMemo >= maximumMemoEntries
                    then Map.deleteMin currentMemo
                    else currentMemo
         in Map.insert memoKey gitObjectValue capped

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
