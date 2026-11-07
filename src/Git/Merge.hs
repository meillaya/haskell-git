module Git.Merge
    ( MergeResult (..)
    , isAncestor
    , mergeAbort
    , mergeContinue
    , mergeRepository
    , mergeTextContent
    , requireMergeBase
    , treeOfCommit
    ) where

import Control.Monad (foldM, forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removeFile
    )
import System.FilePath ((</>), takeDirectory)
import Git.Commit (commitCurrentIndex)
import Git.Hash (ObjectId, objectIdFromHexFor, objectIdToHex)
import Git.Index
    ( IndexEntry (..)
    , buildTreeFromIndex
    , writeIndexFor
    )
import Git.Object (gitObject, gitObjectPayload, gitObjectType, GitObjectType (..))
import Git.Object.Commit (commitFromObjectFor, commitParents, commitTree)
import Git.Object.Loose (writeLooseObject)
import Git.Object.Tree
    ( TreeEntry (..)
    , TreeEntryMode (..)
    , treeEntry
    , treeEntryMode
    , treeEntryObjectId
    , treeFromObjectFor
    , treeToObject
    )
import Git.Refs (resolveHeadReference, updateHeadReference)
import Git.Repo
    ( Repository
    , repositoryGitDir
    , repositoryIndexPath
    , repositoryMergeHeadPath
    , repositoryMergeMsgPath
    , repositoryObjectFormat
    , repositoryOrigHeadPath
    , repositoryRootDir
    )
import Git.Store
    ( Store
    , flattenTree
    , readObjectOrFail
    , storeRepository
    )
import Git.Worktree (checkoutTree, resetHard)

data MergeResult
    = MergeUpToDate
    | MergeFastForward ObjectId
    | MergeCreated ObjectId
    | MergeConflicted [FilePath]
    deriving (Eq, Show)

-- | Merge a commit into the current HEAD. Clean merges are committed
-- immediately; conflicts are left in the index for 'mergeContinue'.
mergeRepository :: Repository -> Store -> ObjectId -> IO MergeResult
mergeRepository repository store targetId = do
    headValue <- resolveHeadReference repository
    headId <-
        case headValue of
            Nothing -> ioError (userError "cannot merge into an unborn branch")
            Just value -> pure value
    if headId == targetId
        then pure MergeUpToDate
        else do
            headIsAncestor <- isAncestor store headId targetId
            if headIsAncestor
                then do
                    updateHeadReference repository targetId
                    checkoutTree repository store targetId
                    pure (MergeFastForward targetId)
                else do
                    targetIsAncestor <- isAncestor store targetId headId
                    if targetIsAncestor
                        then pure MergeUpToDate
                        else do
                            baseId <- requireMergeBase store headId targetId
                            headTree <- treeOfCommit store repository headId
                            targetTree <- treeOfCommit store repository targetId
                            baseTree <- treeOfCommit store repository baseId
                            baseEntries <- flattenTree store baseTree
                            ourEntries <- flattenTree store headTree
                            theirEntries <- flattenTree store targetTree
                            (resolved, conflicts) <-
                                mergeEntryMaps
                                    repository
                                    store
                                    (entryMap baseEntries)
                                    (entryMap ourEntries)
                                    (entryMap theirEntries)
                            treeId <-
                                if null resolved
                                    then writeLooseObject repository (treeToObject [])
                                    else buildTreeFromIndex repository resolved
                            checkoutTree repository store treeId
                            forM_ conflicts (writeConflictFile repository store)
                            let conflictEntries = concatMap conflictIndexEntries conflicts
                            BS.writeFile
                                (repositoryIndexPath repository)
                                ( writeIndexFor
                                    (repositoryObjectFormat repository)
                                    (resolved ++ conflictEntries)
                                )
                            writeMergeState repository headId targetId
                            if null conflicts
                                then do
                                    commitId <-
                                        commitCurrentIndex
                                            repository
                                            store
                                            ("Merge commit " ++ objectIdToHex targetId ++ "\n")
                                    pure (MergeCreated commitId)
                                else pure (MergeConflicted (map conflictPath conflicts))
  where
    entryMap = Map.fromList . map (\(path, entry) -> (path, entry))

data Conflict = Conflict
    { conflictPath :: FilePath
    , conflictBase :: Maybe TreeEntry
    , conflictOurs :: Maybe TreeEntry
    , conflictTheirs :: Maybe TreeEntry
    }

mergeEntryMaps
    :: Repository
    -> Store
    -> Map.Map FilePath TreeEntry
    -> Map.Map FilePath TreeEntry
    -> Map.Map FilePath TreeEntry
    -> IO ([IndexEntry], [Conflict])
mergeEntryMaps repository store baseMap ourMap theirMap =
    foldM step ([], []) paths
  where
    paths =
        sort
            ( nub
                ( Map.keys baseMap
                    ++ Map.keys ourMap
                    ++ Map.keys theirMap
                )
            )
    step (resolved, conflicts) path =
        case (Map.lookup path baseMap, Map.lookup path ourMap, Map.lookup path theirMap) of
            (base, ours, theirs)
                | ours == theirs -> pure (addEntry path ours resolved, conflicts)
                | ours == base -> pure (addEntry path theirs resolved, conflicts)
                | theirs == base -> pure (addEntry path ours resolved, conflicts)
                | Just ourEntry <- ours
                , Just theirEntry <- theirs
                , isRegularEntry ourEntry
                , isRegularEntry theirEntry
                , treeEntryMode ourEntry == treeEntryMode theirEntry -> do
                    ourPayload <- readBlob store ourEntry
                    theirPayload <- readBlob store theirEntry
                    basePayload <-
                        case base of
                            Just baseEntry | isRegularEntry baseEntry -> readBlob store baseEntry
                            _ -> pure BS.empty
                    case mergeTextContent basePayload ourPayload theirPayload of
                        Right mergedPayload -> do
                            mergedId <-
                                writeLooseObject
                                    repository
                                    (gitObject BlobObject mergedPayload)
                            let mergedEntry = ourEntry {treeEntryObjectId = mergedId}
                            pure (addEntry path (Just mergedEntry) resolved, conflicts)
                        Left _ ->
                            pure
                                ( resolved
                                , conflicts ++ [Conflict path base ours theirs]
                                )
                | otherwise ->
                    pure
                        ( resolved
                        , conflicts ++ [Conflict path base ours theirs]
                        )

    addEntry path maybeEntry entries =
        case maybeEntry of
            Nothing -> entries
            Just entry ->
                IndexEntry
                    { indexEntryPath = BS8.pack path
                    , indexEntryMode = treeEntryMode entry
                    , indexEntrySize = 0
                    , indexEntryObjectId = treeEntryObjectId entry
                    , indexEntryStage = 0
                    }
                    : entries

isRegularEntry :: TreeEntry -> Bool
isRegularEntry entry =
    treeEntryMode entry `elem` [RegularFile, ExecutableFile]

readBlob :: Store -> TreeEntry -> IO BS.ByteString
readBlob store entry = do
    gitObjectValue <- readObjectOrFail store (treeEntryObjectId entry)
    case gitObjectType gitObjectValue of
        BlobObject -> pure (gitObjectPayload gitObjectValue)
        _ -> ioError (userError "merge expected a blob")

mergeTextContent :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either () BS.ByteString
mergeTextContent base ours theirs
    | ours == base = Right theirs
    | theirs == base = Right ours
    | ours == theirs = Right ours
    | isBinary base || isBinary ours || isBinary theirs = Left ()
    | otherwise = Left ()
  where
    isBinary bytes = BS.elem 0 (BS.take 8000 bytes)

conflictIndexEntries :: Conflict -> [IndexEntry]
conflictIndexEntries conflict =
    concat
        [ stageEntry 1 (conflictBase conflict)
        , stageEntry 2 (conflictOurs conflict)
        , stageEntry 3 (conflictTheirs conflict)
        ]
  where
    stageEntry stage maybeEntry =
        case maybeEntry of
            Nothing -> []
            Just entry ->
                [ IndexEntry
                    { indexEntryPath = BS8.pack (conflictPath conflict)
                    , indexEntryMode = treeEntryMode entry
                    , indexEntrySize = 0
                    , indexEntryObjectId = treeEntryObjectId entry
                    , indexEntryStage = stage
                    }
                ]

writeConflictFile :: Repository -> Store -> Conflict -> IO ()
writeConflictFile repository store conflict = do
    let path = repositoryRootDir repository </> conflictPath conflict
    createDirectoryIfMissing True (takeDirectory path)
    ours <- payloadOf (conflictOurs conflict)
    theirs <- payloadOf (conflictTheirs conflict)
    let content =
            if isBinary ours || isBinary theirs
                then ours
                else
                    BS8.pack "<<<<<<< HEAD\n"
                        <> ensureNewline ours
                        <> BS8.pack "=======\n"
                        <> ensureNewline theirs
                        <> BS8.pack ">>>>>>> MERGE_HEAD\n"
    BS.writeFile path content
  where
    payloadOf Nothing = pure BS.empty
    payloadOf (Just entry) = readBlob store entry
    isBinary bytes = BS.elem 0 (BS.take 8000 bytes)
    ensureNewline bytes
        | BS.null bytes = BS.empty
        | BS8.last bytes == '\n' = bytes
        | otherwise = bytes <> BS8.singleton '\n'

writeMergeState :: Repository -> ObjectId -> ObjectId -> IO ()
writeMergeState repository originalId targetId = do
    BS.writeFile
        (repositoryMergeHeadPath repository)
        (BS8.pack (objectIdToHex targetId ++ "\n"))
    BS.writeFile
        (repositoryMergeMsgPath repository)
        (BS8.pack ("Merge commit " ++ objectIdToHex targetId ++ "\n"))
    BS.writeFile
        (repositoryOrigHeadPath repository)
        (BS8.pack (objectIdToHex originalId ++ "\n"))
    ensureGitDir repository

ensureGitDir :: Repository -> IO ()
ensureGitDir repository = createDirectoryIfMissing True (repositoryGitDir repository)

mergeContinue :: Repository -> Store -> String -> IO ObjectId
mergeContinue repository store message = do
    mergeMessage <- readMergeMessage repository
    commitCurrentIndex
        repository
        store
        (if null message then mergeMessage else message)

readMergeMessage :: Repository -> IO String
readMergeMessage repository = do
    let path = repositoryMergeMsgPath repository
    exists <- doesFileExist path
    if exists then readFile path else pure "Merge commit\n"

mergeAbort :: Repository -> Store -> IO ()
mergeAbort repository store = do
    let path = repositoryOrigHeadPath repository
    exists <- doesFileExist path
    if not exists
        then ioError (userError "no merge in progress")
        else do
            contents <- readFile path
            case readObjectId repository contents of
                Nothing -> ioError (userError "ORIG_HEAD is malformed")
                Just originalId -> resetHard repository store originalId
            removeIfPresent (repositoryMergeHeadPath repository)
            removeIfPresent (repositoryMergeMsgPath repository)
            removeIfPresent (repositoryOrigHeadPath repository)

readObjectId :: Repository -> String -> Maybe ObjectId
readObjectId repository text =
    objectIdFromHexFor
        (repositoryObjectFormat repository)
        (trim text)

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
    exists <- doesFileExist path
    when exists (removeFile path)

treeOfCommit :: Store -> Repository -> ObjectId -> IO ObjectId
treeOfCommit store repository objectId = do
    gitObjectValue <- readObjectOrFail store objectId
    case gitObjectType gitObjectValue of
        CommitObject ->
            case commitFromObjectFor (repositoryObjectFormat repository) gitObjectValue of
                Left err -> ioError (userError err)
                Right currentCommit -> pure (commitTree currentCommit)
        TreeObject -> pure objectId
        _ -> ioError (userError "merge target is not a commit")

isAncestor :: Store -> ObjectId -> ObjectId -> IO Bool
isAncestor store ancestor descendant
    | ancestor == descendant = pure True
    | otherwise = go Set.empty [descendant]
  where
    objectFormat = repositoryObjectFormat (storeRepository store)
    go _ [] = pure False
    go seen (current : rest)
        | Set.member current seen = go seen rest
        | otherwise = do
            parents <- parentsOf current
            if ancestor `elem` parents then pure True else go (Set.insert current seen) (parents ++ rest)
    parentsOf objectId = do
        gitObjectValue <- readObjectOrFail store objectId
        case gitObjectType gitObjectValue of
            CommitObject ->
                case commitFromObjectFor objectFormat gitObjectValue of
                    Left _ -> pure []
                    Right currentCommit -> pure (commitParents currentCommit)
            _ -> pure []

requireMergeBase :: Store -> ObjectId -> ObjectId -> IO ObjectId
requireMergeBase store left right = do
    leftAncestors <- collectAncestors store left
    findBase leftAncestors [right] Set.empty
  where
    objectFormat = repositoryObjectFormat (storeRepository store)
    findBase _ [] _ = ioError (userError "no merge base")
    findBase ancestors (current : rest) seen
        | Set.member current seen = findBase ancestors rest seen
        | Set.member current ancestors = pure current
        | otherwise = do
            parents <- parentsOf current
            findBase ancestors (parents ++ rest) (Set.insert current seen)
    parentsOf objectId = do
        gitObjectValue <- readObjectOrFail store objectId
        case gitObjectType gitObjectValue of
            CommitObject ->
                case commitFromObjectFor objectFormat gitObjectValue of
                    Left _ -> pure []
                    Right currentCommit -> pure (commitParents currentCommit)
            _ -> pure []

collectAncestors :: Store -> ObjectId -> IO (Set.Set ObjectId)
collectAncestors store start = go Set.empty [start]
  where
    objectFormat = repositoryObjectFormat (storeRepository store)
    go seen [] = pure seen
    go seen (current : rest)
        | Set.member current seen = go seen rest
        | otherwise = do
            parents <- parentsOf current
            go (Set.insert current seen) (parents ++ rest)
    parentsOf objectId = do
        gitObjectValue <- readObjectOrFail store objectId
        case gitObjectType gitObjectValue of
            CommitObject ->
                case commitFromObjectFor objectFormat gitObjectValue of
                    Left _ -> pure []
                    Right currentCommit -> pure (commitParents currentCommit)
            _ -> pure []

trim :: String -> String
trim = reverse . dropWhile (`elem` " \r\n\t") . reverse . dropWhile (`elem` " \r\n\t")
