module Git.Merge
    ( MergeOptions (..)
    , MergeResult (..)
    , defaultMergeOptions
    , isAncestor
    , mergeAbort
    , mergeContinue
    , mergeRepository
    , mergeRepositoryWith
    , mergeTextContent
    , requireMergeBase
    , treeOfCommit
    ) where

import Control.Monad (foldM, forM, forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (maximumBy, nub, sort)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , removeFile
    )
import System.FilePath ((</>), takeDirectory)
import Git.Commit (commitCurrentIndex)
import Git.Diff (contentSimilarity, mergeTextContent)
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

data MergeOptions = MergeOptions
    { mergeNoFastForward :: Bool
    , mergeFFOnly :: Bool
    }
    deriving (Eq, Show)

defaultMergeOptions :: MergeOptions
defaultMergeOptions = MergeOptions False False

data MergeResult
    = MergeUpToDate
    | MergeFastForward ObjectId
    | MergeCreated ObjectId
    | MergeConflicted [FilePath]
    deriving (Eq, Show)

-- | Merge a commit into the current HEAD. Clean merges are committed
-- immediately; conflicts are left in the index for 'mergeContinue'.
mergeRepository :: Repository -> Store -> ObjectId -> IO MergeResult
mergeRepository = mergeRepositoryWith defaultMergeOptions

-- | Merge with explicit fast-forward policy. A no-fast-forward merge always
-- creates a merge commit; an ff-only merge refuses divergent histories.
mergeRepositoryWith :: MergeOptions -> Repository -> Store -> ObjectId -> IO MergeResult
mergeRepositoryWith mergeOptions repository store targetId = do
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
                then
                    if mergeNoFastForward mergeOptions
                        then do
                            writeMergeState repository headId targetId
                            checkoutTree repository store targetId
                            commitId <-
                                commitCurrentIndex
                                    repository
                                    store
                                    ("Merge commit " ++ objectIdToHex targetId ++ "\n")
                            pure (MergeCreated commitId)
                        else do
                            updateHeadReference repository targetId
                            checkoutTree repository store targetId
                            pure (MergeFastForward targetId)
                else do
                    targetIsAncestor <- isAncestor store targetId headId
                    if targetIsAncestor
                        then pure MergeUpToDate
                        else do
                            when (mergeFFOnly mergeOptions) $
                                ioError (userError "merge is not a fast-forward")
                            when (mergeNoFastForward mergeOptions && mergeFFOnly mergeOptions) $
                                ioError (userError "merge cannot be both --no-ff and --ff-only")
                            baseId <- requireMergeBase store headId targetId
                            headTree <- treeOfCommit store repository headId
                            targetTree <- treeOfCommit store repository targetId
                            baseTree <- treeOfCommit store repository baseId
                            baseEntries <- flattenTree store baseTree
                            ourEntries <- flattenTree store headTree
                            theirEntries <- flattenTree store targetTree
                            let baseMap = entryMap baseEntries
                                ourMap = entryMap ourEntries
                                theirMap = entryMap theirEntries
                            (ourRenames, ourNormalized) <-
                                detectRenames store baseMap ourMap
                            (theirRenames, theirNormalized) <-
                                detectRenames store baseMap theirMap
                            (rawResolved, rawConflicts) <-
                                mergeEntryMaps
                                    repository
                                    store
                                    baseMap
                                    ourNormalized
                                    theirNormalized
                            let (resolved, renameConflicts) =
                                    repathEntries
                                        baseMap
                                        ourRenames
                                        theirRenames
                                        rawResolved
                                conflicts =
                                    rawConflicts
                                        ++ renameConflicts
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

-- | Detect renames from base to one side. Exact-content matches are preferred;
-- otherwise text similarity above 50% is used, which covers renames whose
-- contents were also edited.
detectRenames
    :: Store
    -> Map.Map FilePath TreeEntry
    -> Map.Map FilePath TreeEntry
    -> IO (Map.Map FilePath (FilePath, TreeEntry), Map.Map FilePath TreeEntry)
detectRenames store baseMap sideMap = do
    let deleted =
            [ (basePath, entry)
            | (basePath, entry) <- Map.toList baseMap
            , Map.notMember basePath sideMap
            , renameable entry
            ]
        added =
            [ (sidePath, entry)
            | (sidePath, entry) <- Map.toList sideMap
            , Map.notMember sidePath baseMap
            , renameable entry
            ]
        exactCandidates =
            [ (basePath, sidePath, sideEntry)
            | (basePath, baseEntry) <- deleted
            , (sidePath, sideEntry) <- added
            , treeEntryObjectId sideEntry == treeEntryObjectId baseEntry
            ]
        exactTargetCounts =
            Map.fromListWith
                (+)
                [(sidePath, 1 :: Int) | (_, sidePath, _) <- exactCandidates]
        exactMatches =
            [ (basePath, (sidePath, sideEntry))
            | (basePath, sidePath, sideEntry) <- exactCandidates
            , Map.findWithDefault 0 sidePath exactTargetCounts == 1
            ]
        usedBasePaths = Set.fromList [basePath | (basePath, _) <- exactMatches]
        usedSidePaths = Set.fromList [sidePath | (_, (sidePath, _)) <- exactMatches]
        remainingDeleted =
            [ entry | entry@(basePath, _) <- deleted, not (Set.member basePath usedBasePaths) ]
        remainingAdded =
            [ entry | entry@(sidePath, _) <- added, not (Set.member sidePath usedSidePaths) ]
    scored <-
        forM remainingDeleted $ \(basePath, baseEntry) -> do
            basePayload <- renamePayload store baseEntry
            similarities <-
                forM remainingAdded $ \(sidePath, sideEntry) -> do
                    sidePayload <- renamePayload store sideEntry
                    pure
                        ( contentSimilarity basePayload sidePayload
                        , sidePath
                        , sideEntry
                        )
            let best =
                    maximumBy
                        (comparing (\(score, _, _) -> score))
                        ((0, "", baseEntry) : similarities)
            pure (basePath, best)
    let targetBest =
            Map.fromListWith
                maxScored
                [(sidePath, (score, basePath, sideEntry)) | (basePath, (score, sidePath, sideEntry)) <- scored]
        similarityMatches =
            [ (basePath, (sidePath, sideEntry))
            | (basePath, (score, sidePath, sideEntry)) <- scored
            , score >= 0.5
            , Map.lookup sidePath targetBest == Just (score, basePath, sideEntry)
            ]
        matches = exactMatches ++ similarityMatches
        renameMap = Map.fromList matches
        renamedTargets = Set.fromList [sidePath | (_, (sidePath, _)) <- matches]
        withoutTargets =
            foldr Map.delete sideMap (Set.toList renamedTargets)
        normalized =
            Map.union
                (Map.fromList [(basePath, entry) | (basePath, (_, entry)) <- matches])
                withoutTargets
    pure (renameMap, normalized)
  where
    maxScored left right =
        if fst3 left >= fst3 right then left else right
    fst3 (first, _, _) = first

renamePayload :: Store -> TreeEntry -> IO BS.ByteString
renamePayload store entry = do
    objectValue <- readObjectOrFail store (treeEntryObjectId entry)
    case gitObjectType objectValue of
        BlobObject -> pure (gitObjectPayload objectValue)
        _ -> pure BS.empty

renameable :: TreeEntry -> Bool
renameable entry =
    treeEntryMode entry `elem` [RegularFile, ExecutableFile, Symlink]

-- | Rewrite resolved paths according to the rename maps. When both sides
-- rename the same path to different destinations, or a rename collides with
-- another resolved path, the path becomes a conflict.
repathEntries
    :: Map.Map FilePath TreeEntry
    -> Map.Map FilePath (FilePath, TreeEntry)
    -> Map.Map FilePath (FilePath, TreeEntry)
    -> [IndexEntry]
    -> ([IndexEntry], [Conflict])
repathEntries baseMap ourRenames theirRenames entries =
    let (resolvedMap, conflicts) = foldl step (Map.empty, []) entries
     in (Map.elems resolvedMap, conflicts)
  where
    step (resolvedMap, conflicts) entry =
        case choosePath (BS8.unpack (indexEntryPath entry)) of
            Nothing ->
                let path = BS8.unpack (indexEntryPath entry)
                 in ( resolvedMap
                    , conflicts
                        ++ [ Conflict
                                path
                                (Map.lookup path baseMap)
                                (snd <$> Map.lookup path ourRenames)
                                (snd <$> Map.lookup path theirRenames)
                           ]
                    )
            Just newPath ->
                let renamedEntry = entry {indexEntryPath = BS8.pack newPath}
                 in case Map.lookup newPath resolvedMap of
                    Nothing -> (Map.insert newPath renamedEntry resolvedMap, conflicts)
                    Just existing
                        | existing == entry -> (resolvedMap, conflicts)
                        | otherwise ->
                            ( resolvedMap
                            , conflicts
                                ++ [ Conflict
                                        newPath
                                        (Map.lookup (BS8.unpack (indexEntryPath entry)) baseMap)
                                        Nothing
                                        Nothing
                                   ]
                            )

    choosePath path =
        case (Map.lookup path ourRenames, Map.lookup path theirRenames) of
            (Just (ourPath, ourEntry), Just (theirPath, theirEntry))
                | ourPath == theirPath -> Just ourPath
                | otherwise ->
                    Nothing
            (Just (ourPath, _), Nothing) -> Just ourPath
            (Nothing, Just (theirPath, _)) -> Just theirPath
            (Nothing, Nothing) -> Just path

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
