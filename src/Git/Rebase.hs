module Git.Rebase
    ( RebaseResult (..)
    , rebaseAbort
    , rebaseContinue
    , rebaseRepository
    , rebaseSkip
    ) where

import Control.Monad (foldM, forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , removePathForcibly
    )
import System.FilePath ((</>), takeDirectory)
import Git.Commit
    ( commitIndexWithParentsAuthor
    , resolveIdentity
    )
import Git.Hash (ObjectId, objectIdFromHexFor, objectIdToHex)
import Git.Index
    ( IndexEntry (..)
    , buildTreeFromIndex
    , writeIndexFor
    )
import Git.Merge
    ( isAncestor
    , mergeTextContent
    , requireMergeBase
    , treeOfCommit
    )
import Git.Object
    ( GitObjectType (..)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Commit
    ( commitAuthor
    , commitFromObjectFor
    , commitMessage
    , commitParents
    )
import Git.Object.Loose (writeLooseObject)
import Git.Object.Tree
    ( TreeEntry (..)
    , TreeEntryMode (..)
    , treeEntryMode
    , treeEntryObjectId
    , treeToObject
    )
import Git.Refs
    ( HeadReference (..)
    , readHeadReference
    , resolveHeadReference
    , writeHeadReference
    , writeRefValue
    )
import Git.Repo
    ( Repository
    , repositoryIndexPath
    , repositoryObjectFormat
    , repositoryRebaseDir
    , repositoryRootDir
    )
import Git.Store
    ( Store
    , flattenTree
    , readObjectOrFail
    , storeRepository
    )
import Git.Worktree (checkoutTree, resetHard)

data RebaseResult
    = RebaseUpToDate
    | RebaseComplete ObjectId
    | RebaseConflicted [FilePath]
    deriving (Eq, Show)

data RebaseState = RebaseState
    { rebaseBranch :: FilePath
    , rebaseOrigHead :: ObjectId
    , rebaseOnto :: ObjectId
    , rebaseCurrent :: Maybe ObjectId
    , rebaseRemaining :: [ObjectId]
    }

rebaseRepository :: Repository -> Store -> ObjectId -> IO RebaseResult
rebaseRepository repository store onto = do
    headReference <- readHeadReference repository
    branchPath <-
        case headReference of
            SymbolicHead refPath -> pure refPath
            DetachedHead _ -> ioError (userError "rebase requires a branch checkout")
    headValue <- resolveHeadReference repository
    headId <-
        case headValue of
            Nothing -> ioError (userError "cannot rebase an unborn branch")
            Just value -> pure value
    ontoIsAncestor <- isAncestor store onto headId
    if headId == onto || ontoIsAncestor
        then pure RebaseUpToDate
        else do
            headIsAncestor <- isAncestor store headId onto
            if headIsAncestor
                then do
                    writeRefValue repository branchPath onto
                    writeHeadReference repository (SymbolicHead branchPath)
                    checkoutTree repository store onto
                    pure (RebaseComplete onto)
                else do
                    base <- requireMergeBase store headId onto
                    commits <- commitsAfter store headId base
                    case commits of
                        [] -> pure RebaseUpToDate
                        firstCommit : rest -> do
                            writeHeadReference repository (DetachedHead onto)
                            checkoutTree repository store onto
                            let state =
                                    RebaseState
                                        { rebaseBranch = branchPath
                                        , rebaseOrigHead = headId
                                        , rebaseOnto = onto
                                        , rebaseCurrent = Just firstCommit
                                        , rebaseRemaining = rest
                                        }
                            saveState repository state
                            replay repository store state

replay :: Repository -> Store -> RebaseState -> IO RebaseResult
replay repository store state =
    case rebaseCurrent state of
        Nothing -> finishRebase repository store state
        Just originalCommit -> do
            currentHead <- requireHead repository
            outcome <- applyCommit repository store originalCommit currentHead
            case outcome of
                Left conflicts -> do
                    saveState repository state
                    pure (RebaseConflicted (map conflictPath conflicts))
                Right _ -> do
                    case rebaseRemaining state of
                        [] -> finishRebase repository store state {rebaseCurrent = Nothing}
                        nextCommit : rest ->
                            replay
                                repository
                                store
                                state
                                    { rebaseCurrent = Just nextCommit
                                    , rebaseRemaining = rest
                                    }

finishRebase :: Repository -> Store -> RebaseState -> IO RebaseResult
finishRebase repository _store state = do
    currentHead <- requireHead repository
    when (not (null (rebaseBranch state))) $
        writeRefValue repository (rebaseBranch state) currentHead
    writeHeadReference repository (SymbolicHead (rebaseBranch state))
    removeRebaseDir repository
    pure (RebaseComplete currentHead)

applyCommit :: Repository -> Store -> ObjectId -> ObjectId -> IO (Either [Conflict] ObjectId)
applyCommit repository store originalCommit currentHead = do
    originalObject <- readObjectOrFail store originalCommit
    original <-
        either
            (ioError . userError)
            pure
            (commitFromObjectFor (repositoryObjectFormat repository) originalObject)
    baseTreeId <-
        case commitParents original of
            parent : _ -> treeOfCommit store repository parent
            [] -> writeLooseObject repository (treeToObject [])
    currentTreeId <- treeOfCommit store repository currentHead
    originalTreeId <- treeOfCommit store repository originalCommit
    baseEntries <- Map.fromList <$> flattenTree store baseTreeId
    ourEntries <- Map.fromList <$> flattenTree store currentTreeId
    theirEntries <- Map.fromList <$> flattenTree store originalTreeId
    (resolved, conflicts) <- mergeMaps repository store baseEntries ourEntries theirEntries
    if null conflicts
        then do
            treeId <- treeFromEntries repository resolved
            checkoutTree repository store treeId
            committer <- resolveIdentity repository "GIT_COMMITTER" "COMMITTER"
            newHead <-
                commitIndexWithParentsAuthor
                    repository
                    [currentHead]
                    (commitAuthor original)
                    committer
                    (BS8.unpack (commitMessage original))
            pure (Right newHead)
        else do
            treeId <- treeFromEntries repository resolved
            checkoutTree repository store treeId
            forM_ conflicts (writeConflict repository store)
            BS.writeFile
                (repositoryIndexPath repository)
                ( writeIndexFor
                    (repositoryObjectFormat repository)
                    (resolved ++ concatMap conflictIndexes conflicts)
                )
            pure (Left conflicts)

treeFromEntries :: Repository -> [IndexEntry] -> IO ObjectId
treeFromEntries repository entries
    | null entries = writeLooseObject repository (treeToObject [])
    | otherwise = buildTreeFromIndex repository entries

mergeMaps
    :: Repository
    -> Store
    -> Map.Map FilePath TreeEntry
    -> Map.Map FilePath TreeEntry
    -> Map.Map FilePath TreeEntry
    -> IO ([IndexEntry], [Conflict])
mergeMaps repository store baseMap ourMap theirMap =
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
                , isRegular ourEntry
                , isRegular theirEntry
                , treeEntryMode ourEntry == treeEntryMode theirEntry -> do
                    ourPayload <- blobOf store ourEntry
                    theirPayload <- blobOf store theirEntry
                    basePayload <-
                        case base of
                            Just baseEntry | isRegular baseEntry -> blobOf store baseEntry
                            _ -> pure BS.empty
                    case mergeTextContent basePayload ourPayload theirPayload of
                        Right mergedPayload -> do
                            mergedId <- writeLooseObject repository (gitObject BlobObject mergedPayload)
                            pure
                                ( addEntry path (Just (ourEntry {treeEntryObjectId = mergedId})) resolved
                                , conflicts
                                )
                        Left _ -> pure (resolved, conflicts ++ [Conflict path base ours theirs])
                | otherwise -> pure (resolved, conflicts ++ [Conflict path base ours theirs])

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

data Conflict = Conflict
    { conflictPath :: FilePath
    , conflictBase :: Maybe TreeEntry
    , conflictOurs :: Maybe TreeEntry
    , conflictTheirs :: Maybe TreeEntry
    }

conflictIndexes :: Conflict -> [IndexEntry]
conflictIndexes conflict =
    concat
        [ stage 1 (conflictBase conflict)
        , stage 2 (conflictOurs conflict)
        , stage 3 (conflictTheirs conflict)
        ]
  where
    stage stageNumber maybeEntry =
        case maybeEntry of
            Nothing -> []
            Just entry ->
                [ IndexEntry
                    { indexEntryPath = BS8.pack (conflictPath conflict)
                    , indexEntryMode = treeEntryMode entry
                    , indexEntrySize = 0
                    , indexEntryObjectId = treeEntryObjectId entry
                    , indexEntryStage = stageNumber
                    }
                ]

writeConflict :: Repository -> Store -> Conflict -> IO ()
writeConflict repository store conflict = do
    let path = repositoryRootDir repository </> conflictPath conflict
    createDirectoryIfMissing True (takeDirectory path)
    ours <- payloadOf (conflictOurs conflict)
    theirs <- payloadOf (conflictTheirs conflict)
    let content
            | isBinary ours || isBinary theirs = ours
            | otherwise =
                BS8.pack "<<<<<<< HEAD\n"
                    <> ensureNewline ours
                    <> BS8.pack "=======\n"
                    <> ensureNewline theirs
                    <> BS8.pack ">>>>>>> REBASE_HEAD\n"
    BS.writeFile path content
  where
    payloadOf Nothing = pure BS.empty
    payloadOf (Just entry) = blobOf store entry
    isBinary bytes = BS.elem 0 (BS.take 8000 bytes)
    ensureNewline bytes
        | BS.null bytes = BS.empty
        | BS8.last bytes == '\n' = bytes
        | otherwise = bytes <> BS8.singleton '\n'

rebaseContinue :: Repository -> Store -> IO RebaseResult
rebaseContinue repository store = do
    state <- loadState repository
    case rebaseCurrent state of
        Nothing -> finishRebase repository store state
        Just originalCommit -> do
            originalObject <- readObjectOrFail store originalCommit
            original <-
                either
                    (ioError . userError)
                    pure
                    (commitFromObjectFor (repositoryObjectFormat repository) originalObject)
            committer <- resolveIdentity repository "GIT_COMMITTER" "COMMITTER"
            currentHead <- requireHead repository
            _ <-
                commitIndexWithParentsAuthor
                    repository
                    [currentHead]
                    (commitAuthor original)
                    committer
                    (BS8.unpack (commitMessage original))
            case rebaseRemaining state of
                [] -> finishRebase repository store state {rebaseCurrent = Nothing}
                nextCommit : rest ->
                    replay
                        repository
                        store
                        state
                            { rebaseCurrent = Just nextCommit
                            , rebaseRemaining = rest
                            }

rebaseSkip :: Repository -> Store -> IO RebaseResult
rebaseSkip repository store = do
    state <- loadState repository
    case rebaseRemaining state of
        [] -> finishRebase repository store state {rebaseCurrent = Nothing}
        nextCommit : rest ->
            replay
                repository
                store
                state
                    { rebaseCurrent = Just nextCommit
                    , rebaseRemaining = rest
                    }

rebaseAbort :: Repository -> Store -> IO ()
rebaseAbort repository store = do
    state <- loadState repository
    resetHard repository store (rebaseOrigHead state)
    when (not (null (rebaseBranch state))) $
        writeHeadReference repository (SymbolicHead (rebaseBranch state))
    removeRebaseDir repository

saveState :: Repository -> RebaseState -> IO ()
saveState repository state = do
    let directory = repositoryRebaseDir repository
    createDirectoryIfMissing True directory
    writeFile (directory </> "head-name") (rebaseBranch state ++ "\n")
    writeFile (directory </> "orig-head") (objectIdToHex (rebaseOrigHead state) ++ "\n")
    writeFile (directory </> "onto") (objectIdToHex (rebaseOnto state) ++ "\n")
    writeFile
        (directory </> "current")
        (maybe "" ((++ "\n") . objectIdToHex) (rebaseCurrent state))
    writeFile
        (directory </> "remaining")
        (unlines (map objectIdToHex (rebaseRemaining state)))

loadState :: Repository -> IO RebaseState
loadState repository = do
    let directory = repositoryRebaseDir repository
    branch <- readFile (directory </> "head-name")
    originalHex <- readFile (directory </> "orig-head")
    ontoHex <- readFile (directory </> "onto")
    currentHex <- readFile (directory </> "current")
    remainingText <- readFile (directory </> "remaining")
    let objectFormat = repositoryObjectFormat repository
    original <- parseId objectFormat originalHex
    onto <- parseId objectFormat ontoHex
    remaining <- traverse (parseId objectFormat) (lines remainingText)
    let current
            | null (trim currentHex) = Nothing
            | otherwise = Just (mustParse objectFormat currentHex)
    pure
        RebaseState
            { rebaseBranch = trim branch
            , rebaseOrigHead = original
            , rebaseOnto = onto
            , rebaseCurrent = current
            , rebaseRemaining = remaining
            }
  where
    parseId objectFormat text =
        case objectIdFromHexFor objectFormat (trim text) of
            Just value -> pure value
            Nothing -> ioError (userError "malformed rebase state")
    mustParse objectFormat text =
        case objectIdFromHexFor objectFormat (trim text) of
            Just value -> value
            Nothing -> error "malformed rebase state"

removeRebaseDir :: Repository -> IO ()
removeRebaseDir repository = do
    let directory = repositoryRebaseDir repository
    exists <- doesDirectoryExist directory
    when exists (removePathForcibly directory)

commitsAfter :: Store -> ObjectId -> ObjectId -> IO [ObjectId]
commitsAfter store start stop = reverse <$> go [] start
  where
    go accumulated current
        | current == stop = pure accumulated
        | otherwise = do
            parents <- parentsOf current
            case parents of
                parent : _ -> go (current : accumulated) parent
                [] -> pure (current : accumulated)
    parentsOf objectId = do
        gitObjectValue <- readObjectOrFail store objectId
        case gitObjectType gitObjectValue of
            CommitObject ->
                case commitFromObjectFor (repositoryObjectFormat (storeRepository store)) gitObjectValue of
                    Left _ -> pure []
                    Right currentCommit -> pure (commitParents currentCommit)
            _ -> pure []

requireHead :: Repository -> IO ObjectId
requireHead repository = do
    headValue <- resolveHeadReference repository
    case headValue of
        Nothing -> ioError (userError "HEAD is unborn")
        Just value -> pure value

blobOf :: Store -> TreeEntry -> IO BS.ByteString
blobOf store entry = do
    gitObjectValue <- readObjectOrFail store (treeEntryObjectId entry)
    case gitObjectType gitObjectValue of
        BlobObject -> pure (gitObjectPayload gitObjectValue)
        _ -> ioError (userError "rebase expected a blob")

isRegular :: TreeEntry -> Bool
isRegular entry = treeEntryMode entry `elem` [RegularFile, ExecutableFile]

trim :: String -> String
trim = reverse . dropWhile (`elem` " \r\n\t") . reverse . dropWhile (`elem` " \r\n\t")
