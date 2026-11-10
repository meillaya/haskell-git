module Git.Worktree
    ( checkoutTree
    , createBranch
    , currentTreeEntries
    , resetHard
    , statusRepository
    , worktreeEntries
    ) where

import Control.Monad (forM, forM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word32)
import System.Directory
    ( Permissions (..)
    , createDirectoryIfMissing
    , createFileLink
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , getPermissions
    , getSymbolicLinkTarget
    , listDirectory
    , pathIsSymbolicLink
    , removePathForcibly
    , setPermissions
    )
import System.FilePath ((</>), takeDirectory)
import Git.Hash (ObjectId)
import Git.Ignore (isIgnored, loadIgnoreRules)
import Git.Index
    ( IndexEntry (..)
    , indexEntryFromTree
    , parseIndexFor
    , writeIndexFor
    )
import Git.Object
    ( GitObjectType (..)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Commit (commitFromObjectFor, commitTree)
import Git.Object.Loose (objectIdForObjectFor)
import Git.Object.Tree
    ( TreeEntry
    , TreeEntryMode (..)
    , treeEntryMode
    , treeEntryName
    , treeEntryObjectId
    )
import Git.Refs
    ( HeadReference (..)
    , readHeadReference
    , readRefValue
    , updateHeadReference
    , writeHeadReference
    , writeRefValue
    )
import Git.Repo
    ( Repository
    , repositoryIndexPath
    , repositoryObjectFormat
    , repositoryRootDir
    )
import Git.Store (Store, flattenTree, readObjectOrFail)

-- | Flattened entries for the tree of HEAD, if HEAD points at a commit.
currentTreeEntries :: Store -> Repository -> IO [(FilePath, TreeEntry)]
currentTreeEntries store repository = do
    maybeTreeId <- readHeadTree store repository
    case maybeTreeId of
        Nothing -> pure []
        Just treeId -> flattenTree store treeId

readHeadTree :: Store -> Repository -> IO (Maybe ObjectId)
readHeadTree store repository = do
    headReference <- readHeadReference repository
    maybeObjectId <-
        case headReference of
            SymbolicHead refPath -> readRefValue repository refPath
            DetachedHead objectId -> pure (Just objectId)
    case maybeObjectId of
        Nothing -> pure Nothing
        Just objectId -> do
            gitObjectValue <- readObjectOrFail store objectId
            case gitObjectType gitObjectValue of
                CommitObject ->
                    case commitFromObjectFor (repositoryObjectFormat repository) gitObjectValue of
                        Left _ -> pure Nothing
                        Right currentCommit -> pure (Just (commitTree currentCommit))
                TreeObject -> pure (Just objectId)
                _ -> pure Nothing

readIndexEntries :: Repository -> IO [IndexEntry]
readIndexEntries repository = do
    let path = repositoryIndexPath repository
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            indexBytes <- BS.readFile path
            either
                (ioError . userError . (("index " ++ path ++ ": ") ++))
                pure
                (parseIndexFor (repositoryObjectFormat repository) indexBytes)

-- | Update the index and working tree to match a commit or tree. HEAD is left
-- to the caller.
checkoutTree :: Repository -> Store -> ObjectId -> IO ()
checkoutTree repository store targetId = do
    targetEntries <- treeEntriesForObject repository store targetId
    previousIndex <- readIndexEntries repository
    let previousPaths = Set.fromList (map indexEntryPath previousIndex)
        targetPaths = Set.fromList (map (BS8.pack . fst) targetEntries)
        removedPaths = Set.toList (previousPaths `Set.difference` targetPaths)
    forM_ removedPaths $ \pathBytes ->
        removeWorktreePath repository (BS8.unpack pathBytes)
    forM_ targetEntries $ \(relativePath, entry) ->
        writeWorktreeEntry store repository relativePath entry
    entries <- forM targetEntries $ \(relativePath, entry) -> do
        size <- entrySize store entry
        pure (indexEntryFromTree relativePath entry size)
    BS.writeFile
        (repositoryIndexPath repository)
        (writeIndexFor (repositoryObjectFormat repository) entries)

treeEntriesForObject :: Repository -> Store -> ObjectId -> IO [(FilePath, TreeEntry)]
treeEntriesForObject repository store objectId = do
    gitObjectValue <- readObjectOrFail store objectId
    case gitObjectType gitObjectValue of
        CommitObject ->
            case commitFromObjectFor (repositoryObjectFormat repository) gitObjectValue of
                Left err -> ioError (userError err)
                Right currentCommit -> flattenTree store (commitTree currentCommit)
        TreeObject -> flattenTree store objectId
        _ -> ioError (userError "checkout target is not a commit or tree")

entrySize :: Store -> TreeEntry -> IO Word32
entrySize store entry =
    case treeEntryMode entry of
        Gitlink -> pure 0
        Directory -> pure 0
        _ -> fromIntegral . BS.length <$> blobPayload store entry

blobPayload :: Store -> TreeEntry -> IO ByteString
blobPayload store entry = do
    gitObjectValue <- readObjectOrFail store (treeEntryObjectId entry)
    case gitObjectType gitObjectValue of
        BlobObject -> pure (gitObjectPayload gitObjectValue)
        _ -> ioError (userError ("tree entry does not point at a blob: " ++ BS8.unpack (treeEntryName entry)))

writeWorktreeEntry :: Store -> Repository -> FilePath -> TreeEntry -> IO ()
writeWorktreeEntry store repository relativePath entry = do
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
            removeWorktreePathIfPresent destinationPath
            createFileLink (BS8.unpack target) destinationPath
  where
    writeBlob destinationPath isExecutable = do
        payload <- blobPayload store entry
        createDirectoryIfMissing True (takeDirectory destinationPath)
        removeWorktreePathIfPresent destinationPath
        BS.writeFile destinationPath payload
        when isExecutable $ do
            permissions <- getPermissions destinationPath
            setPermissions destinationPath (permissions {executable = True})

removeWorktreePathIfPresent :: FilePath -> IO ()
removeWorktreePathIfPresent path = do
    exists <- doesPathExist path
    when exists (removePathForcibly path)

removeWorktreePath :: Repository -> FilePath -> IO ()
removeWorktreePath repository relativePath = do
    validateRelativePath relativePath
    removeWorktreePathIfPresent (repositoryRootDir repository </> relativePath)

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

-- | Create a branch at HEAD.
createBranch :: Repository -> String -> IO ()
createBranch repository branchName = do
    validateBranchName branchName
    headReference <- readHeadReference repository
    case headReference of
        SymbolicHead refPath -> do
            headValue <- readRefValue repository refPath
            case headValue of
                Nothing -> ioError (userError "cannot create a branch from an unborn HEAD")
                Just objectId ->
                    writeRefValue repository ("refs/heads/" ++ branchName) objectId
        DetachedHead objectId ->
            writeRefValue repository ("refs/heads/" ++ branchName) objectId

validateBranchName :: String -> IO ()
validateBranchName name =
    when (null name || any (`elem` " ~^:?*[\\") name) $
        ioError (userError ("invalid branch name: " ++ name))

-- | Hard reset: move the current branch (or detached HEAD) to the target and
-- update the index and worktree.
resetHard :: Repository -> Store -> ObjectId -> IO ()
resetHard repository store targetId = do
    updateHeadReference repository targetId
    checkoutTree repository store targetId

data StatusEntry = StatusEntry
    { statusPath :: FilePath
    , statusIndexCode :: Char
    , statusWorktreeCode :: Char
    }
    deriving (Eq, Show)

-- | A porcelain-like status. Ignore rules are not evaluated; untracked files
-- are reported as such.
statusRepository :: Repository -> Store -> IO [String]
statusRepository repository store = do
    headEntries <- currentTreeEntries store repository
    indexEntries <- readIndexEntries repository
    ignoreRules <- loadIgnoreRules repository
    allWorktree <- worktreeEntries repository
    let trackedPaths =
            Set.fromList
                ( map (BS8.unpack . indexEntryPath) indexEntries
                    ++ map fst headEntries
                )
        worktree =
            Map.filterWithKey
                (\path _ -> Set.member path trackedPaths || not (isIgnored ignoreRules path))
                allWorktree
        headMap = headMapOf headEntries
        indexMap =
            Map.fromList
                [ (BS8.unpack (indexEntryPath entry), (indexEntryMode entry, indexEntryObjectId entry))
                | entry <- indexEntries
                , indexEntryStage entry == 0
                ]
        paths =
            sort
                ( nub
                    ( Map.keys headMap
                        ++ Map.keys indexMap
                        ++ Map.keys worktree
                    )
                )
        entries = map (statusForPath headMap indexMap worktree) paths
    pure [renderStatus entry | entry <- entries, renderStatus entry /= ""]

headMapOf :: [(FilePath, TreeEntry)] -> Map.Map FilePath (TreeEntryMode, ObjectId)
headMapOf =
    Map.fromList . map (\(path, entry) -> (path, (treeEntryMode entry, treeEntryObjectId entry)))

statusForPath
    :: Map.Map FilePath (TreeEntryMode, ObjectId)
    -> Map.Map FilePath (TreeEntryMode, ObjectId)
    -> Map.Map FilePath (TreeEntryMode, ObjectId)
    -> FilePath
    -> StatusEntry
statusForPath headMap indexMap worktree path =
    StatusEntry
        { statusPath = path
        , statusIndexCode = indexCode
        , statusWorktreeCode = worktreeCode
        }
  where
    headValue = Map.lookup path headMap
    indexValue = Map.lookup path indexMap
    worktreeValue = Map.lookup path worktree
    indexCode =
        case (headValue, indexValue) of
            (Nothing, Nothing) -> ' '
            (Just _, Nothing) -> 'D'
            (Nothing, Just _) -> 'A'
            (Just headEntry, Just indexEntry)
                | headEntry == indexEntry -> ' '
                | otherwise -> 'M'
    worktreeCode =
        case (indexValue, worktreeValue) of
            (Nothing, Just _) -> '?'
            (Just _, Nothing) -> 'D'
            (Just indexEntry, Just worktreeEntry)
                | indexEntry == worktreeEntry -> ' '
                | otherwise -> 'M'
            (Nothing, Nothing) -> ' '

renderStatus :: StatusEntry -> String
renderStatus entry
    | statusIndexCode entry == ' ' && statusWorktreeCode entry == '?' =
        "?? " ++ statusPath entry
    | statusIndexCode entry == ' ' && statusWorktreeCode entry == ' ' = ""
    | otherwise = [statusIndexCode entry, statusWorktreeCode entry, ' '] ++ statusPath entry

worktreeEntries :: Repository -> IO (Map.Map FilePath (TreeEntryMode, ObjectId))
worktreeEntries repository = do
    files <- listFiles (repositoryRootDir repository) ""
    entries <- forM files $ \relativePath -> do
        let absolutePath = repositoryRootDir repository </> relativePath
        isLink <- pathIsSymbolicLink absolutePath
        if isLink
            then do
                target <- BS8.pack <$> getSymbolicLinkTarget absolutePath
                pure
                    ( relativePath
                    , ( Symlink
                      , objectIdForObjectFor (repositoryObjectFormat repository) (gitObject BlobObject target)
                      )
                    )
            else do
                payload <- BS.readFile absolutePath
                permissions <- getPermissions absolutePath
                let mode = if executable permissions then ExecutableFile else RegularFile
                pure
                    ( relativePath
                    , ( mode
                      , objectIdForObjectFor (repositoryObjectFormat repository) (gitObject BlobObject payload)
                      )
                    )
    pure (Map.fromList entries)

listFiles :: FilePath -> FilePath -> IO [FilePath]
listFiles root prefix = do
    names <- listDirectory (root </> prefix)
    fmap concat $
        forM names $ \name ->
            if name == ".git"
                then pure []
                else do
                    let relativePath = if null prefix then name else prefix </> name
                        absolutePath = root </> relativePath
                    isLink <- pathIsSymbolicLink absolutePath
                    isDirectory <- doesDirectoryExist absolutePath
                    if isDirectory && not isLink
                        then listFiles root relativePath
                        else pure [relativePath]

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn separator text =
    let (before, rest) = break (== separator) text
     in case rest of
            [] -> [before]
            (_ : after) -> before : splitOn separator after
