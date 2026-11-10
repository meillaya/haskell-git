module Git.Submodule
    ( Submodule (..)
    , initialiseSubmodules
    , readSubmodules
    , resolveSubmoduleUrl
    , submoduleSync
    , submoduleStatus
    , updateSubmodules
    ) where

import Control.Monad (forM, forM_, when)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removeDirectory
    , renameDirectory
    )
import System.FilePath
    ( (</>)
    , makeRelative
    , takeDirectory
    )
import Git.Clone (cloneRepository)
import Git.Config
    ( ConfigEntry (..)
    , appendConfigValue
    , configEntries
    , configGet
    , loadRepositoryConfig
    , parseConfig
    )
import Git.Hash (ObjectId, objectIdToHex)
import Git.Object (GitObjectType (..), gitObjectType)
import Git.Object.Commit (commitFromObjectFor, commitTree)
import Git.Object.Tree
    ( TreeEntry
    , TreeEntryMode (Gitlink)
    , treeEntryMode
    , treeEntryObjectId
    )
import Git.Refs
    ( HeadReference (..)
    , readHeadReference
    , readRefValue
    , resolveHeadReference
    , writeHeadReference
    )
import Git.Repo
    ( Repository (..)
    , findRepository
    , repositoryGitDir
    , repositoryObjectFormat
    , repositoryRootDir
    )
import Git.Store (Store, flattenTree, loadStore, readObjectOrFail)
import Git.Worktree (checkoutTree)

data Submodule = Submodule
    { submoduleName :: String
    , submodulePath :: FilePath
    , submoduleUrl :: String
    }
    deriving (Eq, Show)

readSubmodules :: Repository -> IO [Submodule]
readSubmodules repository = do
    let path = repositoryRootDir repository </> ".gitmodules"
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            config <- parseConfig <$> readFile path
            let names =
                    [ name
                    | entry <- configEntries config
                    , configEntrySection entry == "submodule"
                    , Just name <- [configEntrySubsection entry]
                    , configEntryKey entry == "path"
                    ]
            pure
                [ Submodule
                    { submoduleName = name
                    , submodulePath = maybe "" id (configGet config ("submodule." ++ name ++ ".path"))
                    , submoduleUrl = maybe "" id (configGet config ("submodule." ++ name ++ ".url"))
                    }
                | name <- names
                ]

resolveSubmoduleUrl :: String -> String -> Either String String
resolveSubmoduleUrl base submodule
    | hasScheme submodule = Right submodule
    | "./" `isPrefixOf` submodule || "../" `isPrefixOf` submodule =
        Right (resolveRelative base submodule)
    | otherwise = Right submodule

hasScheme :: String -> Bool
hasScheme value =
    any (`isPrefixOf` value) ["http://", "https://", "ssh://", "git://"]

resolveRelative :: String -> String -> String
resolveRelative base relative =
    let (prefix, pathPart) = splitScheme base
        levels = countParentPrefixes relative
        stripped = dropParentPrefixes relative
        baseDirectory =
            dropComponents
                (max 0 (levels - 1))
                (takeDirectoryUrl pathPart)
     in prefix ++ joinPath baseDirectory stripped
  where
    countParentPrefixes text
        | "../" `isPrefixOf` text = 1 + countParentPrefixes (drop 3 text)
        | otherwise = 0
    dropParentPrefixes text
        | "../" `isPrefixOf` text = dropParentPrefixes (drop 3 text)
        | "./" `isPrefixOf` text = drop 2 text
        | otherwise = text
    dropComponents count text
        | count <= 0 = text
        | otherwise = dropComponents (count - 1) (takeDirectoryUrl text)
    takeDirectoryUrl text =
        case lastSlash text of
            Nothing -> ""
            Just index -> take index text
    lastSlash text =
        case [index | (index, character) <- zip [0 ..] text, character == '/'] of
            [] -> Nothing
            indices -> Just (last indices)
    joinPath directory relativePath
        | null directory = relativePath
        | null relativePath = directory
        | otherwise = directory ++ "/" ++ relativePath
    splitScheme text
        | "http://" `isPrefixOf` text = ("http://", drop 7 text)
        | "https://" `isPrefixOf` text = ("https://", drop 8 text)
        | "ssh://" `isPrefixOf` text = ("ssh://", drop 6 text)
        | "git://" `isPrefixOf` text = ("git://", drop 6 text)
        | otherwise =
            case break (== ':') text of
                (before, ':' : after) -> (before ++ ":", after)
                _ -> ("", text)

initialiseSubmodules :: Repository -> Store -> Bool -> IO ()
initialiseSubmodules repository store recursive = do
    submodules <- readSubmodules repository
    headEntries <- flattenHead store repository
    let entryMap = Map.fromList headEntries
    forM_ submodules $ \submodule ->
        case Map.lookup (submodulePath submodule) entryMap of
            Just entry | treeEntryMode entry == Gitlink -> do
                url <- resolveUrlFor repository submodule
                initialiseOne repository recursive submodule url (treeEntryObjectId entry)
            _ -> pure ()

-- | Refresh submodule URLs in .git/config from .gitmodules.
submoduleSync :: Repository -> IO ()
submoduleSync repository = do
    submodules <- readSubmodules repository
    forM_ submodules $ \submodule ->
        appendConfigValue
            repository
            "submodule"
            (Just (submoduleName submodule))
            "url"
            (submoduleUrl submodule)

updateSubmodules :: Repository -> Store -> Bool -> IO ()
updateSubmodules = initialiseSubmodules

submoduleStatus :: Repository -> Store -> IO [String]
submoduleStatus repository store = do
    submodules <- readSubmodules repository
    headEntries <- flattenHead store repository
    let entryMap = Map.fromList headEntries
    fmap concat $
        forM submodules $ \submodule ->
            case Map.lookup (submodulePath submodule) entryMap of
                Just entry | treeEntryMode entry == Gitlink -> do
                    let expected = treeEntryObjectId entry
                        worktreePath = repositoryRootDir repository </> submodulePath submodule
                        gitPath = worktreePath </> ".git"
                    gitDirectory <- doesDirectoryExist gitPath
                    gitFile <- doesFileExist gitPath
                    if not gitDirectory && not gitFile
                        then pure ["-" ++ objectIdToHex expected ++ " " ++ submodulePath submodule]
                        else do
                            maybeSubRepository <- findRepository worktreePath
                            case maybeSubRepository of
                                Nothing ->
                                    pure ["-" ++ objectIdToHex expected ++ " " ++ submodulePath submodule]
                                Just subRepository -> do
                                    headValue <- resolveHeadReference subRepository
                                    case headValue of
                                        Just actual
                                            | actual == expected ->
                                                pure [" " ++ objectIdToHex actual ++ " " ++ submodulePath submodule]
                                            | otherwise ->
                                                pure ["+" ++ objectIdToHex actual ++ " " ++ submodulePath submodule]
                                        Nothing ->
                                            pure ["-" ++ objectIdToHex expected ++ " " ++ submodulePath submodule]
                _ -> pure []

initialiseOne :: Repository -> Bool -> Submodule -> String -> ObjectId -> IO ()
initialiseOne superRepository recursive submodule url commitId = do
    let worktreePath = repositoryRootDir superRepository </> submodulePath submodule
        gitDir =
            repositoryGitDir superRepository
                </> "modules"
                </> submodulePath submodule
    gitDirExists <- doesDirectoryExist gitDir
    when (not gitDirExists) $ do
        worktreeExists <- doesDirectoryExist worktreePath
        when worktreeExists $ removeEmptyDirectory worktreePath
        _ <- cloneRepository url worktreePath
        createDirectoryIfMissing True (takeDirectory gitDir)
        renameDirectory (worktreePath </> ".git") gitDir
        writeFile
            (worktreePath </> ".git")
            ("gitdir: " ++ makeRelative worktreePath gitDir ++ "\n")
    maybeSubRepository <- findRepository worktreePath
    case maybeSubRepository of
        Nothing -> ioError (userError ("submodule is not a repository: " ++ submodulePath submodule))
        Just subRepository -> do
            subStore <- loadStore subRepository
            writeHeadReference subRepository (DetachedHead commitId)
            checkoutTree subRepository subStore commitId
            when recursive (initialiseSubmodules subRepository subStore True)

resolveUrlFor :: Repository -> Submodule -> IO String
resolveUrlFor repository submodule = do
    config <- loadRepositoryConfig repository
    let baseUrl = maybe "" id (configGet config "remote.origin.url")
    case resolveSubmoduleUrl baseUrl (submoduleUrl submodule) of
        Right url -> pure url
        Left err -> ioError (userError err)

removeEmptyDirectory :: FilePath -> IO ()
removeEmptyDirectory path = do
    exists <- doesDirectoryExist path
    when exists $ do
        entries <- listDirectory path
        when (null entries) (removeDirectory path)

flattenHead :: Store -> Repository -> IO [(FilePath, TreeEntry)]
flattenHead store repository = do
    headReference <- readHeadReference repository
    maybeObjectId <-
        case headReference of
            SymbolicHead refPath -> readRefValue repository refPath
            DetachedHead objectId -> pure (Just objectId)
    case maybeObjectId of
        Nothing -> pure []
        Just objectId -> do
            gitObjectValue <- readObjectOrFail store objectId
            case gitObjectType gitObjectValue of
                CommitObject ->
                    case commitFromObjectFor (repositoryObjectFormat repository) gitObjectValue of
                        Left _ -> pure []
                        Right currentCommit -> flattenTree store (commitTree currentCommit)
                TreeObject -> flattenTree store objectId
                _ -> pure []
