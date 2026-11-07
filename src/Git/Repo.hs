module Git.Repo
    ( Repository (..)
    , defaultGitDirectories
    , findRepository
    , initRepository
    , initRepositoryWithBranch
    , readConfigValue
    , requireRepository
    , repositoryAt
    , repositoryConfigPath
    , repositoryGitDir
    , repositoryIndexPath
    , repositoryObjectsDir
    , repositoryPackedRefsPath
    , repositoryRefsDir
    , repositoryRootDir
    , validateReferenceName
    ) where

import Control.Monad (filterM, forM_)
import Data.Char (isControl, isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , makeAbsolute
    )
import System.FilePath ((</>), takeDirectory)

data Repository = Repository
    { repositoryRootDir :: FilePath
    , repositoryGitDir :: FilePath
    , repositoryIsBare :: Bool
    }
    deriving (Eq, Show)

defaultGitDirectories :: [FilePath]
defaultGitDirectories =
    [ ".git"
    , ".git/objects"
    , ".git/objects/info"
    , ".git/objects/pack"
    , ".git/refs"
    , ".git/refs/heads"
    , ".git/refs/tags"
    ]

defaultBranchName :: String
defaultBranchName = "main"

repositoryObjectsDir :: Repository -> FilePath
repositoryObjectsDir repository = repositoryGitDir repository </> "objects"

repositoryRefsDir :: Repository -> FilePath
repositoryRefsDir repository = repositoryGitDir repository </> "refs"

repositoryConfigPath :: Repository -> FilePath
repositoryConfigPath repository = repositoryGitDir repository </> "config"

repositoryPackedRefsPath :: Repository -> FilePath
repositoryPackedRefsPath repository = repositoryGitDir repository </> "packed-refs"

repositoryIndexPath :: Repository -> FilePath
repositoryIndexPath repository = repositoryGitDir repository </> "index"

repositoryAt :: FilePath -> Repository
repositoryAt targetPath = Repository targetPath (targetPath </> ".git") False

repositoryAtBare :: FilePath -> Repository
repositoryAtBare targetPath = Repository targetPath targetPath True

-- | Find the repository that contains a path, walking up towards the root. A
-- working tree is found by its @.git@ directory; a bare repository by the
-- @HEAD@, @objects@, and @refs@ entries at its root.
findRepository :: FilePath -> IO (Maybe Repository)
findRepository startPath = do
    absolutePath <- makeAbsolute startPath
    walk absolutePath
  where
    walk path = do
        found <- repositoryAtPath path
        case found of
            Just repository -> pure (Just repository)
            Nothing ->
                let parent = takeDirectory path
                 in if parent == path then pure Nothing else walk parent

    repositoryAtPath path = do
        gitDirectoryExists <- doesDirectoryExist (path </> ".git")
        if gitDirectoryExists
            then pure (Just (Repository path (path </> ".git") False))
            else do
                bare <- looksLikeBareRepository path
                pure (if bare then Just (repositoryAtBare path) else Nothing)

    looksLikeBareRepository path = do
        headExists <- doesFileExist (path </> "HEAD")
        objectsExist <- doesDirectoryExist (path </> "objects")
        refsExist <- doesDirectoryExist (path </> "refs")
        pure (headExists && objectsExist && refsExist)

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    case found of
        Just repository -> pure repository
        Nothing -> ioError (userError ("not a git repository: " ++ path))

initRepository :: FilePath -> IO Repository
initRepository = initRepositoryWithBranch defaultBranchName

-- | Create a repository at a path. Files that already exist are left alone, so
-- running this on an existing repository keeps its HEAD and config.
initRepositoryWithBranch :: String -> FilePath -> IO Repository
initRepositoryWithBranch branchName targetPath = do
    case validateReferenceName ("refs/heads/" ++ branchName) of
        Left err -> ioError (userError err)
        Right () -> pure ()
    let repository = repositoryAt targetPath
    createDirectoryIfMissing True targetPath
    forM_ defaultGitDirectories $ \relativePath ->
        createDirectoryIfMissing True (targetPath </> relativePath)
    writeFileIfMissing
        (repositoryGitDir repository </> "HEAD")
        ("ref: refs/heads/" ++ branchName ++ "\n")
    writeFileIfMissing (repositoryConfigPath repository) defaultConfigText
    writeFileIfMissing
        (repositoryGitDir repository </> "description")
        "Unnamed repository; edit this file 'description' to name the repository.\n"
    pure repository

defaultConfigText :: String
defaultConfigText =
    unlines
        [ "[core]"
        , "\trepositoryformatversion = 0"
        , "\tfilemode = true"
        , "\tbare = false"
        ]

writeFileIfMissing :: FilePath -> String -> IO ()
writeFileIfMissing path contents = do
    exists <- doesFileExist path
    if exists then pure () else writeFile path contents

-- | Read one value from the repository config. The parser understands
-- section headers, comments, and @key = value@ lines; that covers the two
-- values this project needs (the user identity and the clone remote).
readConfigValue :: Repository -> String -> String -> IO (Maybe String)
readConfigValue repository sectionName keyName = do
    let configPath = repositoryConfigPath repository
    exists <- doesFileExist configPath
    if not exists
        then pure Nothing
        else do
            contents <- readFile configPath
            pure (search (map trim (lines contents)) Nothing)
  where
    wantedSection = map toLowerAscii sectionName
    wantedKey = map toLowerAscii keyName

    search [] _ = Nothing
    search (line:rest) currentSection
        | null line = search rest currentSection
        | head line `elem` "#;" = search rest currentSection
        | head line == '[' = search rest (Just (normalizeSection line))
        | otherwise =
            case break (== '=') line of
                (keyPart, '=':valuePart)
                    | currentSection == Just wantedSection ->
                        if map toLowerAscii (trim keyPart) == wantedKey
                            then Just (trim valuePart)
                            else search rest currentSection
                _ -> search rest currentSection

    normalizeSection line =
        map toLowerAscii (trim (takeWhile (/= ']') (drop 1 line)))

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

toLowerAscii :: Char -> Char
toLowerAscii character
    | character >= 'A' && character <= 'Z' = toEnum (fromEnum character + 32)
    | otherwise = character

-- | Reject a reference name that git itself would reject. Clone writes names
-- taken from a remote advertisement, and those names become paths under
-- @.git@, so this check is the boundary between untrusted data and the disk.
validateReferenceName :: String -> Either String ()
validateReferenceName name
    | null name = Left "reference name is empty"
    | not ("refs/" `isPrefixOf` name) = Left "reference name does not start with refs/"
    | any isSpace name = Left "reference name contains whitespace"
    | any isControl name = Left "reference name contains a control character"
    | any (`elem` "~^:?*[\\") name = Left "reference name contains a forbidden character"
    | "/" `isPrefixOf` name || "/" `isSuffixOf` name = Left "reference name has an empty component"
    | "//" `isInfixOf` name = Left "reference name has an empty component"
    | ".." `isInfixOf` name = Left "reference name contains a double dot"
    | "@{" `isInfixOf` name = Left "reference name contains an at brace sequence"
    | ".lock" `isSuffixOf` name = Left "reference name ends with .lock"
    | "." `isSuffixOf` name = Left "reference name ends with a dot"
    | any invalidComponent (splitOn '/' name) = Left "reference name has an invalid component"
    | otherwise = Right ()
  where
    invalidComponent component =
        null component || component == "." || component == ".." || "." `isPrefixOf` component

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn separator text =
    let (before, rest) = break (== separator) text
     in case rest of
            [] -> [before]
            (_:after) -> before : splitOn separator after
