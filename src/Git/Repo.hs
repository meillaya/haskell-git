module Git.Repo
    ( Repository
    , defaultGitDirectories
    , ensureRepository
    , initRepository
    , repositoryAt
    , repositoryGitDir
    , repositoryObjectsDir
    , repositoryRootDir
    ) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))

data Repository = Repository
    { repositoryRootDir :: FilePath
    , repositoryGitDir :: FilePath
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

repositoryObjectsDir :: Repository -> FilePath
repositoryObjectsDir repository = repositoryGitDir repository </> "objects"

repositoryAt :: FilePath -> Repository
repositoryAt targetPath = Repository targetPath (targetPath </> ".git")

initRepository :: FilePath -> IO Repository
initRepository targetPath = do
    let repository = repositoryAt targetPath
    createDirectoryIfMissing True targetPath
    mapM_ (createDirectoryIfMissing True . (targetPath </>)) defaultGitDirectories
    writeFile (repositoryGitDir repository </> "HEAD") "ref: refs/heads/master\n"
    writeFile
        (repositoryGitDir repository </> "config")
        (unlines
            [ "[core]"
            , "\trepositoryformatversion = 0"
            , "\tfilemode = true"
            , "\tbare = false"
            ]
        )
    writeFile
        (repositoryGitDir repository </> "description")
        "Unnamed repository; edit this file 'description' to name the repository.\n"
    pure repository

ensureRepository :: FilePath -> IO Repository
ensureRepository targetPath = do
    let repository = repositoryAt targetPath
    exists <- doesDirectoryExist (repositoryGitDir repository)
    if exists
        then pure repository
        else ioError (userError ("Not a Git repository: " ++ targetPath))
