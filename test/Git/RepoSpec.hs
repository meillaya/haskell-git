module Git.RepoSpec
    ( run
    ) where

import Control.Monad (forM_)
import Git.Repo (defaultGitDirectories, initRepository, repositoryGitDir)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import TestSupport (assert, withTempDirectory)

run :: IO ()
run =
    withTempDirectory "haskell-git-repo" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        assert
            (repositoryGitDir repository == temporaryDir </> ".git")
            "initRepository should point at the .git directory"
        forM_ defaultGitDirectories $ \relativePath -> do
            let fullPath = temporaryDir </> relativePath
            exists <- doesDirectoryExist fullPath
            assert exists ("Missing repository directory: " ++ fullPath)
        headExists <- doesFileExist (temporaryDir </> ".git" </> "HEAD")
        configExists <- doesFileExist (temporaryDir </> ".git" </> "config")
        descriptionExists <- doesFileExist (temporaryDir </> ".git" </> "description")
        assert headExists "initRepository should create HEAD"
        assert configExists "initRepository should create config"
        assert descriptionExists "initRepository should create description"
