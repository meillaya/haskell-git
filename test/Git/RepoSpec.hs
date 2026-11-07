module Git.RepoSpec
    ( run
    ) where

import Control.Monad (forM_)
import Git.Refs (readHeadReference, symbolicHead)
import Git.Repo
    ( defaultGitDirectories
    , findRepository
    , initRepository
    , initRepositoryWithBranch
    , repositoryGitDir
    , repositoryIsBare
    , repositoryRootDir
    )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import TestSupport (assert, assertEqual, runGit, withTempDirectory)

-- | Check repository creation, upward discovery, bare detection, and that a
-- second init keeps the existing HEAD.
run :: IO ()
run = do
    initCreatesLayout
    discoveryWalksUp
    bareRepositoryIsDetected
    reinitialisingKeepsHead

-- | Create a repository and check the directories, HEAD, and git's view of it.
initCreatesLayout :: IO ()
initCreatesLayout =
    withTempDirectory "haskell-git-repo" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        assertEqual (temporaryDir </> ".git") (repositoryGitDir repository) "initRepository should point at the .git directory"
        assert (not (repositoryIsBare repository)) "A normal init should not be bare"
        forM_ defaultGitDirectories $ \relativePath -> do
            exists <- doesDirectoryExist (temporaryDir </> relativePath)
            assert exists ("Missing repository directory: " ++ relativePath)
        headExists <- doesFileExist (temporaryDir </> ".git" </> "HEAD")
        configExists <- doesFileExist (temporaryDir </> ".git" </> "config")
        assert headExists "initRepository should create HEAD"
        assert configExists "initRepository should create config"
        headReference <- readHeadReference repository
        assertEqual (symbolicHead "refs/heads/main") headReference "HEAD should point at refs/heads/main"
        gitHead <- runGit ["-C", temporaryDir, "symbolic-ref", "HEAD"]
        assertEqual "refs/heads/main\n" gitHead "git should see the default branch the repository was created with"

-- | Find the repository from a nested directory.
discoveryWalksUp :: IO ()
discoveryWalksUp =
    withTempDirectory "haskell-git-discovery" $ \temporaryDir -> do
        _ <- initRepository temporaryDir
        let nestedDirectory = temporaryDir </> "a" </> "b"
        createDirectoryIfMissing True nestedDirectory
        found <- findRepository nestedDirectory
        case found of
            Nothing -> fail "findRepository should find an enclosing repository"
            Just repository ->
                assertEqual temporaryDir (repositoryRootDir repository) "findRepository should return the root of the enclosing repository"

-- | Make a bare repository with git and check discovery marks it bare.
bareRepositoryIsDetected :: IO ()
bareRepositoryIsDetected =
    withTempDirectory "haskell-git-bare" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "--bare", temporaryDir </> "bare.git"]
        found <- findRepository (temporaryDir </> "bare.git")
        case found of
            Nothing -> fail "findRepository should detect a bare repository"
            Just repository -> assert (repositoryIsBare repository) "A bare repository should be marked bare"

-- | Create with one branch and init again with another; HEAD should not move.
reinitialisingKeepsHead :: IO ()
reinitialisingKeepsHead =
    withTempDirectory "haskell-git-reinit" $ \temporaryDir -> do
        _ <- initRepositoryWithBranch "trunk" temporaryDir
        _ <- initRepositoryWithBranch "other" temporaryDir
        headContents <- readFile (temporaryDir </> ".git" </> "HEAD")
        assertEqual "ref: refs/heads/trunk\n" headContents "A second init should not change the existing HEAD"
