module Git.RebaseSpec
    ( run
    ) where

import Data.List (isInfixOf)
import Git.Hash (ObjectId)
import Git.Rebase
    ( RebaseResult (..)
    , rebaseAbort
    , rebaseRepository
    )
import Git.Refs (resolveRevision)
import Git.Repo (Repository, findRepository)
import Git.Store (loadStore)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import TestSupport
    ( assert
    , assertContains
    , assertEqual
    , runGit
    , withTempDirectory
    )

run :: IO ()
run = do
    rebasesOntoMain
    detectsRebaseConflict

rebasesOntoMain :: IO ()
rebasesOntoMain =
    withTempDirectory "haskell-git-rebase" $ \temporaryDir -> do
        _ <- createRepository temporaryDir
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "-b", "feature"]
        writeFile (temporaryDir </> "feature.txt") "feature\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "feature commit"]
        featureHead <- trim <$> runGit ["-C", temporaryDir, "rev-parse", "HEAD"]
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "main"]
        writeFile (temporaryDir </> "main.txt") "main\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "main commit"]
        mainHead <- trim <$> runGit ["-C", temporaryDir, "rev-parse", "HEAD"]
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "feature"]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        target <- resolveCommit repository "main"
        result <- rebaseRepository repository store target
        case result of
            RebaseComplete _ -> pure ()
            _ -> fail ("expected a complete rebase, got " ++ show result)
        branchHead <- trim <$> runGit ["-C", temporaryDir, "rev-parse", "refs/heads/feature"]
        assert (branchHead /= featureHead) "rebase should rewrite the feature commit"
        parentText <- runGit ["-C", temporaryDir, "log", "--format=%P", "-n", "1"]
        assertContains mainHead parentText "The replayed commit should sit on the upstream commit"
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "A clean rebase should leave a clean worktree"
        mainExists <- doesFileExist (temporaryDir </> "main.txt")
        featureExists <- doesFileExist (temporaryDir </> "feature.txt")
        assert mainExists "rebase should keep the upstream file"
        assert featureExists "rebase should keep the replayed file"
        _ <- runGit ["-C", temporaryDir, "fsck", "--strict"]
        pure ()

detectsRebaseConflict :: IO ()
detectsRebaseConflict =
    withTempDirectory "haskell-git-rebase-conflict" $ \temporaryDir -> do
        _ <- createConflictRepository temporaryDir
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "main"]
        writeFile (temporaryDir </> "conflict.txt") "main\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "main change"]
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "feature"]
        originalBranchHead <- trim <$> runGit ["-C", temporaryDir, "rev-parse", "HEAD"]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        target <- resolveCommit repository "main"
        result <- rebaseRepository repository store target
        case result of
            RebaseConflicted paths -> assertEqual ["conflict.txt"] paths "rebase should report the conflict"
            _ -> fail ("expected rebase conflict, got " ++ show result)
        unmerged <- runGit ["-C", temporaryDir, "ls-files", "-u"]
        assert ("conflict.txt" `isInfixOf` unmerged) "git should see the rebase conflict"
        rebaseAbort repository store
        abortedHead <- trim <$> runGit ["-C", temporaryDir, "rev-parse", "refs/heads/feature"]
        assertEqual originalBranchHead abortedHead "abort should restore the feature ref"
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "abort should restore a clean worktree"
        pure ()

createRepository :: FilePath -> IO ()
createRepository directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "base.txt") "base\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "base"]
    pure ()

createConflictRepository :: FilePath -> IO ()
createConflictRepository directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "conflict.txt") "base\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "base"]
    _ <- runGit ["-C", directory, "checkout", "-q", "-b", "feature"]
    writeFile (directory </> "conflict.txt") "feature\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "feature change"]
    pure ()

resolveCommit :: Repository -> String -> IO ObjectId
resolveCommit repository revision = do
    maybeObjectId <- resolveRevision repository revision
    maybe (fail ("unknown revision: " ++ revision)) pure maybeObjectId

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
