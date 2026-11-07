module Git.MergeSpec
    ( run
    ) where

import Data.List (isInfixOf)
import Git.Hash (ObjectId)
import Git.Merge
    ( MergeResult (..)
    , mergeAbort
    , mergeRepository
    )
import Git.Refs (resolveRevision)
import Git.Repo (Repository, findRepository)
import Git.Store (loadStore)
import System.Directory (createDirectoryIfMissing)
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
    mergesNonOverlappingChanges
    fastForwards
    detectsConflicts
    abortsConflicts

mergesNonOverlappingChanges :: IO ()
mergesNonOverlappingChanges =
    withTempDirectory "haskell-git-merge" $ \temporaryDir -> do
        _ <- createBase temporaryDir
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "-b", "feature"]
        writeFile (temporaryDir </> "b.txt") "feature b\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "feature b"]
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "main"]
        writeFile (temporaryDir </> "a.txt") "main a\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "main a"]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        target <- resolveCommit repository "feature"
        result <- mergeRepository repository store target
        case result of
            MergeCreated _ -> pure ()
            MergeUpToDate -> fail "divergent branches should not be up to date"
            MergeFastForward _ -> fail "divergent branches should not fast-forward"
            MergeConflicted paths -> fail ("unexpected conflicts: " ++ show paths)
        aContents <- readFile (temporaryDir </> "a.txt")
        bContents <- readFile (temporaryDir </> "b.txt")
        assertEqual "main a\n" aContents "The clean merge should keep main's change"
        assertEqual "feature b\n" bContents "The clean merge should keep feature's change"
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "A clean merge should leave a clean worktree"
        _ <- runGit ["-C", temporaryDir, "fsck", "--strict"]
        logText <- runGit ["-C", temporaryDir, "log", "--format=%P", "-n", "1"]
        assert (length (words logText) == 2) "The merge commit should have two parents"

fastForwards :: IO ()
fastForwards =
    withTempDirectory "haskell-git-merge-ff" $ \temporaryDir -> do
        _ <- createBase temporaryDir
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "-b", "feature"]
        writeFile (temporaryDir </> "feature.txt") "feature\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "feature"]
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "main"]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        target <- resolveCommit repository "feature"
        result <- mergeRepository repository store target
        case result of
            MergeFastForward objectId ->
                assertEqual target objectId "Fast-forward should move HEAD to the target"
            _ -> fail "Expected a fast-forward merge"
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "Fast-forward should leave a clean worktree"

detectsConflicts :: IO ()
detectsConflicts =
    withTempDirectory "haskell-git-merge-conflict" $ \temporaryDir -> do
        _ <- createConflictBase temporaryDir
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "-b", "feature"]
        writeFile (temporaryDir </> "conflict.txt") "feature\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "feature change"]
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "main"]
        writeFile (temporaryDir </> "conflict.txt") "main\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "main change"]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        target <- resolveCommit repository "feature"
        result <- mergeRepository repository store target
        case result of
            MergeConflicted paths ->
                assertEqual ["conflict.txt"] paths "The conflicting path should be reported"
            _ -> fail "Expected merge conflicts"
        conflictContents <- readFile (temporaryDir </> "conflict.txt")
        assertContains "<<<<<<< HEAD" conflictContents "The conflict file should carry markers"
        assertContains ">>>>>>> MERGE_HEAD" conflictContents "The conflict file should carry an end marker"
        unmerged <- runGit ["-C", temporaryDir, "ls-files", "-u"]
        assert ("conflict.txt" `isInfixOf` unmerged) "git should see unmerged index entries"

abortsConflicts :: IO ()
abortsConflicts =
    withTempDirectory "haskell-git-merge-abort" $ \temporaryDir -> do
        _ <- createConflictBase temporaryDir
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "-b", "feature"]
        writeFile (temporaryDir </> "conflict.txt") "feature\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "feature change"]
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "main"]
        writeFile (temporaryDir </> "conflict.txt") "main\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "main change"]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        target <- resolveCommit repository "feature"
        _ <- mergeRepository repository store target
        mergeAbort repository store
        contents <- readFile (temporaryDir </> "conflict.txt")
        assertEqual "main\n" contents "Aborting should restore the pre-merge content"
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "Aborting should leave a clean worktree"

createBase :: FilePath -> IO ()
createBase directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "a.txt") "base a\n"
    writeFile (directory </> "b.txt") "base b\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "base"]
    pure ()

createConflictBase :: FilePath -> IO ()
createConflictBase directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "conflict.txt") "base\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "base"]
    pure ()

resolveCommit :: Repository -> String -> IO ObjectId
resolveCommit repository revision = do
    maybeObjectId <- resolveRevision repository revision
    maybe (fail ("unknown revision: " ++ revision)) pure maybeObjectId

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found
