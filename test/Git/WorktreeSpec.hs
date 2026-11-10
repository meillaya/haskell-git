module Git.WorktreeSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Data.List (isInfixOf)
import Git.Hash (ObjectId, objectIdFromHex)
import Git.Refs (symbolicHead, writeHeadReference)
import Git.Repo (Repository, findRepository)
import Git.Store (loadStore)
import Git.Worktree
    ( checkoutTree
    , resetHard
    , statusRepository
    )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import TestSupport
    ( assert
    , assertContains
    , assertEqual
    , runBash
    , runGit
    , withTempDirectory
    )

run :: IO ()
run = do
    checksOutAnotherBranch
    reportsDirtyStatus
    ignoresUntrackedFiles
    resetsHard

checksOutAnotherBranch :: IO ()
checksOutAnotherBranch =
    withTempDirectory "haskell-git-checkout" $ \temporaryDir -> do
        _ <- createRepository temporaryDir
        featureId <- commitOnBranch temporaryDir "feature" "feature.txt" "feature\n"
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "main"]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        writeHeadReference repository (symbolicHead "refs/heads/feature")
        checkoutTree repository store featureId
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "checkout should leave a clean index"
        featureExists <- doesFileExist (temporaryDir </> "feature.txt")
        assert featureExists "checkout should create the feature file"
        featureContents <- readFile (temporaryDir </> "feature.txt")
        assertContains "feature" featureContents "checkout should write the feature file contents"
        _ <- runGit ["-C", temporaryDir, "fsck", "--strict"]
        pure ()

reportsDirtyStatus :: IO ()
reportsDirtyStatus =
    withTempDirectory "haskell-git-status" $ \temporaryDir -> do
        _ <- createRepository temporaryDir
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        cleanStatus <- statusRepository repository store
        assertEqual [] cleanStatus "A fresh checkout should be clean"
        appendFile (temporaryDir </> "main.txt") "changed\n"
        dirtyStatus <- statusRepository repository store
        assert (" M main.txt" `elem` dirtyStatus) "A modified tracked file should be reported as unstaged"
        writeFile (temporaryDir </> "untracked.txt") "new\n"
        untrackedStatus <- statusRepository repository store
        assert ("?? untracked.txt" `elem` untrackedStatus) "An untracked file should be reported"
        _ <- runBash ("git -C " ++ show temporaryDir ++ " checkout -q -- main.txt")
        pure ()

ignoresUntrackedFiles :: IO ()
ignoresUntrackedFiles =
    withTempDirectory "haskell-git-status-ignore" $ \temporaryDir -> do
        _ <- createRepository temporaryDir
        writeFile
            (temporaryDir </> ".gitignore")
            "*.o\nbuild/\n!keep.o\n"
        writeFile (temporaryDir </> "ignored.o") "object\n"
        writeFile (temporaryDir </> "keep.o") "keep\n"
        createDirectoryIfMissing True (temporaryDir </> "build")
        writeFile (temporaryDir </> "build" </> "artifact") "built\n"
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        status <- statusRepository repository store
        assert (not ("?? ignored.o" `elem` status)) "*.o should be ignored"
        assert (not ("?? build/artifact" `elem` status)) "build/ should be ignored"
        assert ("?? keep.o" `elem` status) "A later negation should unignore keep.o"
        assert ("?? .gitignore" `elem` status) ".gitignore itself should still be visible"

resetsHard :: IO ()
resetsHard =
    withTempDirectory "haskell-git-reset" $ \temporaryDir -> do
        firstId <- createRepository temporaryDir
        _ <- commitOnBranch temporaryDir "feature" "feature.txt" "feature\n"
        _ <- runGit ["-C", temporaryDir, "checkout", "-q", "main"]
        writeFile (temporaryDir </> "main.txt") "dirty\n"
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        resetHard repository store firstId
        contents <- readFile (temporaryDir </> "main.txt")
        assertEqual "main\n" contents "reset --hard should restore the tracked file"
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "reset --hard should leave a clean worktree"

createRepository :: FilePath -> IO ObjectId
createRepository directory = do
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "main.txt") "main\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "main"]
    headText <- trim <$> runGit ["-C", directory, "rev-parse", "HEAD"]
    maybe (fail "invalid HEAD") pure (objectIdFromHex headText)

commitOnBranch :: FilePath -> String -> FilePath -> String -> IO ObjectId
commitOnBranch directory branchName relativePath contents = do
    _ <- runGit ["-C", directory, "checkout", "-q", "-b", branchName]
    createDirectoryIfMissing True (takeDirectory (directory </> relativePath))
    writeFile (directory </> relativePath) contents
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "commit on feature"]
    headText <- trim <$> runGit ["-C", directory, "rev-parse", "HEAD"]
    maybe (fail "invalid HEAD") pure (objectIdFromHex headText)

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
