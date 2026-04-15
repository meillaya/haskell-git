module Git.InteropSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Hash (objectIdFromHex, objectIdToHex)
import Git.Object.Blob (blobContents, blobFromBytes, blobFromObject, blobToObject)
import Git.Object.Commit (commit, commitToObject, personIdent)
import Git.Object.Loose (readLooseObject, writeLooseObject)
import Git.Object.Tree (TreeEntryMode (RegularFile), renderTreeEntry, treeEntry, treeFromObject, treeToObject)
import Git.Refs (currentHeadCommit, updateHeadReference)
import Git.Repo (initRepository, repositoryAt)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (proc, readCreateProcessWithExitCode)
import TestSupport (assert, assertEqual, withTempDirectory)

run :: IO ()
run = do
    gitReadsOurInit
    gitReadsOurBlob
    weReadGitBlob
    gitReadsOurTree
    weReadGitTree
    gitReadsOurCommit

gitReadsOurInit :: IO ()
gitReadsOurInit =
    withTempDirectory "haskell-git-init-interop" $ \temporaryDir -> do
        _ <- initRepository temporaryDir
        (gitDirExit, gitDirStdout, gitDirStderr) <-
            readCreateProcessWithExitCode
                (proc "git" ["-C", temporaryDir, "rev-parse", "--git-dir"])
                ""
        assert (gitDirExit == ExitSuccess) ("git rev-parse --git-dir failed: " ++ gitDirStderr)
        assertEqual ".git\n" gitDirStdout "Git should recognize the initialized .git directory"
        (headExit, headStdout, headStderr) <-
            readCreateProcessWithExitCode
                (proc "git" ["-C", temporaryDir, "symbolic-ref", "HEAD"])
                ""
        assert (headExit == ExitSuccess) ("git symbolic-ref HEAD failed: " ++ headStderr)
        assertEqual "refs/heads/master\n" headStdout "Git should recognize the symbolic HEAD written by init"

gitReadsOurBlob :: IO ()
gitReadsOurBlob =
    withTempDirectory "haskell-git-interop-write" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        let expectedContents = BS8.pack "hello from haskell-git\n"
        objectId <- writeLooseObject repository (blobToObject (blobFromBytes expectedContents))
        (exitCode, stdoutText, stderrText) <-
            readCreateProcessWithExitCode
                (proc "git" ["-C", temporaryDir, "cat-file", "-p", objectIdToHex objectId])
                ""
        assert (exitCode == ExitSuccess) ("git cat-file failed: " ++ stderrText)
        assertEqual expectedContents (BS8.pack stdoutText) "Git should read blobs written by this project"

weReadGitBlob :: IO ()
weReadGitBlob =
    withTempDirectory "haskell-git-interop-read" $ \temporaryDir -> do
        _ <- initRepository temporaryDir
        let expectedContents = BS8.pack "hello from git\n"
        (exitCode, stdoutText, stderrText) <-
            readCreateProcessWithExitCode
                ((proc "git" ["-C", temporaryDir, "hash-object", "-w", "--stdin"]))
                (BS8.unpack expectedContents)
        assert (exitCode == ExitSuccess) ("git hash-object failed: " ++ stderrText)
        objectId <-
            maybe
                (fail "git returned an invalid object id")
                pure
                (objectIdFromHex (trimTrailingNewline stdoutText))
        gitObject <- readLooseObject (repositoryAt temporaryDir) objectId
        blob <- either fail pure (blobFromObject gitObject)
        assertEqual expectedContents (blobContents blob) "This project should read blobs written by Git"

trimTrailingNewline :: String -> String
trimTrailingNewline = reverse . dropWhile (`elem` "\r\n") . reverse

gitReadsOurTree :: IO ()
gitReadsOurTree =
    withTempDirectory "haskell-git-tree-write" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        let blobContentsBytes = BS8.pack "tree blob\n"
        blobId <- writeLooseObject repository (blobToObject (blobFromBytes blobContentsBytes))
        treeId <-
            writeLooseObject
                repository
                (treeToObject [treeEntry RegularFile (BS8.pack "hello.txt") blobId])
        (exitCode, stdoutText, stderrText) <-
            readCreateProcessWithExitCode
                (proc "git" ["-C", temporaryDir, "ls-tree", objectIdToHex treeId])
                ""
        assert (exitCode == ExitSuccess) ("git ls-tree failed: " ++ stderrText)
        assertEqual
            ("100644 blob " ++ objectIdToHex blobId ++ "\thello.txt\n")
            stdoutText
            "Git should inspect trees written by this project"

weReadGitTree :: IO ()
weReadGitTree =
    withTempDirectory "haskell-git-tree-read" $ \temporaryDir -> do
        _ <- initRepository temporaryDir
        let blobContentsBytes = BS8.pack "tree from git\n"
        (blobExit, blobStdout, blobStderr) <-
            readCreateProcessWithExitCode
                (proc "git" ["-C", temporaryDir, "hash-object", "-w", "--stdin"])
                (BS8.unpack blobContentsBytes)
        assert (blobExit == ExitSuccess) ("git hash-object for tree setup failed: " ++ blobStderr)
        let blobIdText = trimTrailingNewline blobStdout
        (indexExit, _, indexStderr) <-
            readCreateProcessWithExitCode
                (proc "git" ["-C", temporaryDir, "update-index", "--add", "--cacheinfo", "100644", blobIdText, "hello.txt"])
                ""
        assert (indexExit == ExitSuccess) ("git update-index failed: " ++ indexStderr)
        (treeExit, treeStdout, treeStderr) <-
            readCreateProcessWithExitCode
                (proc "git" ["-C", temporaryDir, "write-tree"])
                ""
        assert (treeExit == ExitSuccess) ("git write-tree failed: " ++ treeStderr)
        treeId <-
            maybe
                (fail "git returned an invalid tree id")
                pure
                (objectIdFromHex (trimTrailingNewline treeStdout))
        gitObject <- readLooseObject (repositoryAt temporaryDir) treeId
        treeEntries <- either fail pure (treeFromObject gitObject)
        assert (length treeEntries == 1) "This project should decode a single Git tree entry"
        assertEqual
            ("100644 blob " ++ blobIdText ++ "\thello.txt")
            (renderTreeEntry (head treeEntries))
            "This project should read trees written by Git"

gitReadsOurCommit :: IO ()
gitReadsOurCommit =
    withTempDirectory "haskell-git-commit-write" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        blobId <- writeLooseObject repository (blobToObject (blobFromBytes (BS8.pack "commit blob\n")))
        treeId <- writeLooseObject repository (treeToObject [treeEntry RegularFile (BS8.pack "hello.txt") blobId])
        let author = personIdent (BS8.pack "Ada Lovelace") (BS8.pack "ada@example.com") 1713220000 (BS8.pack "+0000")
            firstCommit = commit treeId [] author author (BS8.pack "Initial commit\n")
        firstCommitId <- writeLooseObject repository (commitToObject firstCommit)
        updateHeadReference repository firstCommitId
        currentCommit <- currentHeadCommit repository
        assertEqual
            (Just firstCommitId)
            currentCommit
            "Updating HEAD should point the current branch at the first commit"
        let secondCommit = commit treeId [firstCommitId] author author (BS8.pack "Second commit\n")
        secondCommitId <- writeLooseObject repository (commitToObject secondCommit)
        updateHeadReference repository secondCommitId
        (exitCode, stdoutText, stderrText) <-
            readCreateProcessWithExitCode
                (proc "git" ["-C", temporaryDir, "cat-file", "-p", objectIdToHex secondCommitId])
                ""
        assert (exitCode == ExitSuccess) ("git cat-file for commit failed: " ++ stderrText)
        assert
            (("tree " ++ objectIdToHex treeId) `elem` lines stdoutText)
            "Git should show the commit's tree entry"
        assert
            (("parent " ++ objectIdToHex firstCommitId) `elem` lines stdoutText)
            "Git should show the commit's parent entry"
        assert
            ("author Ada Lovelace <ada@example.com> 1713220000 +0000" `elem` lines stdoutText)
            "Git should show the commit author metadata"
