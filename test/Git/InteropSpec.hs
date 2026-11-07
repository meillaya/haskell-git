module Git.InteropSpec
    ( run
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort)
import Git.Hash (objectIdFromHex, objectIdToHex)
import Git.Index (IndexEntry (..), buildTreeFromIndex, writeIndex)
import Git.Object.Blob (blobFromBytes, blobToObject)
import Git.Object.Commit (commit, commitFromObject, commitToObject, commitTree, personIdent)
import Git.Object.Loose (writeLooseObject)
import Git.Object.Tree (TreeEntryMode (..), treeEntry, treeToObject)
import Git.Refs (updateHeadReference)
import Git.Repo (Repository, findRepository, initRepository, repositoryIndexPath, repositoryRootDir)
import Git.Store (flattenTree, loadStore, readObjectOrFail)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import TestSupport (assert, assertEqual, runGit, withTempDirectory)

-- | Check both directions of interoperability: git reads the objects this
-- project writes, this project reads the objects git writes into a packed
-- repository, and a commit written through the library is accepted by git.
run :: IO ()
run = do
    gitReadsOurInit
    gitReadsOurObjects
    weReadGitObjects
    writePathCreatesCommit

-- | Create a repository here and inspect it with git.
gitReadsOurInit :: IO ()
gitReadsOurInit =
    withTempDirectory "haskell-git-init-interop" $ \temporaryDir -> do
        _ <- initRepository temporaryDir
        gitDirText <- runGit ["-C", temporaryDir, "rev-parse", "--git-dir"]
        assertEqual ".git\n" gitDirText "Git should recognise the .git directory"
        headText <- runGit ["-C", temporaryDir, "symbolic-ref", "HEAD"]
        assertEqual "refs/heads/main\n" headText "Git should see the default branch"

-- | Write a blob, tree, and commit here and read them with git.
gitReadsOurObjects :: IO ()
gitReadsOurObjects =
    withTempDirectory "haskell-git-object-interop" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        let author = personIdent (BS8.pack "Ada") (BS8.pack "ada@example.com") 1713220000 (BS8.pack "+0000")
        blobId <- writeLooseObject repository (blobToObject (blobFromBytes (BS8.pack "hello\n")))
        nestedTreeId <-
            writeLooseObject
                repository
                (treeToObject [treeEntry RegularFile (BS8.pack "nested.txt") blobId])
        treeId <-
            writeLooseObject
                repository
                ( treeToObject
                    [ treeEntry RegularFile (BS8.pack "hello.txt") blobId
                    , treeEntry Directory (BS8.pack "docs") nestedTreeId
                    ]
                )
        blobText <- runGit ["-C", temporaryDir, "cat-file", "-p", objectIdToHex blobId]
        assertEqual "hello\n" blobText "Git should read a blob written by this project"
        treeText <- runGit ["-C", temporaryDir, "ls-tree", objectIdToHex treeId]
        assertEqual
            [ "040000 tree " ++ objectIdToHex nestedTreeId ++ "\tdocs"
            , "100644 blob " ++ objectIdToHex blobId ++ "\thello.txt"
            ][treeLine | treeLine <- lines treeText, not (null treeLine)]
            "Git should read a tree written by this project"
        let currentCommit = commit treeId [] author author (BS8.pack "Initial commit\n")
        commitId <- writeLooseObject repository (commitToObject currentCommit)
        updateHeadReference repository commitId
        commitText <- runGit ["-C", temporaryDir, "cat-file", "-p", objectIdToHex commitId]
        assert (("tree " ++ objectIdToHex treeId) `isLineOf` commitText) "Git should show the tree line"
        assert (("author Ada <ada@example.com> 1713220000 +0000") `isLineOf` commitText) "Git should show the author line"
        headCommit <- runGit ["-C", temporaryDir, "rev-parse", "HEAD"]
        assertEqual (objectIdToHex commitId ++ "\n") headCommit "Git should resolve the commit HEAD points at"

-- | Pack a repository with git and read its tree here.
weReadGitObjects :: IO ()
weReadGitObjects =
    withTempDirectory "haskell-git-read-interop" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "-b", "main", temporaryDir]
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        createDirectoryIfMissing True (temporaryDir </> "docs")
        writeFile (temporaryDir </> "hello.txt") "hello from git\n"
        writeFile (temporaryDir </> "docs" </> "nested.txt") "nested\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "first"]
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        headText <- trim <$> runGit ["-C", temporaryDir, "rev-parse", "HEAD"]
        headCommitId <-
            maybe (fail "git returned an invalid commit id") pure (objectIdFromHex headText)
        headObject <- readObjectOrFail store headCommitId
        currentCommit <- either fail pure (commitFromObject headObject)
        flattened <- flattenTree store (commitTree currentCommit)
        assertEqual
            ["docs/nested.txt", "hello.txt"]
            (sort (map fst flattened))
            "Reading git's packed tree should produce the same paths"

-- | Stage, write a tree, commit, and point HEAD here, then check with git.
writePathCreatesCommit :: IO ()
writePathCreatesCommit =
    withTempDirectory "haskell-git-write-interop" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        firstEntries <- stageFileAt [] repository "hello.txt" "hello\n"
        entries <- stageFileAt firstEntries repository "docs/nested.txt" "nested\n"
        BS.writeFile (repositoryIndexPath repository) (writeIndex entries)
        treeId <- buildTreeFromIndex repository entries
        let author = personIdent (BS8.pack "Ada") (BS8.pack "ada@example.com") 1713220000 (BS8.pack "+0000")
            currentCommit = commit treeId [] author author (BS8.pack "First commit\n")
        commitId <- writeLooseObject repository (commitToObject currentCommit)
        updateHeadReference repository commitId
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "git status should be clean after staging through this project"
        fsckText <- runGit ["-C", temporaryDir, "fsck", "--strict"]
        assertEqual "" fsckText "git fsck should accept the commit written by this project"
        logText <- runGit ["-C", temporaryDir, "log", "--format=%H"]
        assertEqual (objectIdToHex commitId) (trim logText) "git log should show the new commit"

stageFileAt :: [IndexEntry] -> Repository -> FilePath -> String -> IO [IndexEntry]
stageFileAt entries repository relativePath contents = do
    let destination = repositoryRootDir repository </> relativePath
        payload = BS8.pack contents
    createDirectoryIfMissing True (takeDirectory destination)
    BS.writeFile destination payload
    objectId <- writeLooseObject repository (blobToObject (blobFromBytes payload))
    pure
        ( entries
            ++ [ IndexEntry
                    { indexEntryPath = BS8.pack relativePath
                    , indexEntryMode = RegularFile
                    , indexEntrySize = fromIntegral (BS.length payload)
                    , indexEntryObjectId = objectId
                    }
               ]
        )

requireRepository :: FilePath -> IO Repository
requireRepository temporaryDir = do
    found <- findRepository temporaryDir
    maybe (fail "the fixture directory should be a repository") pure found

isLineOf :: String -> String -> Bool
isLineOf needle haystack = needle `elem` lines haystack

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
