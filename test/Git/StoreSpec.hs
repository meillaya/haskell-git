module Git.StoreSpec
    ( run
    ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort)
import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Object (gitObjectPayload)
import Git.Object.Commit (commitFromObject, commitTree)
import Git.Object.Loose (objectIdForObject)
import Git.Repo (findRepository)
import Git.Store
    ( Store
    , flattenTree
    , listObjectIds
    , loadStore
    , readObjectOrFail
    , walkCommits
    )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import TestSupport (assert, assertEqual, runBash, runGit, withTempDirectory)

-- | Check the store layer on a repository that git has packed: every object
-- resolves and hashes to its id, the tree flattens to git's paths, the commit
-- walk matches git, and a large blob round-trips.
run :: IO ()
run = do
    objectsResolve
    treeFlattens
    commitsWalk
    largeBlobRoundTrips

withPackedRepository :: (FilePath -> IO a) -> IO a
withPackedRepository action =
    withTempDirectory "haskell-git-store" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "-b", "main", temporaryDir]
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        createDirectoryIfMissing True (temporaryDir </> "docs")
        BS.writeFile (temporaryDir </> "big.bin") bigBlobContents
        writeFile (temporaryDir </> "docs" </> "nested.txt") "nested\n"
        writeFile (temporaryDir </> "run.sh") "#!/bin/sh\necho hi\n"
        _ <- runBash ("chmod +x " ++ temporaryDir </> "run.sh")
        _ <- runBash ("ln -s big.bin " ++ temporaryDir </> "link")
        forM_ [1 :: Int, 2, 3] $ \number -> do
            BS.writeFile (temporaryDir </> "changing.txt") (BS8.pack ("version " ++ show number ++ "\n"))
            _ <- runGit ["-C", temporaryDir, "add", "-A"]
            () <$ runGit ["-C", temporaryDir, "commit", "-qm", "commit " ++ show number]
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        action temporaryDir

-- | Load a packed repository and check every object hashes to its id.
objectsResolve :: IO ()
objectsResolve =
    withPackedRepository $ \temporaryDir -> do
        store <- loadStoreFor temporaryDir
        objectIds <- listObjectIds store
        gitObjectIds <-
            lines <$> runGit ["-C", temporaryDir, "cat-file", "--batch-all-objects", "--batch-check=%(objectname)"]
        assertEqual (sort gitObjectIds) (sort (map objectIdToHex objectIds)) "The store should list the same objects as git"
        forM_ objectIds $ \objectId -> do
            gitObjectValue <- readObjectOrFail store objectId
            assertEqual objectId (objectIdForObject gitObjectValue) "Every stored object should hash to its id"

-- | Flatten HEAD's tree and compare the paths with git.
treeFlattens :: IO ()
treeFlattens =
    withPackedRepository $ \temporaryDir -> do
        store <- loadStoreFor temporaryDir
        headCommitId <- headCommitOf temporaryDir
        headObject <- readObjectOrFail store headCommitId
        currentCommit <- either fail pure (commitFromObject headObject)
        flattened <- flattenTree store (commitTree currentCommit)
        gitNames <- lines <$> runGit ["-C", temporaryDir, "ls-tree", "-r", "--name-only", "HEAD"]
        assertEqual (sort gitNames) (sort (map fst flattened)) "Flattened tree paths should match git ls-tree"

-- | Walk commits from HEAD and compare the count with git rev-list.
commitsWalk :: IO ()
commitsWalk =
    withPackedRepository $ \temporaryDir -> do
        store <- loadStoreFor temporaryDir
        headCommitId <- headCommitOf temporaryDir
        commits <- walkCommits store headCommitId
        gitCount <- read . trim <$> runGit ["-C", temporaryDir, "rev-list", "--count", "HEAD"]
        assertEqual (gitCount :: Int) (length commits) "Walking commits should visit every reachable commit"

-- | Read a large blob from the pack and compare its bytes.
largeBlobRoundTrips :: IO ()
largeBlobRoundTrips =
    withPackedRepository $ \temporaryDir -> do
        store <- loadStoreFor temporaryDir
        blobIdText <- trim <$> runGit ["-C", temporaryDir, "rev-parse", "HEAD:big.bin"]
        blobId <- maybe (fail "git returned an invalid blob id") pure (objectIdFromHex blobIdText)
        blobObject <- readObjectOrFail store blobId
        assertEqual bigBlobLength (BS.length (gitObjectPayload blobObject)) "The large blob should read back at full length"
        assert (BS.take 400 bigBlobContents == BS.take 400 (gitObjectPayload blobObject)) "The large blob should start with the committed bytes"

loadStoreFor :: FilePath -> IO Store
loadStoreFor temporaryDir = do
    found <- findRepository temporaryDir
    repository <- maybe (fail "the fixture directory should be a repository") pure found
    loadStore repository

headCommitOf :: FilePath -> IO ObjectId
headCommitOf temporaryDir = do
    headText <- trim <$> runGit ["-C", temporaryDir, "rev-parse", "HEAD"]
    maybe (fail "git returned an invalid commit id") pure (objectIdFromHex headText)

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse

bigBlobLength :: Int
bigBlobLength = BS.length bigBlobContents

bigBlobContents :: BS.ByteString
bigBlobContents =
    BS.concat
        [ BS8.pack ("line " ++ show lineNumber ++ " ")
            <> BS.replicate 120 (fromIntegral (lineNumber `mod` 94) + 33)
            <> BS8.singleton '\n'
        | lineNumber <- [1 .. 1500 :: Int]
        ]
