module Git.RefsSpec
    ( run
    ) where

import Control.Exception (IOException, try)
import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Refs
    ( HeadReference (..)
    , currentHeadCommit
    , listRefs
    , readHeadReference
    , readRefValue
    , resolveHeadReference
    , resolveRevision
    , symbolicHead
    , writeHeadReference
    , writeRefValue
    )
import Git.Repo (Repository, initRepository, repositoryGitDir)
import System.FilePath ((</>))
import TestSupport (assert, assertEqual, withTempDirectory)

-- | Check symbolic and detached HEAD, packed references, revision resolution,
-- and that ref names that escape the repository are refused.
run :: IO ()
run = do
    symbolicAndDetached
    packedReferences
    revisionResolution
    invalidNamesRejected

-- | Write a branch ref, resolve HEAD, then detach and resolve again.
symbolicAndDetached :: IO ()
symbolicAndDetached =
    withTempDirectory "haskell-git-refs" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        headReference <- readHeadReference repository
        assertEqual (symbolicHead "refs/heads/main") headReference "HEAD should point at refs/heads/main after init"
        commitId <- requireObjectId "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a"
        writeRefValue repository "refs/heads/main" commitId
        branchRef <- readRefValue repository "refs/heads/main"
        assertEqual (Just commitId) branchRef "writeRefValue should persist branch refs"
        currentCommit <- currentHeadCommit repository
        assertEqual (Just commitId) currentCommit "currentHeadCommit should resolve the symbolic HEAD"
        writeHeadReference repository (DetachedHead commitId)
        detachedHead <- readHeadReference repository
        assertEqual (DetachedHead commitId) detachedHead "A detached HEAD should read back"
        detachedCommit <- resolveHeadReference repository
        assertEqual (Just commitId) detachedCommit "A detached HEAD should resolve to its object id"

-- | Read a packed-refs file and check a loose ref wins.
packedReferences :: IO ()
packedReferences =
    withTempDirectory "haskell-git-packed-refs" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        packedId <- requireObjectId "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a"
        looseId <- requireObjectId "4cf9f177c4c015836fca6a31f9c3917e89ae29ec"
        writeFile
            (repositoryGitDir repository </> "packed-refs")
            ( unlines
                [ "# pack-refs with: peeled fully-peeled sorted"
                , objectIdToHex packedId ++ " refs/heads/packed"
                , "^" ++ objectIdToHex looseId
                ]
            )
        writeRefValue repository "refs/heads/packed" looseId
        refs <- listRefs repository
        assertEqual
            (Just looseId)
            (lookup "refs/heads/packed" refs)
            "A loose ref should win over the packed value"
        let packedOnly = [refPath | (refPath, _) <- refs]
        assert ("refs/heads/packed" `elem` packedOnly) "listRefs should include the packed ref"

-- | Resolve HEAD, a branch shorthand, a full ref, and a full object id.
revisionResolution :: IO ()
revisionResolution =
    withTempDirectory "haskell-git-revision" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        commitId <- requireObjectId "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a"
        writeRefValue repository "refs/heads/main" commitId
        byShorthand <- resolveRevision repository "main"
        assertEqual (Just commitId) byShorthand "A branch shorthand should resolve"
        byFullName <- resolveRevision repository "refs/heads/main"
        assertEqual (Just commitId) byFullName "A full ref name should resolve"
        byHead <- resolveRevision repository "HEAD"
        assertEqual (Just commitId) byHead "HEAD should resolve"
        byObjectId <- resolveRevision repository (objectIdToHex commitId)
        assertEqual (Just commitId) byObjectId "A full object id should resolve"

-- | Try to write a ref whose name escapes the repository.
invalidNamesRejected :: IO ()
invalidNamesRejected =
    withTempDirectory "haskell-git-refs-invalid" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        commitId <- requireObjectId "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a"
        result <- try (writeRefValue repository "refs/heads/../escape" commitId) :: IO (Either IOException ())
        case result of
            Left _ -> pure ()
            Right () -> fail "A ref name with a dot sequence should be refused"

requireObjectId :: String -> IO ObjectId
requireObjectId hexText =
    maybe (fail ("invalid fixture object id: " ++ hexText)) pure (objectIdFromHex hexText)
