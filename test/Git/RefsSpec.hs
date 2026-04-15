module Git.RefsSpec
    ( run
    ) where

import Git.Hash (ObjectId, objectIdFromHex)
import Git.Refs (currentHeadCommit, readHeadReference, readRefValue, symbolicHead, updateHeadReference, writeRefValue)
import Git.Repo (initRepository)
import TestSupport (assertEqual, withTempDirectory)

run :: IO ()
run =
    withTempDirectory "haskell-git-refs" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        headReference <- readHeadReference repository
        assertEqual
            (symbolicHead "refs/heads/master")
            headReference
            "HEAD should point to refs/heads/master after init"
        commitId <- requireObjectId "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a"
        writeRefValue repository "refs/heads/master" commitId
        branchRef <- readRefValue repository "refs/heads/master"
        assertEqual
            (Just commitId)
            branchRef
            "writeRefValue should persist branch refs"
        updateHeadReference repository commitId
        currentCommit <- currentHeadCommit repository
        assertEqual
            (Just commitId)
            currentCommit
            "currentHeadCommit should resolve the current symbolic HEAD"

requireObjectId :: String -> IO ObjectId
requireObjectId hexText =
    maybe (fail ("invalid object id fixture: " ++ hexText)) pure (objectIdFromHex hexText)
