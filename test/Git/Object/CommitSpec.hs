module Git.Object.CommitSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Hash (ObjectId, objectIdFromHex)
import Git.Object.Commit
    ( commit,
      commitAuthor,
      commitCommitter,
      decodeCommit,
      commitFromObject,
      commitMessage,
      commitParents,
      commitToObject,
      commitTree,
      personIdent )
import TestSupport (assertEqual)

run :: IO ()
run = do
    treeId <- requireObjectId "4cf9f177c4c015836fca6a31f9c3917e89ae29ec"
    parentId <- requireObjectId "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a"
    let author = personIdent (BS8.pack "Ada Lovelace") (BS8.pack "ada@example.com") 1713220000 (BS8.pack "+0000")
        committer = personIdent (BS8.pack "Grace Hopper") (BS8.pack "grace@example.com") 1713220600 (BS8.pack "+0000")
        currentCommit = commit treeId [parentId] author committer (BS8.pack "Add commit plumbing\n")
    decodedCommit <- either fail pure (commitFromObject (commitToObject currentCommit))
    assertEqual treeId (commitTree decodedCommit) "Commit tree should round-trip"
    assertEqual [parentId] (commitParents decodedCommit) "Commit parents should round-trip"
    assertEqual author (commitAuthor decodedCommit) "Commit author should round-trip"
    assertEqual committer (commitCommitter decodedCommit) "Commit committer should round-trip"
    assertEqual (BS8.pack "Add commit plumbing\n") (commitMessage decodedCommit) "Commit message should round-trip"
    assertEqual
        (Left "Malformed commit: missing tree line")
        (decodeCommit
            (BS8.pack
                "author Ada Lovelace <ada@example.com> 1713220000 +0000\n\
                \committer Ada Lovelace <ada@example.com> 1713220000 +0000\n\n\
                \Missing tree"))
        "Commit decoder should reject commits without a tree line"

requireObjectId :: String -> IO ObjectId
requireObjectId hexText =
    maybe (fail ("invalid object id fixture: " ++ hexText)) pure (objectIdFromHex hexText)
