module Git.Object.CommitSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Hash (ObjectId, objectIdFromHex)
import Git.Object.Commit
    ( commit,
      commitAuthor,
      commitCommitter,
      commitFromObject,
      commitMessage,
      commitParents,
      commitToObject,
      commitTree,
      decodeCommit,
      encodeCommit,
      personIdent,
      )
import TestSupport (assertEqual)

-- | Check the commit codec: parents, identities, messages with blank lines,
-- extra signature headers, and malformed metadata.
run :: IO ()
run = do
    roundTrip
    multipleParents
    blankLineInMessage
    ignoresExtraHeaders
    rejectsMalformedMetadata

-- | Encode a commit and compare every field after decoding.
roundTrip :: IO ()
roundTrip = do
    treeId <- requireObjectId "4cf9f177c4c015836fca6a31f9c3917e89ae29ec"
    parentId <- requireObjectId "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a"
    let author = personIdent (BS8.pack "Ada Lovelace") (BS8.pack "ada@example.com") 1713220000 (BS8.pack "+0000")
        committer = personIdent (BS8.pack "Grace Hopper") (BS8.pack "grace@example.com") 1713220600 (BS8.pack "-0500")
        currentCommit = commit treeId [parentId] author committer (BS8.pack "Add commit plumbing\n")
    decodedCommit <- either fail pure (commitFromObject (commitToObject currentCommit))
    assertEqual treeId (commitTree decodedCommit) "Commit tree should round-trip"
    assertEqual [parentId] (commitParents decodedCommit) "Commit parents should round-trip"
    assertEqual author (commitAuthor decodedCommit) "Commit author should round-trip"
    assertEqual committer (commitCommitter decodedCommit) "Commit committer should round-trip"
    assertEqual (BS8.pack "Add commit plumbing\n") (commitMessage decodedCommit) "Commit message should round-trip"

-- | Decode a merge commit and check both parents survive.
multipleParents :: IO ()
multipleParents = do
    treeId <- requireObjectId "4cf9f177c4c015836fca6a31f9c3917e89ae29ec"
    firstParent <- requireObjectId "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a"
    secondParent <- requireObjectId "ce013625030ba8dba906f756967f9e9ca394464a"
    let author = personIdent (BS8.pack "Ada") (BS8.pack "ada@example.com") 1713220000 (BS8.pack "+0000")
        decodedCommit = commit treeId [firstParent, secondParent] author author (BS8.pack "merge\n")
    decoded <- either fail pure (decodeCommit (encodeCommit decodedCommit))
    assertEqual [firstParent, secondParent] (commitParents decoded) "A merge commit should keep both parents"

-- | Round-trip a message that contains a blank line.
blankLineInMessage :: IO ()
blankLineInMessage = do
    treeId <- requireObjectId "4cf9f177c4c015836fca6a31f9c3917e89ae29ec"
    let author = personIdent (BS8.pack "Ada") (BS8.pack "ada@example.com") 1713220000 (BS8.pack "+0000")
        currentCommit = commit treeId [] author author (BS8.pack "subject\n\nbody line\n")
    decoded <- either fail pure (decodeCommit (encodeCommit currentCommit))
    assertEqual (BS8.pack "subject\n\nbody line\n") (commitMessage decoded) "A message with a blank line should round-trip"

-- | Decode a commit with signature and encoding headers.
ignoresExtraHeaders :: IO ()
ignoresExtraHeaders = do
    let rawCommit =
            BS8.pack
                "tree 4cf9f177c4c015836fca6a31f9c3917e89ae29ec\n\
                \parent 716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a\n\
                \author Ada <ada@example.com> 1713220000 +0000\n\
                \committer Ada <ada@example.com> 1713220000 +0000\n\
                \gpgsig -----BEGIN SSH SIGNATURE-----\n\
                \ U1NIU0lHAAAAAQAAADMAAAALc3NoLWVkMjU1MTkAAAAg\n\
                \ -----END SSH SIGNATURE-----\n\
                \encoding UTF-8\n\
                \\n\
                \signed message\n"
    decoded <- either fail pure (decodeCommit rawCommit)
    assertEqual (BS8.pack "signed message\n") (commitMessage decoded) "Signature headers should not disturb the message"

-- | Give the decoder a commit without a tree and one with a bad timezone.
rejectsMalformedMetadata :: IO ()
rejectsMalformedMetadata = do
    assertEqual
        (Left "Malformed commit: missing tree line")
        ( decodeCommit
            ( BS8.pack
                "author Ada Lovelace <ada@example.com> 1713220000 +0000\n\
                \committer Ada Lovelace <ada@example.com> 1713220000 +0000\n\n\
                \Missing tree"
            )
        )
        "Commit decoder should reject a commit without a tree line"
    case
        decodeCommit
            ( BS8.pack
                "tree 4cf9f177c4c015836fca6a31f9c3917e89ae29ec\n\
                \author Ada <ada@example.com> 1713220000 +000\n\
                \committer Ada <ada@example.com> 1713220000 +0000\n\n\
                \bad timezone"
            ) of
        Left _ -> pure ()
        Right _ -> fail "A malformed timezone should be rejected"

requireObjectId :: String -> IO ObjectId
requireObjectId hexText =
    maybe (fail ("invalid test object id: " ++ hexText)) pure (objectIdFromHex hexText)
