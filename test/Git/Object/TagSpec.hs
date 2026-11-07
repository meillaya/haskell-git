module Git.Object.TagSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Hash (ObjectId, objectIdFromHex)
import Git.Object (GitObjectType (..))
import Git.Object.Person (personIdent)
import Git.Object.Tag
    ( Tag (..)
    , decodeTag
    , encodeTag
    , tagFromObject
    , tagMessage
    , tagName
    , tagObject
    , tagObjectType
    , tagTagger
    , tagToObject
    )
import TestSupport (assertEqual)

-- | Check the annotated tag codec against the byte layout git writes and
-- reads.
run :: IO ()
run = do
    roundTrip
    exactBytes
    rejectsMissingHeaders

sampleTag :: ObjectId -> Tag
sampleTag targetId =
    Tag
        { tagObject = targetId
        , tagObjectType = CommitObject
        , tagName = BS8.pack "v1"
        , tagTagger = personIdent (BS8.pack "Ada") (BS8.pack "ada@example.com") 1713220000 (BS8.pack "+0000")
        , tagMessage = BS8.pack "Release v1\n"
        }

-- | Encode a tag and compare every field after decoding.
roundTrip :: IO ()
roundTrip = do
    targetId <- requireObjectId "4cf9f177c4c015836fca6a31f9c3917e89ae29ec"
    let sample = sampleTag targetId
    decoded <- either fail pure (decodeTag (encodeTag sample))
    assertEqual targetId (tagObject decoded) "Tag target should round-trip"
    assertEqual CommitObject (tagObjectType decoded) "Tag target type should round-trip"
    assertEqual (BS8.pack "v1") (tagName decoded) "Tag name should round-trip"
    assertEqual (tagTagger sample) (tagTagger decoded) "Tag tagger should round-trip"
    assertEqual (BS8.pack "Release v1\n") (tagMessage decoded) "Tag message should round-trip"
    wrapped <- either fail pure (tagFromObject (tagToObject decoded))
    assertEqual (tagName decoded) (tagName wrapped) "Tag object wrapper should round-trip"

-- | Compare the encoded tag with the byte layout git writes.
exactBytes :: IO ()
exactBytes = do
    targetId <- requireObjectId "4cf9f177c4c015836fca6a31f9c3917e89ae29ec"
    let expected =
            BS8.pack
                "object 4cf9f177c4c015836fca6a31f9c3917e89ae29ec\n\
                \type commit\n\
                \tag v1\n\
                \tagger Ada <ada@example.com> 1713220000 +0000\n\
                \\n\
                \Release v1\n"
    assertEqual expected (encodeTag (sampleTag targetId)) "Tag encoding should match git's layout"

-- | Decode a tag missing its type, name, and tagger.
rejectsMissingHeaders :: IO ()
rejectsMissingHeaders =
    case decodeTag (BS8.pack "object 4cf9f177c4c015836fca6a31f9c3917e89ae29ec\n\nmessage\n") of
        Left _ -> pure ()
        Right _ -> fail "A tag without type, name, and tagger headers should be rejected"

requireObjectId :: String -> IO ObjectId
requireObjectId hexText =
    maybe (fail ("invalid test object id: " ++ hexText)) pure (objectIdFromHex hexText)
