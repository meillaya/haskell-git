module Git.PackSpec
    ( run
    ) where

import qualified Data.ByteString as BS
import Git.Pack
    ( PackObjectKind (..),
      packHeaderObjectCount,
      packHeaderVersion,
      packObjectKind,
      packObjectSize,
      parsePackHeader,
      parsePackObjectHeader,
      )
import TestSupport (assertEqual)

-- | Check pack file and object header parsing, including the multi-byte size
-- encoding and the delta kinds.
run :: IO ()
run = do
    header
    objectHeaders
    rejectsBadHeaders

-- | Parse a hand-built pack header and first object header.
header :: IO ()
header = do
    let packBytes = BS.pack [80, 65, 67, 75, 0, 0, 0, 2, 0, 0, 0, 1, 0x3c]
    (parsedHeader, objectBytes) <- either fail pure (parsePackHeader packBytes)
    assertEqual 2 (packHeaderVersion parsedHeader) "Pack version should decode"
    assertEqual 1 (packHeaderObjectCount parsedHeader) "Pack object count should decode"
    (objectHeader, _) <- either fail pure (parsePackObjectHeader objectBytes)
    assertEqual PackBlob (packObjectKind objectHeader) "Pack object kind should decode"
    assertEqual 12 (packObjectSize objectHeader) "Pack object size should decode"

-- | Parse blob, delta, and multi-byte object headers.
objectHeaders :: IO ()
objectHeaders = do
    -- Blob, size 12: 0x3c = kind 3, size 12.
    (blobHeader, _) <- either fail pure (parsePackObjectHeader (BS.pack [0x3c]))
    assertEqual PackBlob (packObjectKind blobHeader) "A blob header should decode"
    assertEqual 12 (packObjectSize blobHeader) "A blob size should decode"
    -- Offset delta, size 1: 0x61 = kind 6, size 1.
    (offsetDeltaHeader, _) <- either fail pure (parsePackObjectHeader (BS.pack [0x61]))
    assertEqual PackOffsetDelta (packObjectKind offsetDeltaHeader) "An offset delta header should decode"
    -- Reference delta, size 2: 0x72 = kind 7, size 2.
    (referenceDeltaHeader, _) <- either fail pure (parsePackObjectHeader (BS.pack [0x72]))
    assertEqual PackReferenceDelta (packObjectKind referenceDeltaHeader) "A reference delta header should decode"
    -- Multi-byte size: 200 = 0x0c8 -> low nibble 8, continuation 0x0c.
    (largeHeader, _) <- either fail pure (parsePackObjectHeader (BS.pack [0xb8, 0x0c]))
    assertEqual PackBlob (packObjectKind largeHeader) "A multi-byte header should keep its kind"
    assertEqual 200 (packObjectSize largeHeader) "A multi-byte header should decode its size"

-- | Give the parsers bad versions, signatures, and kinds.
rejectsBadHeaders :: IO ()
rejectsBadHeaders = do
    assertEqual
        (Left "Unsupported packfile version")
        (parsePackHeader (BS.pack [80, 65, 67, 75, 0, 0, 0, 9, 0, 0, 0, 1]))
        "Pack parser should reject unsupported pack versions"
    assertEqual
        (Left "Malformed packfile: missing PACK signature")
        (parsePackHeader (BS.pack [80, 65, 88, 75, 0, 0, 0, 2, 0, 0, 0, 1]))
        "Pack parser should reject invalid pack signatures"
    case parsePackObjectHeader (BS.pack [0x01]) of
        Left _ -> pure ()
        Right _ -> fail "A reserved object kind should be rejected"
