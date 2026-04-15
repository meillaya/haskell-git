module Git.PackSpec
    ( run
    ) where

import qualified Data.ByteString as BS
import Git.Pack
    ( PackObjectKind (PackBlob),
      packHeaderObjectCount,
      packHeaderVersion,
      packObjectKind,
      packObjectSize,
      parsePackHeader,
      parsePackObjectHeader )
import TestSupport (assertEqual)

run :: IO ()
run = do
    let packBytes = BS.pack [80, 65, 67, 75, 0, 0, 0, 2, 0, 0, 0, 1, 0x3c]
    (header, objectBytes) <- either fail pure (parsePackHeader packBytes)
    assertEqual 2 (packHeaderVersion header) "Pack version should decode"
    assertEqual 1 (packHeaderObjectCount header) "Pack object count should decode"
    (objectHeader, _) <- either fail pure (parsePackObjectHeader objectBytes)
    assertEqual PackBlob (packObjectKind objectHeader) "Pack object kind should decode"
    assertEqual 12 (packObjectSize objectHeader) "Pack object size should decode"
    assertEqual
        (Left "Unsupported packfile version")
        (parsePackHeader (BS.pack [80, 65, 67, 75, 0, 0, 0, 9, 0, 0, 0, 1]))
        "Pack parser should reject unsupported pack versions"
    assertEqual
        (Left "Malformed packfile: missing PACK signature")
        (parsePackHeader (BS.pack [80, 65, 88, 75, 0, 0, 0, 2, 0, 0, 0, 1]))
        "Pack parser should reject invalid pack signatures"
