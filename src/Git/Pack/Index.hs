module Git.Pack.Index
    ( PackIndex
    , PackIndexEntry (..)
    , lookupPackIndexOffset
    , packIndexEntries
    , packIndexObjectIds
    , parsePackIndex
    ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import Git.Hash
    ( ObjectId
    , hashBytes
    , objectIdBytes
    , objectIdFromBytes
    , objectIdLength
    )

data PackIndexEntry = PackIndexEntry
    { packIndexEntryObjectId :: ObjectId
    , packIndexEntryOffset :: Int
    , packIndexEntryCrc32 :: Word32
    }
    deriving (Eq, Show)

data PackIndex = PackIndex
    { packIndexEntries :: [PackIndexEntry]
    , packIndexOffsetByObjectId :: Map.Map ObjectId Int
    }

packIndexObjectIds :: PackIndex -> [ObjectId]
packIndexObjectIds = map packIndexEntryObjectId . packIndexEntries

lookupPackIndexOffset :: PackIndex -> ObjectId -> Maybe Int
lookupPackIndexOffset packIndex objectId =
    Map.lookup objectId (packIndexOffsetByObjectId packIndex)

-- | Parse a version 2 pack index. Both trailer checksums are verified, so a
-- truncated or edited index cannot silently misdirect object lookups.
parsePackIndex :: ByteString -> Either String PackIndex
parsePackIndex bytes = do
    if BS.length bytes < minimumLength
        then Left "malformed pack index: file is too short"
        else pure ()
    if BS.take 4 bytes /= packIndexMagic
        then Left "malformed pack index: bad signature"
        else pure ()

    -- Layout: fanout table, sorted object ids, CRCs, 32-bit offsets, the
    -- 64-bit offsets the 32-bit table cannot hold, then two checksums.
    let version = decodeWord32 (BS.take 4 (BS.drop 4 bytes))
        count =
            fromIntegral
                (decodeWord32 (BS.take 4 (BS.drop (fanoutOffset + (fanoutEntryCount - 1) * 4) bytes)))
        objectIdStart = fanoutOffset + fanoutEntryCount * 4
        crcStart = objectIdStart + count * objectIdLength
        offsetStart = crcStart + count * 4
        largeOffsetStart = offsetStart + count * 4
    if version /= supportedVersion
        then Left ("unsupported pack index version: " ++ show version)
        else pure ()
    if BS.length bytes < largeOffsetStart + checksumPairLength
        then Left "malformed pack index: truncated object tables"
        else pure ()

    let rawOffsets =
            [ decodeWord32 (BS.take 4 (BS.drop (offsetStart + index * 4) bytes))
            | index <- [0 .. count - 1]
            ]
        largeOffsetCount = length (filter isLargeOffset rawOffsets)
        trailerStart = largeOffsetStart + largeOffsetCount * 8
        contentLength = trailerStart + objectIdLength
    if BS.length bytes < trailerStart + checksumPairLength
        then Left "malformed pack index: truncated large-offset table"
        else pure ()
    if objectIdBytes (hashBytes (BS.take contentLength bytes)) /= BS.drop contentLength bytes
        then Left "malformed pack index: checksum mismatch"
        else pure ()

    let buildEntry index = do
            let rawOffset = rawOffsets !! index
                objectIdPosition = objectIdStart + index * objectIdLength
                crcPosition = crcStart + index * 4
            objectId <-
                maybe
                    (Left "malformed pack index: invalid object id")
                    Right
                    (objectIdFromBytes (BS.take objectIdLength (BS.drop objectIdPosition bytes)))
            offset <- resolveOffset bytes largeOffsetCount largeOffsetStart rawOffset
            Right
                PackIndexEntry
                    { packIndexEntryObjectId = objectId
                    , packIndexEntryOffset = offset
                    , packIndexEntryCrc32 = decodeWord32 (BS.take 4 (BS.drop crcPosition bytes))
                    }
    entries <- traverse buildEntry [0 .. count - 1]
    Right
        PackIndex
            { packIndexEntries = entries
            , packIndexOffsetByObjectId =
                Map.fromList
                    [ (packIndexEntryObjectId entry, packIndexEntryOffset entry)
                    | entry <- entries
                    ]
            }

resolveOffset :: ByteString -> Int -> Int -> Word32 -> Either String Int
resolveOffset bytes largeOffsetCount largeOffsetStart rawOffset
    | not (isLargeOffset rawOffset) = Right (fromIntegral rawOffset)
    | largeIndex >= largeOffsetCount =
        Left "malformed pack index: large offset out of range"
    | otherwise =
        Right
            ( fromIntegral
                (decodeWord64 (BS.take 8 (BS.drop (largeOffsetStart + largeIndex * 8) bytes)))
            )
  where
    largeIndex = fromIntegral (rawOffset .&. 0x7fffffff)

isLargeOffset :: Word32 -> Bool
isLargeOffset rawOffset = rawOffset .&. largeOffsetFlag /= 0

decodeWord32 :: ByteString -> Word32
decodeWord32 = BS.foldl' (\accumulator word -> (accumulator `shiftL` 8) .|. fromIntegral word) 0

decodeWord64 :: ByteString -> Word64
decodeWord64 = BS.foldl' (\accumulator word -> (accumulator `shiftL` 8) .|. fromIntegral word) 0

packIndexMagic :: ByteString
packIndexMagic = BS.pack [0xff, 0x74, 0x4f, 0x63]

fanoutOffset :: Int
fanoutOffset = 8

fanoutEntryCount :: Int
fanoutEntryCount = 256

supportedVersion :: Word32
supportedVersion = 2

largeOffsetFlag :: Word32
largeOffsetFlag = 0x80000000

minimumLength :: Int
minimumLength = fanoutOffset + fanoutEntryCount * 4 + checksumPairLength

checksumPairLength :: Int
checksumPairLength = objectIdLength * 2
