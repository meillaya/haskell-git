module Git.Pack.Read
    ( PackRawObject (..)
    , decodeObjectHeader
    , decodeOffsetDeltaBase
    , decompressMember
    , parsePackObjectAt
    , parsePackObjectAtFor
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.Zlib.Internal as ZlibInternal
import Data.Word (Word8)
import Git.Hash (HashAlgorithm (..), ObjectId, objectIdFromBytesFor, objectIdLengthFor)
import Git.Pack (PackObjectKind (..), packObjectKind)

-- | One parsed entry from a pack file. A delta entry stores the base reference
-- and the raw delta instructions; resolving it needs the object store.
data PackRawObject
    = PackRawFull PackObjectKind ByteString
    | PackRawOffsetDelta Int ByteString
    | PackRawRefDelta ObjectId ByteString
    deriving (Eq, Show)

-- | Parse the object that starts at a byte offset. The returned count is the
-- total number of bytes the entry occupies, header and compressed stream
-- included, so a caller walking a pack can advance past it.
parsePackObjectAt :: ByteString -> Int -> Either String (PackRawObject, Int)
parsePackObjectAt = parsePackObjectAtFor Sha1

parsePackObjectAtFor :: HashAlgorithm -> ByteString -> Int -> Either String (PackRawObject, Int)
parsePackObjectAtFor hashAlgorithm bytes offset
    | offset < 0 || offset >= BS.length bytes =
        Left "malformed pack: object offset is outside the file"
    | otherwise = do
        (objectKind, declaredSize, headerLength) <- decodeObjectHeader bytes offset
        let bodyStart = offset + headerLength
            objectIdLength = objectIdLengthFor hashAlgorithm
        case objectKind of
            PackCommit -> parseFull PackCommit bodyStart declaredSize
            PackTree -> parseFull PackTree bodyStart declaredSize
            PackBlob -> parseFull PackBlob bodyStart declaredSize
            PackTag -> parseFull PackTag bodyStart declaredSize
            PackOffsetDelta -> do
                (baseOffset, deltaStart) <- decodeOffsetDeltaBase bytes bodyStart
                if baseOffset <= 0 || baseOffset >= offset
                    then Left "malformed pack: offset delta base is not before the delta"
                    else do
                        (delta, consumed) <- decompressMember bytes deltaStart
                        if BS.length delta /= declaredSize
                            then Left "malformed pack: delta payload size does not match its header"
                            else
                                Right
                                    ( PackRawOffsetDelta baseOffset delta
                                    , deltaStart + consumed - offset
                                    )
            PackReferenceDelta -> do
                if BS.length bytes < bodyStart + objectIdLength
                    then Left "malformed pack: truncated reference delta base id"
                    else pure ()
                baseObjectId <-
                    maybe
                        (Left "malformed pack: invalid reference delta base id")
                        Right
                        ( objectIdFromBytesFor
                            hashAlgorithm
                            (BS.take objectIdLength (BS.drop bodyStart bytes))
                        )
                let deltaStart = bodyStart + objectIdLength
                (delta, consumed) <- decompressMember bytes deltaStart
                if BS.length delta /= declaredSize
                    then Left "malformed pack: delta payload size does not match its header"
                    else
                        Right
                            ( PackRawRefDelta baseObjectId delta
                            , deltaStart + consumed - offset
                            )
  where
    parseFull kind bodyStart declaredSize = do
        (payload, consumed) <- decompressMember bytes bodyStart
        if BS.length payload /= declaredSize
            then Left "malformed pack: object payload size does not match its header"
            else Right (PackRawFull kind payload, bodyStart + consumed - offset)

-- | Decode the variable-length object header. Returns the kind, the declared
-- uncompressed size, and the number of header bytes.
decodeObjectHeader :: ByteString -> Int -> Either String (PackObjectKind, Int, Int)
decodeObjectHeader bytes offset = do
    firstByte <- byteAt bytes offset
    let kindCode = (firstByte `shiftR` 4) .&. 0x07
        initialSize = fromIntegral (firstByte .&. 0x0f)
    (size, headerLength) <- go initialSize 4 1 firstByte
    objectKind <-
        maybe
            (Left "malformed pack: unknown object kind")
            Right
            (kindForCode kindCode)
    if size > maximumObjectSize
        then Left "malformed pack: object size is unreasonably large"
        else Right (objectKind, size, headerLength)
  where
    go size shift headerLength currentByte
        | headerLength > maximumHeaderLength =
            Left "malformed pack: object header is too long"
        | testBit currentByte 7 = do
            nextByte <- byteAt bytes (offset + headerLength)
            go
                (size .|. (fromIntegral (nextByte .&. 0x7f) `shiftL` shift))
                (shift + 7)
                (headerLength + 1)
                nextByte
        | otherwise = Right (size, headerLength)

    kindForCode 1 = Just PackCommit
    kindForCode 2 = Just PackTree
    kindForCode 3 = Just PackBlob
    kindForCode 4 = Just PackTag
    kindForCode 6 = Just PackOffsetDelta
    kindForCode 7 = Just PackReferenceDelta
    kindForCode _ = Just `seq` Nothing

decodeOffsetDeltaBase :: ByteString -> Int -> Either String (Int, Int)
decodeOffsetDeltaBase bytes position = do
    firstByte <- byteAt bytes position
    go
        (fromIntegral (firstByte .&. 0x7f))
        (position + 1)
        1
        firstByte
  where
    go accumulatedOffset nextPosition lengthSoFar currentByte
        | lengthSoFar > maximumHeaderLength =
            Left "malformed pack: offset base header is too long"
        | testBit currentByte 7 = do
            nextByte <- byteAt bytes nextPosition
            go
                (((accumulatedOffset + 1) `shiftL` 7) .|. fromIntegral (nextByte .&. 0x7f))
                (nextPosition + 1)
                (lengthSoFar + 1)
                nextByte
        | otherwise = Right (accumulatedOffset, nextPosition)

-- | Inflate one zlib member from a byte string. The second value is the number
-- of compressed bytes consumed, which is where the next pack entry begins.
decompressMember :: ByteString -> Int -> Either String (ByteString, Int)
decompressMember bytes start
    | start < 0 || start >= BS.length bytes = Left "malformed pack: object data is outside the file"
    | otherwise =
        case
            ZlibInternal.foldDecompressStreamWithInput
                ( \chunk continuation ->
                    case continuation of
                        Left err -> Left err
                        Right (chunks, leftover) -> Right (chunk : chunks, leftover)
                )
                (\leftover -> Right ([], BL.toStrict leftover))
                (Left . show)
                ( ZlibInternal.decompressST
                    ZlibInternal.zlibFormat
                    ZlibInternal.defaultDecompressParams {ZlibInternal.decompressAllMembers = False}
                )
                (BL.fromStrict (BS.drop start bytes)) of
            Left err -> Left ("failed to decode pack member: " ++ err)
            Right (chunks, leftover) ->
                Right
                    ( BS.concat chunks
                    , BS.length (BS.drop start bytes) - BS.length leftover
                    )

byteAt :: ByteString -> Int -> Either String Word8
byteAt bytes index
    | index < 0 || index >= BS.length bytes = Left "malformed pack: truncated object"
    | otherwise = Right (BS.index bytes index)

maximumHeaderLength :: Int
maximumHeaderLength = 10

-- A delta or object body larger than this is out of scope for a tool that
-- keeps the whole pack in memory anyway.
maximumObjectSize :: Int
maximumObjectSize = 2 * 1024 * 1024 * 1024
