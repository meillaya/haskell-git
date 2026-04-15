module Git.Pack
    ( PackHeader
    , PackObjectHeader
    , PackObjectKind (..)
    , packHeaderObjectCount
    , packHeaderVersion
    , packObjectKind
    , packObjectSize
    , parsePackHeader
    , parsePackObjectHeader
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

data PackObjectKind
    = PackCommit
    | PackTree
    | PackBlob
    | PackTag
    | PackOffsetDelta
    | PackReferenceDelta
    deriving (Eq, Show)

data PackHeader = PackHeader
    { packHeaderVersion :: Int
    , packHeaderObjectCount :: Int
    }
    deriving (Eq, Show)

data PackObjectHeader = PackObjectHeader
    { packObjectKind :: PackObjectKind
    , packObjectSize :: Int
    }
    deriving (Eq, Show)

parsePackHeader :: ByteString -> Either String (PackHeader, ByteString)
parsePackHeader rawBytes
    | BS.length rawBytes < 12 = Left "Malformed packfile: truncated header"
    | BS.take 4 rawBytes /= BS.pack [80, 65, 67, 75] = Left "Malformed packfile: missing PACK signature"
    | otherwise =
        let version = fromIntegral (decodeWord32 (BS.take 4 (BS.drop 4 rawBytes)))
            objectCount = fromIntegral (decodeWord32 (BS.take 4 (BS.drop 8 rawBytes)))
         in if version `elem` [2, 3]
                then Right (PackHeader version objectCount, BS.drop 12 rawBytes)
                else Left "Unsupported packfile version"

parsePackObjectHeader :: ByteString -> Either String (PackObjectHeader, ByteString)
parsePackObjectHeader rawBytes =
    case BS.uncons rawBytes of
        Nothing -> Left "Malformed packfile: missing object header"
        Just (firstByte, rest) ->
            let kindCode = (firstByte `shiftR` 4) .&. 0x07
             in parseSize kindCode firstByte rest 4 (fromIntegral (firstByte .&. 0x0f))
  where
    parseSize kindCode currentByte remaining shift currentSize
        | testBit currentByte 7 =
            case BS.uncons remaining of
                Nothing -> Left "Malformed packfile: truncated object header"
                Just (nextByte, rest) ->
                    parseSize
                        kindCode
                        nextByte
                        rest
                        (shift + 7)
                        (currentSize .|. (fromIntegral (nextByte .&. 0x7f) `shiftL` shift))
        | otherwise = do
            objectKindValue <- decodeKind kindCode
            Right (PackObjectHeader objectKindValue currentSize, remaining)

    decodeKind kindCode =
        case kindCode of
            1 -> Right PackCommit
            2 -> Right PackTree
            3 -> Right PackBlob
            4 -> Right PackTag
            6 -> Right PackOffsetDelta
            7 -> Right PackReferenceDelta
            _ -> Left "Unsupported pack object kind"

decodeWord32 :: ByteString -> Word32
decodeWord32 = BS.foldl' (\acc word -> (acc `shiftL` 8) .|. fromIntegral word) 0
