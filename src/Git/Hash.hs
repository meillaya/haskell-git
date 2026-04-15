module Git.Hash
    ( HashAlgorithm (..)
    , ObjectId
    , defaultHashAlgorithm
    , hashBytes
    , objectIdBytes
    , objectIdFromBytes
    , objectIdFromHex
    , objectIdPath
    , objectIdToHex
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isHexDigit, toLower)
import Data.Word (Word8)
import Numeric (showHex)

data HashAlgorithm
    = Sha1
    deriving (Eq, Ord, Show)

newtype ObjectId = ObjectId ByteString
    deriving (Eq, Ord, Show)

defaultHashAlgorithm :: HashAlgorithm
defaultHashAlgorithm = Sha1

hashBytes :: ByteString -> ObjectId
hashBytes = ObjectId . SHA1.hash

objectIdFromBytes :: ByteString -> Maybe ObjectId
objectIdFromBytes rawBytes
    | BS.length rawBytes == 20 = Just (ObjectId rawBytes)
    | otherwise = Nothing

objectIdBytes :: ObjectId -> ByteString
objectIdBytes (ObjectId rawBytes) = rawBytes

objectIdToHex :: ObjectId -> String
objectIdToHex (ObjectId rawBytes) = concatMap renderByte (BS.unpack rawBytes)
  where
    renderByte :: Word8 -> String
    renderByte byte =
        let rendered = showHex byte ""
         in case rendered of
                [singleDigit] -> ['0', singleDigit]
                digits -> map toLower digits

objectIdFromHex :: String -> Maybe ObjectId
objectIdFromHex hexText
    | length hexText /= 40 = Nothing
    | not (all isHexDigit hexText) = Nothing
    | otherwise = ObjectId . BS.pack <$> traverse parsePair (pairs hexText)
  where
    pairs :: [a] -> [(a, a)]
    pairs [] = []
    pairs (firstDigit:secondDigit:remaining) = (firstDigit, secondDigit) : pairs remaining
    pairs _ = []

    parsePair :: (Char, Char) -> Maybe Word8
    parsePair (upperNibble, lowerNibble)
        | all isHexDigit [upperNibble, lowerNibble] =
            Just (fromIntegral (digitToInt upperNibble * 16 + digitToInt lowerNibble))
        | otherwise = Nothing

objectIdPath :: ObjectId -> (FilePath, FilePath)
objectIdPath objectId =
    let hexText = objectIdToHex objectId
     in splitAt 2 hexText
