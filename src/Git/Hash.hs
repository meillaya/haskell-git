module Git.Hash
    ( HashAlgorithm (..)
    , ObjectId
    , defaultHashAlgorithm
    , hashAlgorithmFromName
    , hashAlgorithmName
    , hashBytes
    , hashBytesFor
    , hashPartsFor
    , hashSize
    , isZeroObjectId
    , objectIdBytes
    , objectIdFromBytes
    , objectIdFromBytesFor
    , objectIdFromHex
    , objectIdFromHexFor
    , objectIdLength
    , objectIdLengthFor
    , objectIdPath
    , objectIdToHex
    , zeroObjectIdFor
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (digitToInt, isHexDigit, toLower)
import Data.Word (Word8)
import Numeric (showHex)

-- | The object formats this implementation understands. Git identifies them
-- on the wire and in the repository config by these names.
data HashAlgorithm
    = Sha1
    | Sha256
    deriving (Eq, Ord, Show)

newtype ObjectId = ObjectId ByteString
    deriving (Eq, Ord, Show)

-- | The raw length of a SHA-1 object id. Kept for callers that predate the
-- multi-format API.
objectIdLength :: Int
objectIdLength = objectIdLengthFor Sha1

objectIdLengthFor :: HashAlgorithm -> Int
objectIdLengthFor Sha1 = 20
objectIdLengthFor Sha256 = 32

hashSize :: HashAlgorithm -> Int
hashSize = objectIdLengthFor

defaultHashAlgorithm :: HashAlgorithm
defaultHashAlgorithm = Sha1

hashAlgorithmName :: HashAlgorithm -> ByteString
hashAlgorithmName Sha1 = BS8.pack "sha1"
hashAlgorithmName Sha256 = BS8.pack "sha256"

hashAlgorithmFromName :: ByteString -> Maybe HashAlgorithm
hashAlgorithmFromName name
    | name == BS8.pack "sha1" = Just Sha1
    | name == BS8.pack "sha256" = Just Sha256
    | otherwise = Nothing

hashBytes :: ByteString -> ObjectId
hashBytes = hashBytesFor Sha1

hashBytesFor :: HashAlgorithm -> ByteString -> ObjectId
hashBytesFor Sha1 = ObjectId . SHA1.hash
hashBytesFor Sha256 = ObjectId . SHA256.hash

-- | Hash the concatenation of two byte strings without building the
-- intermediate copy. Object ids hash a small header followed by the payload,
-- so this avoids copying every object during clone.
hashPartsFor :: HashAlgorithm -> ByteString -> ByteString -> ObjectId
hashPartsFor Sha1 first second =
    ObjectId (SHA1.finalize (SHA1.update (SHA1.update SHA1.init first) second))
hashPartsFor Sha256 first second =
    ObjectId (SHA256.finalize (SHA256.update (SHA256.update SHA256.init first) second))

zeroObjectIdFor :: HashAlgorithm -> ObjectId
zeroObjectIdFor hashAlgorithm =
    ObjectId (BS.replicate (hashSize hashAlgorithm) 0)

-- | True for the all-zero id. Upload-pack uses it in the advertisement for an
-- empty repository.
isZeroObjectId :: ObjectId -> Bool
isZeroObjectId (ObjectId rawBytes) = BS.all (== 0) rawBytes

objectIdFromBytes :: ByteString -> Maybe ObjectId
objectIdFromBytes = objectIdFromBytesFor Sha1

objectIdFromBytesFor :: HashAlgorithm -> ByteString -> Maybe ObjectId
objectIdFromBytesFor hashAlgorithm rawBytes
    | BS.length rawBytes == hashSize hashAlgorithm = Just (ObjectId rawBytes)
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
    | length hexText == objectIdLengthFor Sha1 * 2 =
        objectIdFromHexFor Sha1 hexText
    | length hexText == objectIdLengthFor Sha256 * 2 =
        objectIdFromHexFor Sha256 hexText
    | otherwise = Nothing

objectIdFromHexFor :: HashAlgorithm -> String -> Maybe ObjectId
objectIdFromHexFor hashAlgorithm hexText
    | length hexText /= hashSize hashAlgorithm * 2 = Nothing
    | not (all isHexDigit hexText) = Nothing
    | otherwise = ObjectId . BS.pack <$> traverse parsePair (pairs hexText)
  where
    pairs :: [a] -> [(a, a)]
    pairs [] = []
    pairs (firstDigit:secondDigit:remaining) = (firstDigit, secondDigit) : pairs remaining
    pairs [_] = []

    parsePair :: (Char, Char) -> Maybe Word8
    parsePair (upperNibble, lowerNibble) =
        Just (fromIntegral (digitToInt upperNibble * 16 + digitToInt lowerNibble))

objectIdPath :: ObjectId -> (FilePath, FilePath)
objectIdPath objectId =
    let hexText = objectIdToHex objectId
     in splitAt 2 hexText
