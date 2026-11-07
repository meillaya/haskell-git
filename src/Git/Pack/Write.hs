module Git.Pack.Write
    ( writePackFile
    ) where

import qualified Codec.Compression.Zlib as Zlib
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8)
import System.IO (Handle, IOMode (WriteMode), hClose, openBinaryFile)
import Git.Hash (HashAlgorithm (..))
import Git.Object (GitObject, GitObjectType (..), gitObjectPayload, gitObjectType)

data PackHasher
    = Sha1Hasher SHA1.Ctx
    | Sha256Hasher SHA256.Ctx

-- | Write a version 2 pack containing the given full objects. No deltas are
-- emitted; this keeps the writer simple and is valid for push.
writePackFile :: HashAlgorithm -> [GitObject] -> FilePath -> IO ()
writePackFile objectFormat objects path = do
    handle <- openBinaryFile path WriteMode
    hasher <- pure (newHasher objectFormat)
    finalHasher <- do
        hasherAfterHeader <- writeHashed handle hasher (packHeader objectFormat (length objects))
        foldMObjects handle hasherAfterHeader objects
    BS.hPut handle (finalizeHasher finalHasher)
    hClose handle

packHeader :: HashAlgorithm -> Int -> ByteString
packHeader objectFormat count =
    BS8.pack "PACK"
        <> encodeWord32 2
        <> encodeWord32 (fromIntegral count)

foldMObjects handle hasher [] = pure hasher
foldMObjects handle hasher (gitObjectValue : rest) = do
    let payload = gitObjectPayload gitObjectValue
        entry =
            objectHeader (gitObjectType gitObjectValue) (BS.length payload)
                <> BL.toStrict (Zlib.compress (BL.fromStrict payload))
    nextHasher <- writeHashed handle hasher entry
    foldMObjects handle nextHasher rest

writeHashed :: Handle -> PackHasher -> ByteString -> IO PackHasher
writeHashed handle hasher bytes = do
    BS.hPut handle bytes
    pure (updateHasher hasher bytes)

objectHeader :: GitObjectType -> Int -> ByteString
objectHeader objectType payloadSize =
    encodeSize (kindCode objectType) (fromIntegral payloadSize)

kindCode :: GitObjectType -> Word8
kindCode BlobObject = 3
kindCode TreeObject = 2
kindCode CommitObject = 1
kindCode TagObject = 4

encodeSize :: Word8 -> Int -> ByteString
encodeSize kindCodeValue size =
    case go (size `shiftR` 4) [] of
        [] -> BS.singleton firstByte
        restBytes -> BS.pack (firstByte : restBytes)
  where
    firstByte =
        (kindCodeValue `shiftL` 4)
            .|. fromIntegral (size .&. 0x0f)
            .|. (if size `shiftR` 4 == 0 then 0 else 0x80)
    go 0 accumulated = reverse accumulated
    go remaining accumulated =
        let byte = fromIntegral (remaining .&. 0x7f)
            rest = remaining `shiftR` 7
         in go rest ((byte .|. (if rest == 0 then 0 else 0x80)) : accumulated)

encodeWord32 :: Int -> ByteString
encodeWord32 value =
    BS.pack
        [ fromIntegral ((value `shiftR` 24) .&. 0xff)
        , fromIntegral ((value `shiftR` 16) .&. 0xff)
        , fromIntegral ((value `shiftR` 8) .&. 0xff)
        , fromIntegral (value .&. 0xff)
        ]

newHasher :: HashAlgorithm -> PackHasher
newHasher Sha1 = Sha1Hasher SHA1.init
newHasher Sha256 = Sha256Hasher SHA256.init

updateHasher :: PackHasher -> ByteString -> PackHasher
updateHasher (Sha1Hasher context) bytes = Sha1Hasher (SHA1.update context bytes)
updateHasher (Sha256Hasher context) bytes = Sha256Hasher (SHA256.update context bytes)

finalizeHasher :: PackHasher -> ByteString
finalizeHasher (Sha1Hasher context) = SHA1.finalize context
finalizeHasher (Sha256Hasher context) = SHA256.finalize context
