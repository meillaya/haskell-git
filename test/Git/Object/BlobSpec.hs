module Git.Object.BlobSpec
    ( run
    ) where

import Control.Exception (try)
import qualified Data.ByteString.Char8 as BS8
import Git.Hash (hashBytes, objectIdToHex)
import Git.Object (GitObject, decodeGitObject, encodeGitObject)
import Git.Object.Blob (blobContents, blobFromBytes, blobFromObject, blobToObject)
import Git.Object.Loose (looseObjectPath, readLooseObject, writeLooseObject)
import Git.Repo (initRepository)
import TestSupport (assertContains, assertEqual, runBash, withTempDirectory)

-- | Check the blob codec, the strict object header checks, and the error path
-- for a corrupt loose object.
run :: IO ()
run = do
    roundTrip
    strictHeaders
    corruptLooseObject

-- | Encode a blob, decode it, and compare the payload.
roundTrip :: IO ()
roundTrip = do
    let contents = BS8.pack "blob payload"
        blob = blobFromBytes contents
        encoded = encodeGitObject (blobToObject blob)
        objectIdHex = objectIdToHex (hashBytes encoded)
    assertEqual 40 (length objectIdHex) "SHA-1 object ids should render as 40 hex characters"
    decodedObject <- either fail pure (decodeGitObject encoded)
    decodedBlob <- either fail pure (blobFromObject decodedObject)
    assertEqual contents (blobContents decodedBlob) "Encoded blobs should decode to the same payload"

-- | Give the decoder object headers git rejects.
strictHeaders :: IO ()
strictHeaders = do
    assertEqual
        (Left "Malformed Git object: missing NUL separator")
        (decodeGitObject (BS8.pack "blob 4test"))
        "Object decoder should reject payloads without a NUL separator"
    assertEqual
        (Left "Malformed Git object: payload length does not match header")
        (decodeGitObject (BS8.pack "blob 4\0toolong"))
        "Object decoder should reject payloads whose size header is wrong"
    assertEqual
        (Left "Malformed Git object: size has a leading zero")
        (decodeGitObject (BS8.pack "blob 04\0abcd"))
        "Object decoder should reject a size with a leading zero"
    assertEqual
        (Left "Malformed Git object: size is not decimal")
        (decodeGitObject (BS8.pack "blob +4\0abcd"))
        "Object decoder should reject a signed size"
    assertEqual
        (Left "Malformed Git object: unknown type")
        (decodeGitObject (BS8.pack "widget 0\0"))
        "Object decoder should reject an unknown type"

-- | Overwrite a loose object with non-zlib bytes and read it.
corruptLooseObject :: IO ()
corruptLooseObject =
    withTempDirectory "haskell-git-corrupt-blob" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        objectId <- writeLooseObject repository (blobToObject (blobFromBytes (BS8.pack "payload")))
        let objectPath = looseObjectPath repository objectId
        _ <- runBash ("chmod +w " ++ objectPath)
        writeFile objectPath "not-zlib"
        corruptReadResult <- try (readLooseObject repository objectId) :: IO (Either IOError GitObject)
        case corruptReadResult of
            Left ioErr ->
                assertContains
                    "Failed to decompress loose object"
                    (show ioErr)
                    "Corrupt loose objects should fail with a controlled decompression message"
            Right _ -> fail "Expected reading a corrupt loose object to fail"
