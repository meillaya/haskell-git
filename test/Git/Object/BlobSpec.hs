module Git.Object.BlobSpec
    ( run
    ) where

import Control.Exception (try)
import qualified Data.ByteString.Char8 as BS8
import Git.Hash (ObjectId, hashBytes, objectIdToHex)
import Git.Object (GitObject, decodeGitObject, encodeGitObject)
import Git.Object.Blob (blobContents, blobFromBytes, blobFromObject, blobToObject)
import Git.Object.Loose (readLooseObject, writeLooseObject)
import Git.Repo (initRepository, repositoryObjectsDir)
import System.FilePath ((</>))
import TestSupport (assert, assertContains, assertEqual, withTempDirectory)

run :: IO ()
run = do
    let contents = BS8.pack "blob payload"
        blob = blobFromBytes contents
        encoded = encodeGitObject (blobToObject blob)
        objectIdHex = objectIdToHex (hashBytes encoded)
    assert (length objectIdHex == 40) "SHA-1 object ids should render as 40 hex characters"
    decodedObject <- either fail pure (decodeGitObject encoded)
    decodedBlob <- either fail pure (blobFromObject decodedObject)
    assertEqual contents (blobContents decodedBlob) "Encoded blobs should decode to the same payload"
    assertEqual
        (Left "Malformed Git object: missing NUL separator")
        (decodeGitObject (BS8.pack "blob 4test"))
        "Object decoder should reject payloads without a NUL separator"
    assertEqual
        (Left "Malformed Git object: payload length does not match header")
        (decodeGitObject (BS8.pack "blob 4\0toolong"))
        "Object decoder should reject payloads whose size header is wrong"
    withTempDirectory "haskell-git-corrupt-blob" $ \temporaryDir -> do
        repository <- initRepository temporaryDir
        objectId <- writeLooseObject repository (blobToObject (blobFromBytes (BS8.pack "payload")))
        let (prefix, suffix) = splitAt 2 (objectIdHexFromWrite objectId)
            objectPath = repositoryObjectsDir repository </> prefix </> suffix
        writeFile objectPath "not-zlib"
        corruptReadResult <- try (readLooseObject repository objectId) :: IO (Either IOError GitObject)
        case corruptReadResult of
            Left ioErr ->
                assertContains
                    "Failed to decompress loose object"
                    (show ioErr)
                    "Corrupt loose objects should fail with a controlled decompression message"
            Right _ -> fail "Expected reading a corrupt loose object to fail"

objectIdHexFromWrite :: ObjectId -> String
objectIdHexFromWrite = objectIdToHex
