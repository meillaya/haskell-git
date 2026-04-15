module Git.Object.Blob
    ( Blob
    , blobContents
    , blobFromObject
    , blobFromBytes
    , blobToObject
    ) where

import Data.ByteString (ByteString)
import Git.Object (GitObject, GitObjectType (BlobObject), gitObject, gitObjectPayload, gitObjectType)

newtype Blob = Blob ByteString
    deriving (Eq, Show)

blobFromBytes :: ByteString -> Blob
blobFromBytes = Blob

blobContents :: Blob -> ByteString
blobContents (Blob bytes) = bytes

blobToObject :: Blob -> GitObject
blobToObject = gitObject BlobObject . blobContents

blobFromObject :: GitObject -> Either String Blob
blobFromObject currentObject =
    if gitObjectType currentObject == BlobObject
        then Right (blobFromBytes (gitObjectPayload currentObject))
        else Left "Expected a blob object"
