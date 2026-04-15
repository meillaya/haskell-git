module Git.Object
    ( GitObject
    , GitObjectType (..)
    , decodeGitObject
    , encodeGitObject
    , gitObject
    , gitObjectPayload
    , gitObjectType
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Text.Read (readMaybe)

data GitObjectType
    = BlobObject
    | TreeObject
    | CommitObject
    deriving (Eq, Show)

data GitObject = GitObject
    { objectType :: GitObjectType
    , objectPayload :: ByteString
    }
    deriving (Eq, Show)

gitObject :: GitObjectType -> ByteString -> GitObject
gitObject = GitObject

gitObjectType :: GitObject -> GitObjectType
gitObjectType = objectType

gitObjectPayload :: GitObject -> ByteString
gitObjectPayload = objectPayload

encodeGitObject :: GitObject -> ByteString
encodeGitObject currentObject =
    let payload = gitObjectPayload currentObject
        header =
            BS8.pack
                (renderType (gitObjectType currentObject) ++ " " ++ show (BS.length payload))
     in header <> BS.singleton 0 <> payload

decodeGitObject :: ByteString -> Either String GitObject
decodeGitObject rawBytes = do
    let (header, payloadWithNull) = BS.break (== 0) rawBytes
    if BS.null payloadWithNull
        then Left "Malformed Git object: missing NUL separator"
        else do
            let payload = BS.tail payloadWithNull
            (objectTypeName, sizeText) <- parseHeader header
            objectTypeValue <- parseType objectTypeName
            expectedSize <- parseSize sizeText
            if BS.length payload /= expectedSize
                then Left "Malformed Git object: payload length does not match header"
                else Right (gitObject objectTypeValue payload)
  where
    parseHeader headerBytes =
        let (typeName, sizeBytes) = BS8.break (== ' ') headerBytes
         in if BS.null sizeBytes
                then Left "Malformed Git object header"
                else Right (typeName, BS8.unpack (BS.tail sizeBytes))

    parseType typeName =
        case BS8.unpack typeName of
            "blob" -> Right BlobObject
            "tree" -> Right TreeObject
            "commit" -> Right CommitObject
            _ -> Left "Unsupported Git object type"

    parseSize sizeText =
        maybe (Left "Malformed Git object size") Right (readMaybe sizeText)

renderType :: GitObjectType -> String
renderType BlobObject = "blob"
renderType TreeObject = "tree"
renderType CommitObject = "commit"
