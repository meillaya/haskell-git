module Git.Object
    ( GitObject
    , GitObjectType (..)
    , decodeGitObject
    , encodeGitObject
    , gitObject
    , gitObjectPayload
    , gitObjectType
    , objectTypeFromName
    , objectTypeName
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Text.Read (readMaybe)

data GitObjectType
    = BlobObject
    | TreeObject
    | CommitObject
    | TagObject
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

objectTypeName :: GitObjectType -> String
objectTypeName BlobObject = "blob"
objectTypeName TreeObject = "tree"
objectTypeName CommitObject = "commit"
objectTypeName TagObject = "tag"

objectTypeFromName :: ByteString -> Maybe GitObjectType
objectTypeFromName name =
    case BS8.unpack name of
        "blob" -> Just BlobObject
        "tree" -> Just TreeObject
        "commit" -> Just CommitObject
        "tag" -> Just TagObject
        _ -> Nothing

encodeGitObject :: GitObject -> ByteString
encodeGitObject currentObject =
    let payload = gitObjectPayload currentObject
        header =
            BS8.pack
                (objectTypeName (gitObjectType currentObject) ++ " " ++ show (BS.length payload))
     in header <> BS.singleton 0 <> payload

decodeGitObject :: ByteString -> Either String GitObject
decodeGitObject rawBytes = do
    let (header, payloadWithNull) = BS.break (== 0) rawBytes
    if BS.null payloadWithNull
        then Left "Malformed Git object: missing NUL separator"
        else do
            let payload = BS.tail payloadWithNull
            (typeName, sizeText) <- parseHeader header
            objectTypeValue <- parseType typeName
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
            "tag" -> Right TagObject
            _ -> Left "Malformed Git object: unknown type"

    -- Git writes the size in decimal with no sign and no leading zero. The
    -- strict form matters: a malicious object store can otherwise smuggle in
    -- ids that hash differently from what git would accept.
    parseSize sizeText
        | null sizeText = Left "Malformed Git object: empty size"
        | not (all isDigit sizeText) = Left "Malformed Git object: size is not decimal"
        | length sizeText > 1 && head sizeText == '0' =
            Left "Malformed Git object: size has a leading zero"
        | otherwise =
            maybe
                (Left "Malformed Git object: size overflows an integer")
                Right
                (readMaybe sizeText)
