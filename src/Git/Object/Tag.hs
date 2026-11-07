module Git.Object.Tag
    ( Tag (..)
    , decodeTag
    , encodeTag
    , tagFromObject
    , tagMessage
    , tagName
    , tagObject
    , tagObjectType
    , tagTagger
    , tagToObject
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Object
    ( GitObject
    , GitObjectType (TagObject)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    , objectTypeFromName
    , objectTypeName
    )
import Git.Object.Person (PersonIdent, parsePersonIdent, renderPersonIdent)

data Tag = Tag
    { tagObject :: ObjectId
    , tagObjectType :: GitObjectType
    , tagName :: ByteString
    , tagTagger :: PersonIdent
    , tagMessage :: ByteString
    }
    deriving (Eq, Show)

encodeTag :: Tag -> ByteString
encodeTag currentTag =
    BS.intercalate
        (BS8.pack "\n")
        ( [ BS8.pack ("object " ++ objectIdToHex (tagObject currentTag))
          , BS8.pack ("type " ++ objectTypeName (tagObjectType currentTag))
          , BS8.pack "tag " <> tagName currentTag
          , BS8.pack "tagger " <> renderPersonIdent (tagTagger currentTag)
          , BS.empty
          , tagMessage currentTag
          ]
        )

decodeTag :: ByteString -> Either String Tag
decodeTag rawTag = do
    let (headerPart, messagePartWithSeparator) = BS8.breakSubstring (BS8.pack "\n\n") rawTag
    if BS.null messagePartWithSeparator
        then Left "Malformed tag: missing header/message separator"
        else do
            let messagePart = BS.drop 2 messagePartWithSeparator
                headerLines = BS8.split '\n' headerPart
            targetId <- parseObjectLine headerLines
            targetType <- parseTypeLine headerLines
            name <- parseNameLine headerLines
            tagger <- parseTaggerLine headerLines
            Right (Tag targetId targetType name tagger messagePart)
  where
    headerValue label headerLines =
        case filter (BS8.isPrefixOf (BS8.pack (label ++ " "))) headerLines of
            [line] -> Right (BS.drop (length label + 1) line)
            [] -> Left ("Malformed tag: missing " ++ label ++ " line")
            _ -> Left ("Malformed tag: duplicate " ++ label ++ " line")

    parseObjectLine headerLines = do
        value <- headerValue "object" headerLines
        maybe
            (Left "Malformed tag: invalid object id")
            Right
            (objectIdFromHex (BS8.unpack value))

    parseTypeLine headerLines = do
        value <- headerValue "type" headerLines
        maybe
            (Left "Malformed tag: unknown target type")
            Right
            (objectTypeFromName value)

    parseNameLine headerLines = do
        value <- headerValue "tag" headerLines
        if BS.null value
            then Left "Malformed tag: empty tag name"
            else Right value

    parseTaggerLine headerLines = do
        value <- headerValue "tagger" headerLines
        case parsePersonIdent value of
            Left err -> Left ("Malformed tag: " ++ err)
            Right ident -> Right ident

tagToObject :: Tag -> GitObject
tagToObject = gitObject TagObject . encodeTag

tagFromObject :: GitObject -> Either String Tag
tagFromObject currentObject =
    if gitObjectType currentObject == TagObject
        then decodeTag (gitObjectPayload currentObject)
        else Left "Expected a tag object"
