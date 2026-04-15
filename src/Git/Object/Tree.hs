module Git.Object.Tree
    ( TreeEntry
    , TreeEntryMode (..)
    , decodeTree
    , encodeTree
    , renderTreeEntry
    , treeEntry
    , treeEntryName
    , treeEntryObjectId
    , treeEntryMode
    , treeFromObject
    , treeToObject
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import Git.Hash (ObjectId, objectIdBytes, objectIdFromBytes, objectIdToHex)
import Git.Object (GitObject, GitObjectType (TreeObject), gitObject, gitObjectPayload, gitObjectType)

data TreeEntryMode
    = RegularFile
    deriving (Eq, Show)

data TreeEntry = TreeEntry
    { treeEntryMode :: TreeEntryMode
    , treeEntryName :: ByteString
    , treeEntryObjectId :: ObjectId
    }
    deriving (Eq, Show)

treeEntry :: TreeEntryMode -> ByteString -> ObjectId -> TreeEntry
treeEntry = TreeEntry

encodeTree :: [TreeEntry] -> ByteString
encodeTree entries = BS.concat (map encodeEntry (sortOn treeEntryName entries))
  where
    encodeEntry currentEntry =
        renderMode (treeEntryMode currentEntry)
            <> BS8.pack " "
            <> treeEntryName currentEntry
            <> BS.singleton 0
            <> objectIdBytes (treeEntryObjectId currentEntry)

decodeTree :: ByteString -> Either String [TreeEntry]
decodeTree rawTree = go rawTree []
  where
    go bytes entries
        | BS.null bytes = Right (reverse entries)
        | otherwise = do
            let (modeBytes, afterMode) = BS8.break (== ' ') bytes
            if BS.null afterMode
                then Left "Malformed tree entry: missing mode separator"
                else do
                    mode <- parseMode modeBytes
                    let (nameBytes, afterName) = BS.break (== 0) (BS.tail afterMode)
                    if BS.null afterName
                        then Left "Malformed tree entry: missing NUL separator"
                        else do
                            let objectIdBytesRaw = BS.take 20 (BS.tail afterName)
                                remaining = BS.drop 20 (BS.tail afterName)
                            objectId <-
                                maybe
                                    (Left "Malformed tree entry: invalid object id bytes")
                                    Right
                                    (objectIdFromBytes objectIdBytesRaw)
                            go remaining (treeEntry mode nameBytes objectId : entries)

    parseMode modeBytes =
        case BS8.unpack modeBytes of
            "100644" -> Right RegularFile
            _ -> Left "Unsupported tree entry mode"

treeToObject :: [TreeEntry] -> GitObject
treeToObject = gitObject TreeObject . encodeTree

treeFromObject :: GitObject -> Either String [TreeEntry]
treeFromObject currentObject =
    if gitObjectType currentObject == TreeObject
        then decodeTree (gitObjectPayload currentObject)
        else Left "Expected a tree object"

renderTreeEntry :: TreeEntry -> String
renderTreeEntry currentEntry =
    BS8.unpack (renderMode (treeEntryMode currentEntry))
        ++ " blob "
        ++ objectIdToHex (treeEntryObjectId currentEntry)
        ++ "\t"
        ++ BS8.unpack (treeEntryName currentEntry)

renderMode :: TreeEntryMode -> ByteString
renderMode RegularFile = BS8.pack "100644"
