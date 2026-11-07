module Git.Object.Tree
    ( TreeEntry
    , TreeEntryMode (..)
    , decodeTree
    , encodeTree
    , renderTreeEntry
    , treeEntry
    , treeEntryMode
    , treeEntryName
    , treeEntryObjectId
    , treeEntryTypeName
    , treeFromObject
    , treeToObject
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import Data.List (sortOn)
import Git.Hash (ObjectId, objectIdBytes, objectIdFromBytes, objectIdLength, objectIdToHex)
import Git.Object (GitObject, GitObjectType (TreeObject), gitObject, gitObjectPayload, gitObjectType)

-- | The five entry modes a git tree may contain. Git enforces that no other
-- mode appears.
data TreeEntryMode
    = RegularFile
    | ExecutableFile
    | Symlink
    | Directory
    | Gitlink
    deriving (Eq, Show)

data TreeEntry = TreeEntry
    { treeEntryMode :: TreeEntryMode
    , treeEntryName :: ByteString
    , treeEntryObjectId :: ObjectId
    }
    deriving (Eq, Show)

treeEntry :: TreeEntryMode -> ByteString -> ObjectId -> TreeEntry
treeEntry = TreeEntry

-- | Serialise a tree. Entries are written in git's canonical order, where a
-- directory compares as its name followed by a slash, so a file @foo.txt@
-- sorts before a directory @foo@.
encodeTree :: [TreeEntry] -> ByteString
encodeTree entries = BS.concat (map encodeEntry (sortOn sortKey entries))
  where
    encodeEntry currentEntry =
        encodeMode (treeEntryMode currentEntry)
            <> BS8.singleton ' '
            <> treeEntryName currentEntry
            <> BS.singleton 0
            <> objectIdBytes (treeEntryObjectId currentEntry)

    sortKey currentEntry =
        treeEntryName currentEntry
            <> if treeEntryMode currentEntry == Directory then BS8.singleton '/' else BS.empty

-- | Parse a tree payload. Names are validated here because clone turns them
-- into filesystem paths and a tree that git itself would reject must not reach
-- the disk.
decodeTree :: ByteString -> Either String [TreeEntry]
decodeTree rawTree = go rawTree [] Set.empty
  where
    go bytes entries seen
        | BS.null bytes = Right (reverse entries)
        | otherwise = do
            let (modeBytes, afterMode) = BS8.break (== ' ') bytes
            if BS.null afterMode
                then Left "Malformed tree entry: missing mode separator"
                else do
                    mode <- decodeMode modeBytes
                    let (nameBytes, afterName) = BS.break (== 0) (BS.tail afterMode)
                    if BS.null afterName
                        then Left "Malformed tree entry: missing NUL separator"
                        else do
                            validateName nameBytes
                            if Set.member nameBytes seen
                                then Left ("Malformed tree entry: duplicate name " ++ BS8.unpack nameBytes)
                                else do
                                    let remaining = BS.tail afterName
                                        rawObjectId = BS.take objectIdLength remaining
                                    if BS.length rawObjectId /= objectIdLength
                                        then Left "Malformed tree entry: truncated object id"
                                        else do
                                            objectId <-
                                                maybe
                                                    (Left "Malformed tree entry: invalid object id bytes")
                                                    Right
                                                    (objectIdFromBytes rawObjectId)
                                            go
                                                (BS.drop objectIdLength remaining)
                                                (treeEntry mode nameBytes objectId : entries)
                                                (Set.insert nameBytes seen)

    validateName name
        | BS.null name = Left "Malformed tree entry: empty name"
        | name == BS8.singleton '.' = Left "Malformed tree entry: name is a dot"
        | name == BS8.pack ".." = Left "Malformed tree entry: name is a double dot"
        | BS8.elem '/' name = Left "Malformed tree entry: name contains a slash"
        | otherwise = Right ()

treeToObject :: [TreeEntry] -> GitObject
treeToObject = gitObject TreeObject . encodeTree

treeFromObject :: GitObject -> Either String [TreeEntry]
treeFromObject currentObject =
    if gitObjectType currentObject == TreeObject
        then decodeTree (gitObjectPayload currentObject)
        else Left "Expected a tree object"

-- | The type name git prints for an entry, which follows the mode rather than
-- the object it points at.
treeEntryTypeName :: TreeEntryMode -> ByteString
treeEntryTypeName mode =
    case mode of
        Directory -> BS8.pack "tree"
        Gitlink -> BS8.pack "commit"
        _ -> BS8.pack "blob"

-- | One @git ls-tree@ line. The mode is printed with six digits, so a
-- directory reads @040000@ even though the stored bytes are @40000@.
renderTreeEntry :: TreeEntry -> ByteString
renderTreeEntry currentEntry =
    renderModeDisplay (treeEntryMode currentEntry)
        <> BS8.singleton ' '
        <> treeEntryTypeName (treeEntryMode currentEntry)
        <> BS8.singleton ' '
        <> BS8.pack (objectIdToHex (treeEntryObjectId currentEntry))
        <> BS8.singleton '\t'
        <> treeEntryName currentEntry

encodeMode :: TreeEntryMode -> ByteString
encodeMode RegularFile = BS8.pack "100644"
encodeMode ExecutableFile = BS8.pack "100755"
encodeMode Symlink = BS8.pack "120000"
encodeMode Directory = BS8.pack "40000"
encodeMode Gitlink = BS8.pack "160000"

decodeMode :: ByteString -> Either String TreeEntryMode
decodeMode modeBytes =
    case BS8.unpack modeBytes of
        "100644" -> Right RegularFile
        "100755" -> Right ExecutableFile
        "120000" -> Right Symlink
        "40000" -> Right Directory
        "160000" -> Right Gitlink
        other -> Left ("Malformed tree entry: unsupported mode " ++ other)

renderModeDisplay :: TreeEntryMode -> ByteString
renderModeDisplay Directory = BS8.pack "040000"
renderModeDisplay mode = encodeMode mode
