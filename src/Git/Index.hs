module Git.Index
    ( IndexEntry (..)
    , buildTreeFromIndex
    , indexEntryFromTree
    , parseIndex
    , sortIndexEntries
    , upsertIndexEntry
    , writeIndex
    ) where

import Control.Monad (forM)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Word (Word16, Word32)
import Git.Hash (ObjectId, hashBytes, objectIdBytes, objectIdFromBytes, objectIdLength)
import Git.Object.Loose (writeLooseObject)
import Git.Object.Tree (TreeEntry (..), TreeEntryMode (..), treeEntry, treeToObject)
import Git.Repo (Repository)

-- | One index entry. The stat fields git stores are written as zero: git
-- treats a zero size as "hash the file and compare", so a fresh checkout is
-- still reported clean and git refreshes the fields on the next status.
data IndexEntry = IndexEntry
    { indexEntryPath :: ByteString
    , indexEntryMode :: TreeEntryMode
    , indexEntrySize :: Word32
    , indexEntryObjectId :: ObjectId
    }
    deriving (Eq, Show)

indexEntryFromTree :: FilePath -> TreeEntry -> Word32 -> IndexEntry
indexEntryFromTree path entry size =
    IndexEntry
        { indexEntryPath = BS8.pack path
        , indexEntryMode = treeEntryMode entry
        , indexEntrySize = size
        , indexEntryObjectId = treeEntryObjectId entry
        }

sortIndexEntries :: [IndexEntry] -> [IndexEntry]
sortIndexEntries = sortOn indexEntryPath

-- | Replace an entry with the same path, or insert it, then keep the list in
-- path order so the serialised index is stable.
upsertIndexEntry :: IndexEntry -> [IndexEntry] -> [IndexEntry]
upsertIndexEntry entry entries = sortIndexEntries (entry : filter ((/= indexEntryPath entry) . indexEntryPath) entries)

-- | Serialise the index in version 2. Each entry is a 62-byte fixed header,
-- the path, one to eight NUL bytes of padding, and the whole file ends with a
-- SHA-1. The padding rule is measured from the start of the entry, not from
-- the start of the file.
writeIndex :: [IndexEntry] -> ByteString
writeIndex entries =
    let sorted = sortIndexEntries entries
        header = BS8.pack "DIRC" <> encodeWord32 2 <> encodeWord32 (fromIntegral (length sorted))
        body = BS.concat (map encodeEntry sorted)
        content = header <> body
     in content <> objectIdBytes (hashBytes content)

encodeEntry :: IndexEntry -> ByteString
encodeEntry entry =
    let pathBytes = indexEntryPath entry
        flags = fromIntegral (min (BS.length pathBytes) maximumInlinePathLength) :: Word16
        withoutPath =
            BS.concat
                [ encodeWord32 0 -- ctime seconds
                , encodeWord32 0 -- ctime nanoseconds
                , encodeWord32 0 -- mtime seconds
                , encodeWord32 0 -- mtime nanoseconds
                , encodeWord32 0 -- device
                , encodeWord32 0 -- inode
                , encodeWord32 (modeWord (indexEntryMode entry))
                , encodeWord32 0 -- uid
                , encodeWord32 0 -- gid
                , encodeWord32 (indexEntrySize entry)
                , objectIdBytes (indexEntryObjectId entry)
                , encodeWord16 flags
                , pathBytes
                , BS.singleton 0
                ]
        padding = (alignment - (BS.length withoutPath `mod` alignment)) `mod` alignment
     in withoutPath <> BS.replicate padding 0

-- | Parse a version 2 index. Extensions are skipped and the trailing SHA-1 is
-- verified. Versions 3 and 4 are refused by name.
parseIndex :: ByteString -> Either String [IndexEntry]
parseIndex bytes = do
    if BS.length bytes < minimumIndexLength
        then Left "malformed index: file is too short"
        else pure ()
    if BS.take 4 bytes /= indexSignature
        then Left "malformed index: bad signature"
        else pure ()
    let version = decodeWord32 (BS.take 4 (BS.drop 4 bytes))
        count = fromIntegral (decodeWord32 (BS.take 4 (BS.drop 8 bytes)))
    if version /= supportedIndexVersion
        then Left ("unsupported index version: " ++ show version)
        else pure ()
    let contentLength = BS.length bytes - objectIdLength
        expectedChecksum = BS.drop contentLength bytes
        actualChecksum = objectIdBytes (hashBytes (BS.take contentLength bytes))
    if expectedChecksum /= actualChecksum
        then Left "malformed index: checksum mismatch"
        else pure ()
    entries <- parseEntries headerLength count
    Right entries
  where
    headerLength = indexHeaderLength

    parseEntries _ 0 = Right []
    parseEntries position remaining
        | position + fixedEntryLength > BS.length bytes - objectIdLength =
            Left "malformed index: truncated entry"
        | otherwise = do
            let modePosition = position + modeOffset
                sizePosition = position + sizeOffset
                objectIdPosition = position + objectIdOffset
                flagsPosition = position + flagsOffset
                pathStart = position + fixedEntryLength
                rawFlags = decodeWord16 (BS.take 2 (BS.drop flagsPosition bytes))
                declaredPathLength = fromIntegral (rawFlags .&. 0x0fff)
            objectId <-
                maybe
                    (Left "malformed index: invalid object id")
                    Right
                    (objectIdFromBytes (BS.take objectIdLength (BS.drop objectIdPosition bytes)))
            pathLength <-
                resolvePathLength pathStart declaredPathLength
            let entryLength = alignedEntryLength pathLength
                nextPosition = position + entryLength
            if nextPosition > BS.length bytes - objectIdLength
                then Left "malformed index: entry padding runs past the file"
                else pure ()
            let pathBytes = BS.take pathLength (BS.drop pathStart bytes)
            mode <- decodeModeWord (decodeWord32 (BS.take 4 (BS.drop modePosition bytes)))
            let size = decodeWord32 (BS.take 4 (BS.drop sizePosition bytes))
                entry =
                    IndexEntry
                        { indexEntryPath = pathBytes
                        , indexEntryMode = mode
                        , indexEntrySize = size
                        , indexEntryObjectId = objectId
                        }
            (entry :) <$> parseEntries nextPosition (remaining - 1)

    resolvePathLength pathStart declaredPathLength
        | declaredPathLength < maximumInlinePathLength = Right declaredPathLength
        | otherwise =
            case BS.elemIndex 0 (BS.drop pathStart bytes) of
                Just terminator -> Right terminator
                Nothing -> Left "malformed index: unterminated long path"

decodeModeWord :: Word32 -> Either String TreeEntryMode
decodeModeWord modeWord
    | modeWord .&. 0o170000 == 0o100000 =
        case modeWord .&. 0o111 of
            0 -> Right RegularFile
            _ -> Right ExecutableFile
    | modeWord .&. 0o170000 == 0o120000 = Right Symlink
    | modeWord .&. 0o170000 == 0o160000 = Right Gitlink
    | otherwise = Left ("malformed index: unsupported mode " ++ show modeWord)

modeWord :: TreeEntryMode -> Word32
modeWord RegularFile = 0o100644
modeWord ExecutableFile = 0o100755
modeWord Symlink = 0o120000
modeWord Directory = 0o40000
modeWord Gitlink = 0o160000

encodeWord16 :: Word16 -> ByteString
encodeWord16 value =
    BS.pack
        [ fromIntegral (value `shiftR` 8)
        , fromIntegral (value .&. 0xff)
        ]

encodeWord32 :: Word32 -> ByteString
encodeWord32 value =
    BS.pack
        [ fromIntegral (value `shiftR` 24)
        , fromIntegral ((value `shiftR` 16) .&. 0xff)
        , fromIntegral ((value `shiftR` 8) .&. 0xff)
        , fromIntegral (value .&. 0xff)
        ]

decodeWord16 :: ByteString -> Word16
decodeWord16 = BS.foldl' (\accumulator word -> (accumulator `shiftL` 8) .|. fromIntegral word) 0

decodeWord32 :: ByteString -> Word32
decodeWord32 = BS.foldl' (\accumulator word -> (accumulator `shiftL` 8) .|. fromIntegral word) 0

alignment :: Int
alignment = 8

alignedEntryLength :: Int -> Int
alignedEntryLength pathLength =
    ((fixedEntryLength + pathLength + alignment) `div` alignment) * alignment

indexSignature :: ByteString
indexSignature = BS8.pack "DIRC"

indexHeaderLength :: Int
indexHeaderLength = 12

supportedIndexVersion :: Word32
supportedIndexVersion = 2

fixedEntryLength :: Int
fixedEntryLength = 62

modeOffset :: Int
modeOffset = 24

sizeOffset :: Int
sizeOffset = 36

objectIdOffset :: Int
objectIdOffset = 40

flagsOffset :: Int
flagsOffset = 60

maximumInlinePathLength :: Int
maximumInlinePathLength = 0x0fff

minimumIndexLength :: Int
minimumIndexLength = indexHeaderLength + objectIdLength

-- | Build the tree objects that the index describes and return the root tree
-- id. Directories are formed from the path components; a path that is both a
-- file and a directory is rejected.
buildTreeFromIndex :: Repository -> [IndexEntry] -> IO ObjectId
buildTreeFromIndex repository entries
    | null entries = ioError (userError "cannot write a tree from an empty index")
    | otherwise = buildLevel [(indexEntryPath entry, entry) | entry <- entries]
  where
    buildLevel levelEntries = do
        let grouped =
                Map.toList
                    ( Map.fromListWith
                        (++)
                        [ (firstComponent path, [(restOfPath path, entry)])
                        | (path, entry) <- levelEntries
                        ]
                    )
        treeEntries <- forM grouped $ \(component, children) -> do
            let directEntries = [entry | (rest, entry) <- children, BS.null rest]
                nestedEntries = [(rest, entry) | (rest, entry) <- children, not (BS.null rest)]
            case (directEntries, nestedEntries) of
                ([entry], []) ->
                    pure (treeEntry (indexEntryMode entry) component (indexEntryObjectId entry))
                ([], _) -> do
                    childTreeId <- buildLevel nestedEntries
                    pure (treeEntry Directory component childTreeId)
                _ ->
                    ioError
                        (userError ("conflicting index paths under " ++ BS8.unpack component))
        writeLooseObject repository (treeToObject treeEntries)

firstComponent :: ByteString -> ByteString
firstComponent = BS8.takeWhile (/= '/')

restOfPath :: ByteString -> ByteString
restOfPath path =
    case BS8.break (== '/') path of
        (_, rest) | BS.null rest -> BS.empty
        (_, rest) -> BS.tail rest
