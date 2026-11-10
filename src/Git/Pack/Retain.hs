module Git.Pack.Retain
    ( retainPackFile
    ) where

import Control.Monad (when)
import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), complement, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , removeFile
    )
import System.FilePath ((</>), takeDirectory)
import System.IO
    ( Handle
    , IOMode (ReadMode)
    , SeekMode (AbsoluteSeek)
    , hFileSize
    , hSeek
    , withBinaryFile
    )
import Git.Hash
    ( HashAlgorithm
    , ObjectId
    , hashBytesFor
    , hashSize
    , objectIdBytes
    , objectIdFromBytesFor
    , objectIdToHex
    )
import Git.Object
    ( GitObject
    , GitObjectType (..)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Loose (looseObjectPath, objectIdForObjectFor, readLooseObject)
import Git.Pack (PackObjectKind (..), packHeaderObjectCount)
import Git.Pack.Delta (applyDelta, parseDelta)
import Git.Pack.Index (parsePackIndexFor)
import Git.Pack.Read (PackRawObject (..))
import Git.Pack.Stream
    ( readPackHeaderFromFile
    , readPackObjectAtHandle
    , validatePackFile
    )
import Git.Repo (Repository, repositoryObjectsDir)

data RetainedObject = RetainedObject
    { retainedObjectId :: ObjectId
    , retainedOffset :: Int
    , retainedCrc :: Word32
    }

-- | Turn a just-imported pack into a permanent pack plus a v2 index, then
-- remove the loose copies. The pack must be self-contained (a thin pack is
-- left alone). This is best-effort at the call site; the loose objects remain
-- until the index has been written and parsed successfully.
retainPackFile :: HashAlgorithm -> Repository -> FilePath -> IO ()
retainPackFile objectFormat repository packPath = do
    validatePackFile objectFormat packPath
    header <- readPackHeaderFromFile packPath
    let objectCount = packHeaderObjectCount header
    objects <- withBinaryFile packPath ReadMode $ \handle ->
        collectObjects objectFormat repository handle objectCount 12 Map.empty [] False
    case objects of
        Left _ -> pure ()
        Right (entries, externalBase)
            | externalBase -> pure ()
            | null entries -> pure ()
            | otherwise -> do
                packHash <- readPackHash objectFormat packPath
                let packName = "pack-" ++ objectIdToHex packHash
                    packDirectory = repositoryObjectsDir repository </> "pack"
                    destinationPack = packDirectory </> packName ++ ".pack"
                    destinationIndex = packDirectory </> packName ++ ".idx"
                createDirectoryIfMissing True packDirectory
                indexBytes <- buildPackIndex objectFormat packPath entries packHash
                case parsePackIndexFor objectFormat indexBytes of
                    Left err -> ioError (userError ("generated pack index is invalid: " ++ err))
                    Right _ -> do
                        copyIfMissing packPath destinationPack
                        indexExists <- doesFileExist destinationIndex
                        when (not indexExists) (BS.writeFile destinationIndex indexBytes)
                        mapM_ (removeLoose repository . retainedObjectId) entries
  where
    copyIfMissing source destination = do
        exists <- doesFileExist destination
        when (not exists) (copyFile source destination)
    removeLoose repository objectId = do
        let path = looseObjectPath repository objectId
        exists <- doesFileExist path
        when exists (removeFile path)

collectObjects
    :: HashAlgorithm
    -> Repository
    -> Handle
    -> Int
    -> Int
    -> Map.Map Int ObjectId
    -> [RetainedObject]
    -> Bool
    -> IO (Either String ([RetainedObject], Bool))
collectObjects objectFormat repository handle remaining offset offsetMap entries externalBase
    | remaining <= 0 = pure (Right (reverse entries, externalBase))
    | otherwise = do
        result <- readPackObjectAtHandle objectFormat handle offset
        case result of
            Left err -> pure (Left err)
            Right (rawObject, consumed) -> do
                crc <- entryCrc handle offset consumed
                case rawObject of
                    PackRawFull objectKind payload -> do
                        objectValue <- pure (toGitObject objectKind payload)
                        let objectId = objectIdForObjectFor objectFormat objectValue
                        collectObjects
                            objectFormat
                            repository
                            handle
                            (remaining - 1)
                            (offset + consumed)
                            (Map.insert offset objectId offsetMap)
                            (RetainedObject objectId offset crc : entries)
                            externalBase
                    PackRawOffsetDelta baseDistance delta ->
                        case Map.lookup (offset - baseDistance) offsetMap of
                            Nothing -> pure (Left "offset delta base is not in the pack")
                            Just baseId -> do
                                baseObject <- readLooseObject repository baseId
                                objectValue <- either (pure . Left) (pure . Right) (applyDeltaToObject baseObject delta)
                                case objectValue of
                                    Left err -> pure (Left err)
                                    Right value -> do
                                        let objectId = objectIdForObjectFor objectFormat value
                                        collectObjects
                                            objectFormat
                                            repository
                                            handle
                                            (remaining - 1)
                                            (offset + consumed)
                                            (Map.insert offset objectId offsetMap)
                                            (RetainedObject objectId offset crc : entries)
                                            externalBase
                    PackRawRefDelta baseObjectId delta -> do
                        baseObject <- readLooseObject repository baseObjectId
                        objectValue <- either (pure . Left) (pure . Right) (applyDeltaToObject baseObject delta)
                        case objectValue of
                            Left err -> pure (Left err)
                            Right value -> do
                                let objectId = objectIdForObjectFor objectFormat value
                                    baseInPack = baseObjectId `elem` Map.elems offsetMap
                                collectObjects
                                    objectFormat
                                    repository
                                    handle
                                    (remaining - 1)
                                    (offset + consumed)
                                    (Map.insert offset objectId offsetMap)
                                    (RetainedObject objectId offset crc : entries)
                                    (externalBase || not baseInPack)

entryCrc :: Handle -> Int -> Int -> IO Word32
entryCrc handle offset lengthBytes = do
    hSeek handle AbsoluteSeek (fromIntegral offset)
    bytes <- BS.hGet handle lengthBytes
    pure (crc32 bytes)

crc32 :: ByteString -> Word32
crc32 bytes = complement (BS.foldl' step 0xffffffff bytes)
  where
    step crc byte =
        let index = fromIntegral ((crc `xor` fromIntegral byte) .&. 0xff)
         in (crc `shiftR` 8) `xor` crcTable ! index
    crcTable :: Array Int Word32
    crcTable = listArray (0, 255) [generateTableEntry index | index <- [0 .. 255]]
    generateTableEntry index =
        foldl (\value _ -> (value `shiftR` 1) `xor` (if value .&. 1 /= 0 then 0xedb88320 else 0)) index [1 .. 8 :: Int]

toGitObject :: PackObjectKind -> ByteString -> GitObject
toGitObject objectKind payload =
    case objectKind of
        PackCommit -> gitObject CommitObject payload
        PackTree -> gitObject TreeObject payload
        PackBlob -> gitObject BlobObject payload
        PackTag -> gitObject TagObject payload
        _ -> gitObject BlobObject payload

applyDeltaToObject :: GitObject -> ByteString -> Either String GitObject
applyDeltaToObject baseObject delta =
    case parseDelta delta of
        Left err -> Left err
        Right (baseSize, resultSize, instructions) ->
            case applyDelta baseSize resultSize (gitObjectPayload baseObject) instructions of
                Left err -> Left err
                Right payload -> Right (gitObject (gitObjectType baseObject) payload)

readPackHash :: HashAlgorithm -> FilePath -> IO ObjectId
readPackHash objectFormat path =
    withBinaryFile path ReadMode $ \handle -> do
        size <- hFileSizeInt handle
        let hashLength = hashSize objectFormat
        hSeek handle AbsoluteSeek (fromIntegral (size - hashLength))
        bytes <- BS.hGet handle hashLength
        case objectIdFromBytesFor objectFormat bytes of
            Just objectId -> pure objectId
            Nothing -> ioError (userError "pack trailer is not a valid object id")
  where
    hFileSizeInt handle = do
        size <- hFileSize handle
        pure (fromIntegral size)

buildPackIndex
    :: HashAlgorithm
    -> FilePath
    -> [RetainedObject]
    -> ObjectId
    -> IO ByteString
buildPackIndex objectFormat packPath entries packHash = do
    entryBytes <- withBinaryFile packPath ReadMode $ \handle -> do
        traverse
            (\entry -> do
                hSeek handle AbsoluteSeek (fromIntegral (retainedOffset entry))
                pure (retainedCrc entry))
            sorted
    let objectIds = map retainedObjectId sorted
        fanout =
            [ fromIntegral
                (length [() | objectId <- objectIds, BS.head (objectIdBytes objectId) <= index])
            | index <- [0 .. 255]
            ]
        offsets =
            [ fromIntegral (retainedOffset entry :: Int)
            | entry <- sorted
            ]
        largeOffsets =
            [ fromIntegral (retainedOffset entry :: Int) :: Word64
            | entry <- sorted
            , retainedOffset entry > 0x7fffffff
            ]
        encodedOffsets =
            [ if offset > 0x7fffffff
                  then 0x80000000 + fromIntegral (largeIndex offset)
                  else offset
            | offset <- offsets
            ]
        body =
            packIndexMagic
                <> encodeWord32 2
                <> BS.concat (map encodeWord32 fanout)
                <> BS.concat (map objectIdBytes objectIds)
                <> BS.concat (map encodeWord32 entryBytes)
                <> BS.concat (map encodeWord32 encodedOffsets)
                <> BS.concat (map encodeWord64 largeOffsets)
                <> objectIdBytes packHash
        checksum = hashBytesFor objectFormat body
    pure (body <> objectIdBytes checksum)
  where
    sorted = sortOn (objectIdBytes . retainedObjectId) entries
    largeIndex :: Word32 -> Int
    largeIndex offset =
        length
            [ ()
            | other <- offsetsBefore
            , other > 0x7fffffff
            , other < offset
            ]
    offsetsBefore = [fromIntegral (retainedOffset entry) | entry <- sorted]

packIndexMagic :: ByteString
packIndexMagic = BS.pack [0xff, 0x74, 0x4f, 0x63]

encodeWord32 :: Word32 -> ByteString
encodeWord32 value =
    BS.pack
        [ fromIntegral ((value `shiftR` 24) .&. 0xff)
        , fromIntegral ((value `shiftR` 16) .&. 0xff)
        , fromIntegral ((value `shiftR` 8) .&. 0xff)
        , fromIntegral (value .&. 0xff)
        ]

encodeWord64 :: Word64 -> ByteString
encodeWord64 value =
    BS.concat
        [ encodeWord32 (fromIntegral (value `shiftR` 32))
        , encodeWord32 (fromIntegral (value .&. 0xffffffff))
        ]
