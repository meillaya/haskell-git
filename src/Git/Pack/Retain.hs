module Git.Pack.Retain
    ( retainPackDirectly
    , retainPackFile
    ) where

import Control.Exception (evaluate, finally)
import Control.Monad (foldM, unless, when)
import Data.Array (Array, accumArray, elems, listArray, (!))
import Data.Bits ((.&.), complement, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , removeFile
    )
import System.FilePath ((</>))
import System.IO
    ( Handle
    , IOMode (ReadMode)
    , SeekMode (AbsoluteSeek)
    , hClose
    , hFileSize
    , hSeek
    , openTempFile
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
import Git.Object.Loose
    ( looseObjectPath
    , objectIdForObjectFor
    , objectIdForPayloadFor
    , readLooseObject
    )
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
                let indexBytes = buildPackIndex objectFormat entries packHash
                case parsePackIndexFor objectFormat indexBytes of
                    Left err -> ioError (userError ("generated pack index is invalid: " ++ err))
                    Right _ -> do
                        copyIfMissing packPath destinationPack
                        indexExists <- doesFileExist destinationIndex
                        when (not indexExists) (BS.writeFile destinationIndex indexBytes)
                        mapM_ (removeLoose . retainedObjectId) entries
  where
    copyIfMissing source destination = do
        exists <- doesFileExist destination
        when (not exists) (copyFile source destination)
    removeLoose objectId = do
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
    -> [RetainedObject]
    -> ObjectId
    -> ByteString
buildPackIndex objectFormat entries packHash =
    buildPackIndexSorted objectFormat (sortRetainedObjects entries) packHash

sortRetainedObjects :: [RetainedObject] -> [RetainedObject]
sortRetainedObjects = sortOn (objectIdBytes . retainedObjectId)

buildPackIndexSorted
    :: HashAlgorithm
    -> [RetainedObject]
    -> ObjectId
    -> ByteString
buildPackIndexSorted objectFormat sorted packHash =
    body <> objectIdBytes checksum
  where
    objectIds = map retainedObjectId sorted
    firstByteCounts :: Array Int Int
    firstByteCounts =
        accumArray
            (+)
            0
            (0, 255)
            [ (fromIntegral (BS.head (objectIdBytes objectId)), 1 :: Int)
            | objectId <- objectIds
            ]
    fanout :: [Word32]
    fanout =
        [ fromIntegral cumulative
        | cumulative <- drop 1 (scanl (+) 0 (elems firstByteCounts))
        ]
    offsets :: [Word32]
    offsets =
        [ fromIntegral (retainedOffset entry :: Int)
        | entry <- sorted
        ]
    largeOffsetValues =
        [ retainedOffset entry :: Int
        | entry <- sorted
        , retainedOffset entry > 0x7fffffff
        ]
    largeOffsetIndex :: Map.Map Int Int
    largeOffsetIndex = Map.fromList (zip largeOffsetValues [0 ..])
    encodedOffsets :: [Word32]
    encodedOffsets =
        [ if offset > 0x7fffffff
              then 0x80000000 + fromIntegral (largeOffsetIndex Map.! fromIntegral offset)
              else offset
        | offset <- offsets
        ]
    largeOffsets :: [Word64]
    largeOffsets = map (fromIntegral :: Int -> Word64) largeOffsetValues
    body =
        packIndexMagic
            <> encodeWord32 2
            <> BS.concat (map encodeWord32 fanout)
            <> BS.concat (map objectIdBytes objectIds)
            <> BS.concat (map (encodeWord32 . retainedCrc) sorted)
            <> BS.concat (map encodeWord32 encodedOffsets)
            <> BS.concat (map encodeWord64 largeOffsets)
            <> objectIdBytes packHash
    checksum = hashBytesFor objectFormat body

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

-- | Cache a resolved object in an uncompressed temporary file so delta bases
-- stay reachable without materialising loose objects. Keeping the cache
-- uncompressed makes delta resolution cheap; the file is deleted when the
-- retention attempt finishes.
data CachedObject = CachedObject
    { cachedRetained :: RetainedObject
    , cachedPayloadOffset :: Int
    , cachedPayloadLength :: Int
    , cachedObjectType :: GitObjectType
    }

data DirectRetention = DirectRetention
    { directObjectFormat :: HashAlgorithm
    , directCacheHandle :: Handle
    , directCacheSize :: Int
    , directOffsetMap :: Map.Map Int CachedObject
    , directObjectMap :: Map.Map ObjectId CachedObject
    , directIndexEntries :: [RetainedObject]
    , directPending :: [(Int, Word32, ObjectId, ByteString)]
    }

-- | Retain a self-contained pack by walking it once, resolving deltas through a
-- temporary uncompressed object cache, and writing the pack plus a v2 index.
-- No loose objects are written, which removes the write/read/delete cycle that
-- dominated clone time for large packs. Returns False when the pack is thin or
-- otherwise cannot be retained this way; the caller must then fall back to
-- 'importPackFile' and 'retainPackFile'.
retainPackDirectly :: HashAlgorithm -> Repository -> FilePath -> IO Bool
retainPackDirectly objectFormat repository packPath = do
    validatePackFile objectFormat packPath
    header <- readPackHeaderFromFile packPath
    let objectCount = packHeaderObjectCount header
    if objectCount <= 0
        then pure False
        else do
            packHash <- readPackHash objectFormat packPath
            let packDirectory = repositoryObjectsDir repository </> "pack"
                packName = "pack-" ++ objectIdToHex packHash
                destinationPack = packDirectory </> packName ++ ".pack"
                destinationIndex = packDirectory </> packName ++ ".idx"
            createDirectoryIfMissing True packDirectory
            (cachePath, cacheHandle) <-
                openTempFile packDirectory "tmp-haskell-git-direct-cache"
            let cleanupCache = do
                    hClose cacheHandle
                    exists <- doesFileExist cachePath
                    when exists (removeFile cachePath)
            outcome <-
                ( do
                    retained <-
                        do
                            let initialState =
                                    DirectRetention
                                        { directObjectFormat = objectFormat
                                        , directCacheHandle = cacheHandle
                                        , directCacheSize = 0
                                        , directOffsetMap = Map.empty
                                        , directObjectMap = Map.empty
                                        , directIndexEntries = []
                                        , directPending = []
                                        }
                            walked <-
                                withBinaryFile packPath ReadMode $ \packHandle ->
                                    consumeDirectEntries packHandle objectCount 12 initialState
                            resolveDirectPending walked
                    case retained of
                        Nothing -> pure False
                        Just finalState -> do
                            let entries = reverse (directIndexEntries finalState)
                                indexBytes = buildPackIndex objectFormat entries packHash
                            case parsePackIndexFor objectFormat indexBytes of
                                Left err ->
                                    ioError (userError ("generated pack index is invalid: " ++ err))
                                Right _ -> do
                                    copyIfMissing packPath destinationPack
                                    indexExists <- doesFileExist destinationIndex
                                    unless indexExists (BS.writeFile destinationIndex indexBytes)
                                    pure True
                )
                    `finally` cleanupCache
            pure outcome
  where
    copyIfMissing source destination = do
        exists <- doesFileExist destination
        when (not exists) (copyFile source destination)

consumeDirectEntries
    :: Handle
    -> Int
    -> Int
    -> DirectRetention
    -> IO DirectRetention
consumeDirectEntries packHandle remaining offset state
    | remaining <= 0 = pure state
    | otherwise = do
        (rawObject, consumed) <-
            either (ioError . userError) pure
                =<< readPackObjectAtHandle (directObjectFormat state) packHandle offset
        crc <- entryCrc packHandle offset consumed
        nextState <- rememberDirectObject packHandle offset crc rawObject state
        consumeDirectEntries packHandle (remaining - 1) (offset + consumed) nextState

rememberDirectObject
    :: Handle
    -> Int
    -> Word32
    -> PackRawObject
    -> DirectRetention
    -> IO DirectRetention
rememberDirectObject _packHandle offset crc rawObject state =
    case rawObject of
        PackRawFull objectKind payload ->
            appendDirectObject state offset crc (objectKindType objectKind) payload
        PackRawOffsetDelta baseDistance delta ->
            case Map.lookup (offset - baseDistance) (directOffsetMap state) of
                Nothing ->
                    ioError
                        ( userError
                            ( "pack delta base is not available at offset "
                                ++ show (offset - baseDistance)
                            )
                        )
                Just base -> do
                    basePayload <- readCachedPayload state base
                    applyWithBase state offset crc base basePayload delta
        PackRawRefDelta baseObjectId delta ->
            case Map.lookup baseObjectId (directObjectMap state) of
                Just base -> do
                    basePayload <- readCachedPayload state base
                    applyWithBase state offset crc base basePayload delta
                Nothing ->
                    pure
                        state
                            { directPending =
                                (offset, crc, baseObjectId, delta) : directPending state
                            }

objectKindType :: PackObjectKind -> GitObjectType
objectKindType objectKind =
    case objectKind of
        PackCommit -> CommitObject
        PackTree -> TreeObject
        PackBlob -> BlobObject
        PackTag -> TagObject
        PackOffsetDelta -> BlobObject
        PackReferenceDelta -> BlobObject

applyWithBase
    :: DirectRetention
    -> Int
    -> Word32
    -> CachedObject
    -> ByteString
    -> ByteString
    -> IO DirectRetention
applyWithBase state offset crc base basePayload delta = do
    let baseObject = gitObject (cachedObjectType base) basePayload
    objectValue <-
        either (ioError . userError) pure (applyDeltaToObject baseObject delta)
    appendDirectObject state offset crc (gitObjectType objectValue) (gitObjectPayload objectValue)

appendDirectObject
    :: DirectRetention
    -> Int
    -> Word32
    -> GitObjectType
    -> ByteString
    -> IO DirectRetention
appendDirectObject state offset crc objectType payload = do
    let cacheOffset = directCacheSize state
        objectFormat = directObjectFormat state
    objectId <- evaluate (objectIdForPayloadFor objectFormat objectType payload)
    let retained = RetainedObject objectId offset crc
        cached =
            CachedObject
                { cachedRetained = retained
                , cachedPayloadOffset = cacheOffset
                , cachedPayloadLength = BS.length payload
                , cachedObjectType = objectType
                }
    hSeek (directCacheHandle state) AbsoluteSeek (fromIntegral cacheOffset)
    BS.hPut (directCacheHandle state) payload
    pure
        state
            { directCacheSize = cacheOffset + BS.length payload
            , directOffsetMap = Map.insert offset cached (directOffsetMap state)
            , directObjectMap = Map.insert objectId cached (directObjectMap state)
            , directIndexEntries = retained : directIndexEntries state
            }

readCachedPayload :: DirectRetention -> CachedObject -> IO ByteString
readCachedPayload state cached = do
    hSeek (directCacheHandle state) AbsoluteSeek (fromIntegral (cachedPayloadOffset cached))
    payload <- BS.hGet (directCacheHandle state) (cachedPayloadLength cached)
    if BS.length payload /= cachedPayloadLength cached
        then ioError (userError "direct pack retention: object cache is truncated")
        else pure payload

resolveDirectPending :: DirectRetention -> IO (Maybe DirectRetention)
resolveDirectPending state = go (directPending state) (state {directPending = []})
  where
    go pending current
        | null pending = pure (Just current)
        | otherwise = do
            (next, remaining) <- foldM step (current, []) pending
            if length remaining == length pending
                then pure Nothing
                else go remaining next
    step (current, remaining) (offset, crc, baseObjectId, delta) =
        case Map.lookup baseObjectId (directObjectMap current) of
            Nothing ->
                pure (current, remaining ++ [(offset, crc, baseObjectId, delta)])
            Just base -> do
                basePayload <- readCachedPayload current base
                current' <- applyWithBase current offset crc base basePayload delta
                pure (current', remaining)
