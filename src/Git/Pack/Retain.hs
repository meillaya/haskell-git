module Git.Pack.Retain
    ( retainPackDirectly
    , retainPackFile
    ) where

import Control.Exception (IOException, bracket, evaluate, finally, try)
import Control.Monad (foldM, forM, unless, when)
import qualified Codec.Compression.Zlib as Zlib
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Array (Array, accumArray, elems, listArray, (!))
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Word (Word32, Word64, Word8)
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , getFileSize
    , removeFile
    , renameFile
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
    ( HashAlgorithm (..)
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
import Git.Store (Store, loadStore, readObjectOrFail)

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
    { cachedPayloadOffset :: Int
    , cachedPayloadLength :: Int
    , cachedObjectType :: GitObjectType
    }

-- | A delta entry that is still waiting for its base object. An offset delta
-- can wait for its base only when the base entry itself was deferred (for
-- example a thin pack whose delta chain is rooted at an external object).
data PendingObject
    = PendingRefDelta Int Word32 ObjectId ByteString
    | PendingOffsetDelta Int Word32 Int ByteString

data DirectRetention = DirectRetention
    { directObjectFormat :: HashAlgorithm
    , directCacheHandle :: Handle
    , directCacheSize :: Int
    , directOffsetMap :: Map.Map Int CachedObject
    , directObjectMap :: Map.Map ObjectId CachedObject
    , directIndexEntries :: [RetainedObject]
    , directPending :: [PendingObject]
    , directDeltaCount :: Int
    -- | Full objects read from the local repository to complete a thin pack,
    -- newest first. They are appended to the completed pack in reverse order.
    , directAppendedBases :: [GitObject]
    }

-- | Incremental pack checksum, matching the object format of the repository.
data PackHasher
    = Sha1Hasher SHA1.Ctx
    | Sha256Hasher SHA256.Ctx

newPackHasher :: HashAlgorithm -> PackHasher
newPackHasher Sha1 = Sha1Hasher SHA1.init
newPackHasher Sha256 = Sha256Hasher SHA256.init

updatePackHasher :: PackHasher -> ByteString -> PackHasher
updatePackHasher (Sha1Hasher context) bytes = Sha1Hasher (SHA1.update context bytes)
updatePackHasher (Sha256Hasher context) bytes = Sha256Hasher (SHA256.update context bytes)

finalizePackHasher :: PackHasher -> ByteString
finalizePackHasher (Sha1Hasher context) = SHA1.finalize context
finalizePackHasher (Sha256Hasher context) = SHA256.finalize context

-- | Retain a pack by walking it once, resolving deltas through a temporary
-- uncompressed object cache, and writing the pack plus a v2 index. No loose
-- objects are written, which removes the write/read/delete cycle that dominated
-- clone time for large packs.
--
-- A thin pack (one whose reference deltas point at objects outside the pack) is
-- completed in the style of @git index-pack --fix-thin@: the missing base
-- objects are read from the local repository store, appended to a copy of the
-- pack as full objects, and the pack header and trailer are rewritten. The
-- report callbacks receive object progress @(done, total)@ and the number of
-- resolved deltas once at the end. Returns False when the pack cannot be
-- retained this way (for example a base object is missing from the store); the
-- caller must then fall back to 'importPackFile' and 'retainPackFile'.
retainPackDirectly
    :: HashAlgorithm
    -> Repository
    -> FilePath
    -> (Int -> Int -> IO ())
    -> (Int -> Int -> IO ())
    -> IO Bool
retainPackDirectly objectFormat repository packPath reportObjects reportDeltas = do
    validatePackFile objectFormat packPath
    header <- readPackHeaderFromFile packPath
    let objectCount = packHeaderObjectCount header
    if objectCount <= 0
        then do
            reportObjects 0 0
            pure False
        else do
            reportObjects 0 objectCount
            packHash <- readPackHash objectFormat packPath
            let packDirectory = repositoryObjectsDir repository </> "pack"
            createDirectoryIfMissing True packDirectory
            (cachePath, cacheHandle) <-
                openTempFile packDirectory "tmp-haskell-git-direct-cache"
            let cleanupCache = do
                    hClose cacheHandle
                    exists <- doesFileExist cachePath
                    when exists (removeFile cachePath)
                initialState =
                    DirectRetention
                        { directObjectFormat = objectFormat
                        , directCacheHandle = cacheHandle
                        , directCacheSize = 0
                        , directOffsetMap = Map.empty
                        , directObjectMap = Map.empty
                        , directIndexEntries = []
                        , directPending = []
                        , directDeltaCount = 0
                        , directAppendedBases = []
                        }
            outcome <-
                ( do
                    walked <-
                        withBinaryFile packPath ReadMode $ \packHandle ->
                            consumeDirectEntries reportObjects packHandle objectCount objectCount 12 initialState
                    (resolved, remaining) <- resolveDirectPendingRounds walked
                    if null remaining
                        then do
                            installRetainedPack
                                objectFormat
                                repository
                                (reverse (directIndexEntries resolved))
                                packHash
                                packPath
                            reportObjects objectCount objectCount
                            reportDeltas (directDeltaCount resolved) (directDeltaCount resolved)
                            pure True
                        else do
                            completed <-
                                completeThinPack
                                    objectFormat
                                    repository
                                    packPath
                                    packDirectory
                                    objectCount
                                    reportObjects
                                    resolved
                                    remaining
                            case completed of
                                Nothing -> pure False
                                Just final -> do
                                    reportDeltas (directDeltaCount final) (directDeltaCount final)
                                    pure True
                ) `finally` cleanupCache
            pure outcome

consumeDirectEntries
    :: (Int -> Int -> IO ())
    -> Handle
    -> Int
    -> Int
    -> Int
    -> DirectRetention
    -> IO DirectRetention
consumeDirectEntries reportObjects packHandle objectCount remaining offset state
    | remaining <= 0 = pure state
    | otherwise = do
        (rawObject, consumed) <-
            either (ioError . userError) pure
                =<< readPackObjectAtHandle (directObjectFormat state) packHandle offset
        crc <- entryCrc packHandle offset consumed
        nextState <- rememberDirectObject packHandle offset crc rawObject state
        let walked = objectCount - remaining + 1
        when (walked `rem` directProgressInterval == 0) $
            reportObjects walked objectCount
        consumeDirectEntries
            reportObjects
            packHandle
            objectCount
            (remaining - 1)
            (offset + consumed)
            nextState

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
                Just base -> do
                    basePayload <- readCachedPayload state base
                    let counted = countDelta state
                    applyWithBase counted offset crc base basePayload delta
                Nothing ->
                    let counted = countDelta state
                     in pure
                            counted
                                { directPending =
                                    PendingOffsetDelta offset crc (offset - baseDistance) delta
                                        : directPending counted
                                }
        PackRawRefDelta baseObjectId delta ->
            case Map.lookup baseObjectId (directObjectMap state) of
                Just base -> do
                    basePayload <- readCachedPayload state base
                    let counted = countDelta state
                    applyWithBase counted offset crc base basePayload delta
                Nothing ->
                    let counted = countDelta state
                     in pure
                            counted
                                { directPending =
                                    PendingRefDelta offset crc baseObjectId delta : directPending counted
                                }
  where
    countDelta current = current {directDeltaCount = directDeltaCount current + 1}

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
                { cachedPayloadOffset = cacheOffset
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

-- | Resolve pending reference deltas in rounds, since a delta can be the base
-- of another delta that appears earlier in the pack. The returned list is the
-- set of still-unresolved deltas; an empty list means the pack was fully
-- resolved from its own objects.
resolveDirectPendingRounds :: DirectRetention -> IO (DirectRetention, [PendingObject])
resolveDirectPendingRounds state = go (directPending state) (state {directPending = []})
  where
    go pending current
        | null pending = pure (current, [])
        | otherwise = do
            (next, remaining) <- foldM step (current, []) pending
            if length remaining == length pending
                then pure (next, remaining)
                else go remaining next
    step (current, remaining) pendingObject =
        case pendingObject of
            PendingRefDelta offset crc baseObjectId delta ->
                case Map.lookup baseObjectId (directObjectMap current) of
                    Nothing -> pure (current, remaining ++ [pendingObject])
                    Just base -> do
                        basePayload <- readCachedPayload current base
                        current' <- applyWithBase current offset crc base basePayload delta
                        pure (current', remaining)
            PendingOffsetDelta offset crc baseOffset delta ->
                case Map.lookup baseOffset (directOffsetMap current) of
                    Nothing -> pure (current, remaining ++ [pendingObject])
                    Just base -> do
                        basePayload <- readCachedPayload current base
                        current' <- applyWithBase current offset crc base basePayload delta
                        pure (current', remaining)

-- | Complete a thin pack by loading the missing base objects from the local
-- repository store and appending them to a copy of the pack. Returns Nothing
-- (so the caller falls back to the loose import path) if a base is missing or
-- anything goes wrong; no completed pack or index is left behind in that case.
completeThinPack
    :: HashAlgorithm
    -> Repository
    -> FilePath
    -> FilePath
    -> Int
    -> (Int -> Int -> IO ())
    -> DirectRetention
    -> [PendingObject]
    -> IO (Maybe DirectRetention)
completeThinPack objectFormat repository packPath packDirectory objectCount reportObjects stalled remaining = do
    attempt <- try run :: IO (Either IOException (Maybe DirectRetention))
    pure (either (const Nothing) id attempt)
  where
    run = do
        resolved <- resolvePendingWithStore objectFormat repository stalled remaining
        case resolved of
            Nothing -> pure Nothing
            Just final -> do
                let appended = appendedBasesToWrite objectFormat final
                    finalTotal = objectCount + length appended
                reportObjects objectCount finalTotal
                bracket
                    (openTempFile packDirectory "tmp-haskell-git-completed-pack")
                    cleanupCompletedTemp
                    (\(completedPath, completedHandle) -> do
                        (appendedEntries, newPackHash) <-
                            writeCompletedPack
                                objectFormat
                                packPath
                                completedHandle
                                objectCount
                                appended
                        hClose completedHandle
                        let entries = reverse (directIndexEntries final) ++ appendedEntries
                            indexBytes = buildPackIndex objectFormat entries newPackHash
                        case parsePackIndexFor objectFormat indexBytes of
                            Left _ -> pure Nothing
                            Right _ -> do
                                installCompletedPack
                                    objectFormat
                                    repository
                                    entries
                                    newPackHash
                                    completedPath
                                reportObjects finalTotal finalTotal
                                pure (Just final)
                    )

cleanupCompletedTemp :: (FilePath, Handle) -> IO ()
cleanupCompletedTemp (completedPath, completedHandle) = do
    _ <- try (hClose completedHandle) :: IO (Either IOException ())
    exists <- doesFileExist completedPath
    when exists (removeFile completedPath)

-- | The bases to append to the completed pack, in append order. A base that
-- the thin pack can already reconstruct itself is dropped: appending it would
-- index the same object id twice. Dropping it is safe because the object is
-- already present in the pack body with identical content.
appendedBasesToWrite :: HashAlgorithm -> DirectRetention -> [GitObject]
appendedBasesToWrite objectFormat final = go Map.empty (reverse (directAppendedBases final))
  where
    packedIds =
        Map.fromList
            [ (retainedObjectId entry, ())
            | entry <- directIndexEntries final
            ]
    go _ [] = []
    go seen (gitObjectValue : rest)
        | Map.member objectId packedIds = go seen rest
        | Map.member objectId seen = go seen rest
        | otherwise = gitObjectValue : go (Map.insert objectId () seen) rest
      where
        objectId = objectIdForObjectFor objectFormat gitObjectValue

-- | Resolve the pending reference deltas that a thin pack inherited from the
-- clone/fetch it belongs to. Bases are read from the local repository, which
-- covers both loose objects and objects from earlier retained packs. A base
-- that cannot be read makes the whole completion fail cleanly.
resolvePendingWithStore
    :: HashAlgorithm
    -> Repository
    -> DirectRetention
    -> [PendingObject]
    -> IO (Maybe DirectRetention)
resolvePendingWithStore objectFormat repository stalled remaining = do
    loadedStore <- try (loadStore repository) :: IO (Either IOException Store)
    case loadedStore of
        Left _ -> pure Nothing
        Right store -> go (stalled {directPending = remaining}) store
  where
    go current store = do
        (resolved, unresolved) <- resolveDirectPendingRounds current
        if null unresolved
            then pure (Just resolved)
            else do
                let candidateIds =
                        stableUnique
                            [ baseObjectId
                            | PendingRefDelta _ _ baseObjectId _ <- unresolved
                            , Map.notMember baseObjectId (directObjectMap resolved)
                            ]
                bases <- catMaybes <$> forM candidateIds (loadBase store)
                if null bases
                    then pure Nothing
                    else do
                        next <- foldM appendExternalBase resolved bases
                        go (next {directPending = unresolved}) store

    loadBase store objectId = do
        attempt <- try (readObjectOrFail store objectId) :: IO (Either IOException GitObject)
        case attempt of
            Left _ -> pure Nothing
            Right gitObjectValue
                | objectIdForObjectFor objectFormat gitObjectValue == objectId ->
                    pure (Just (objectId, gitObjectValue))
                | otherwise -> pure Nothing

stableUnique :: [ObjectId] -> [ObjectId]
stableUnique = Map.keys . Map.fromList . map (\objectId -> (objectId, ()))

appendExternalBase :: DirectRetention -> (ObjectId, GitObject) -> IO DirectRetention
appendExternalBase state (objectId, gitObjectValue) = do
    let payload = gitObjectPayload gitObjectValue
        cacheOffset = directCacheSize state
        cached =
            CachedObject
                { cachedPayloadOffset = cacheOffset
                , cachedPayloadLength = BS.length payload
                , cachedObjectType = gitObjectType gitObjectValue
                }
    hSeek (directCacheHandle state) AbsoluteSeek (fromIntegral cacheOffset)
    BS.hPut (directCacheHandle state) payload
    pure
        state
            { directCacheSize = cacheOffset + BS.length payload
            , directObjectMap = Map.insert objectId cached (directObjectMap state)
            , directAppendedBases = gitObjectValue : directAppendedBases state
            }

-- | Write the completed pack: a v2 header with the larger object count, a
-- streamed copy of the original body (whose entry offsets do not change because
-- the header length does not change), the appended base objects, and the
-- checksum trailer. Returns the appended entries' index records and the hash of
-- the completed pack.
writeCompletedPack
    :: HashAlgorithm
    -> FilePath
    -> Handle
    -> Int
    -> [GitObject]
    -> IO ([RetainedObject], ObjectId)
writeCompletedPack objectFormat originalPackPath completedHandle objectCount appendedBases = do
    originalSize <- getFileSize originalPackPath
    let hashLength = hashSize objectFormat
        bodyLength = fromIntegral (originalSize - fromIntegral (directPackHeaderLength + hashLength))
        headerBytes =
            BS8.pack "PACK"
                <> encodeWord32 2
                <> encodeWord32 (fromIntegral (objectCount + length appendedBases))
        firstAppendedOffset = directPackHeaderLength + bodyLength
    BS.hPut completedHandle headerBytes
    bodyHasher <-
        copyPackBody
            objectFormat
            originalPackPath
            completedHandle
            (updatePackHasher (newPackHasher objectFormat) headerBytes)
            (fromIntegral bodyLength)
    (finalHasher, appendedEntries, _) <-
        foldM
            (writeAppendedBase objectFormat completedHandle)
            (bodyHasher, [], firstAppendedOffset)
            appendedBases
    let trailer = finalizePackHasher finalHasher
    BS.hPut completedHandle trailer
    newPackHash <-
        maybe
            (ioError (userError "completed pack trailer is not a valid object id"))
            pure
            (objectIdFromBytesFor objectFormat trailer)
    pure (appendedEntries, newPackHash)

copyPackBody
    :: HashAlgorithm
    -> FilePath
    -> Handle
    -> PackHasher
    -> Integer
    -> IO PackHasher
copyPackBody _objectFormat originalPackPath outputHandle initialHasher bodyLength =
    withBinaryFile originalPackPath ReadMode $ \inputHandle -> do
        hSeek inputHandle AbsoluteSeek (fromIntegral directPackHeaderLength)
        go inputHandle initialHasher bodyLength
  where
    go inputHandle hasher remaining
        | remaining <= 0 = pure hasher
        | otherwise = do
            let chunkSize = fromIntegral (min remaining packCopyChunkSize)
            chunk <- BS.hGet inputHandle chunkSize
            if BS.null chunk
                then ioError (userError "direct pack retention: original pack body is truncated")
                else do
                    BS.hPut outputHandle chunk
                    go
                        inputHandle
                        (updatePackHasher hasher chunk)
                        (remaining - fromIntegral (BS.length chunk))

writeAppendedBase
    :: HashAlgorithm
    -> Handle
    -> (PackHasher, [RetainedObject], Int)
    -> GitObject
    -> IO (PackHasher, [RetainedObject], Int)
writeAppendedBase objectFormat outputHandle (hasher, entries, offset) gitObjectValue = do
    let payload = gitObjectPayload gitObjectValue
        entry =
            objectHeader (gitObjectType gitObjectValue) (BS.length payload)
                <> BL.toStrict (Zlib.compress (BL.fromStrict payload))
        retained =
            RetainedObject
                { retainedObjectId =
                    objectIdForPayloadFor objectFormat (gitObjectType gitObjectValue) payload
                , retainedOffset = offset
                , retainedCrc = crc32 entry
                }
    BS.hPut outputHandle entry
    pure
        ( updatePackHasher hasher entry
        , retained : entries
        , offset + BS.length entry
        )

objectHeader :: GitObjectType -> Int -> ByteString
objectHeader objectType payloadSize =
    encodeObjectSize (objectKindCode objectType) payloadSize

objectKindCode :: GitObjectType -> Word8
objectKindCode BlobObject = 3
objectKindCode TreeObject = 2
objectKindCode CommitObject = 1
objectKindCode TagObject = 4

-- | Encode the pack entry header (kind and uncompressed size) the way git
-- does: a 4-bit kind followed by a little-endian variable length size.
encodeObjectSize :: Word8 -> Int -> ByteString
encodeObjectSize kindCodeValue size =
    case go (size `shiftR` 4) [] of
        [] -> BS.singleton firstByte
        restBytes -> BS.pack (firstByte : restBytes)
  where
    firstByte =
        (kindCodeValue `shiftL` 4)
            .|. fromIntegral (size .&. 0x0f)
            .|. (if size `shiftR` 4 == 0 then 0 else 0x80)
    go 0 accumulated = reverse accumulated
    go remaining accumulated =
        let byte = fromIntegral (remaining .&. 0x7f)
            rest = remaining `shiftR` 7
         in go rest ((byte .|. (if rest == 0 then 0 else 0x80)) : accumulated)

installRetainedPack
    :: HashAlgorithm
    -> Repository
    -> [RetainedObject]
    -> ObjectId
    -> FilePath
    -> IO ()
installRetainedPack objectFormat repository entries packHash sourcePack = do
    let packDirectory = repositoryObjectsDir repository </> "pack"
        packName = "pack-" ++ objectIdToHex packHash
        destinationPack = packDirectory </> packName ++ ".pack"
        destinationIndex = packDirectory </> packName ++ ".idx"
        indexBytes = buildPackIndex objectFormat entries packHash
    createDirectoryIfMissing True packDirectory
    case parsePackIndexFor objectFormat indexBytes of
        Left err -> ioError (userError ("generated pack index is invalid: " ++ err))
        Right _ -> do
            copyIfMissing sourcePack destinationPack
            indexExists <- doesFileExist destinationIndex
            unless indexExists (BS.writeFile destinationIndex indexBytes)
  where
    copyIfMissing source destination = do
        exists <- doesFileExist destination
        when (not exists) (copyFile source destination)

installCompletedPack
    :: HashAlgorithm
    -> Repository
    -> [RetainedObject]
    -> ObjectId
    -> FilePath
    -> IO ()
installCompletedPack objectFormat repository entries packHash completedPath = do
    let packDirectory = repositoryObjectsDir repository </> "pack"
        packName = "pack-" ++ objectIdToHex packHash
        destinationPack = packDirectory </> packName ++ ".pack"
        destinationIndex = packDirectory </> packName ++ ".idx"
        indexBytes = buildPackIndex objectFormat entries packHash
    createDirectoryIfMissing True packDirectory
    case parsePackIndexFor objectFormat indexBytes of
        Left err -> ioError (userError ("generated pack index is invalid: " ++ err))
        Right _ -> do
            packExists <- doesFileExist destinationPack
            unless packExists (renameFile completedPath destinationPack)
            indexExists <- doesFileExist destinationIndex
            unless indexExists (BS.writeFile destinationIndex indexBytes)

directPackHeaderLength :: Int
directPackHeaderLength = 12

directProgressInterval :: Int
directProgressInterval = 16

packCopyChunkSize :: Integer
packCopyChunkSize = 64 * 1024
