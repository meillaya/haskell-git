module Git.Pack.Import
    ( importPackFile
    ) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import System.IO (IOMode (ReadMode), withBinaryFile)
import Git.Hash (HashAlgorithm, ObjectId, objectIdToHex)
import Git.Object
    ( GitObject
    , GitObjectType (..)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Loose
    ( looseObjectPath
    , readLooseObject
    , writeLooseObject
    )
import Git.Pack (PackObjectKind (..), packHeaderObjectCount)
import Git.Pack.Delta (applyDelta, parseDelta)
import Git.Pack.Read (PackRawObject (..))
import Git.Pack.Stream (readPackHeaderFromFile, readPackObjectAtHandle, validatePackFile)
import Git.Repo (Repository)

-- | Import every object from a pack file into loose objects. Objects are
-- written as soon as their delta base is available; only unresolved reference
-- deltas stay in memory. The pack trailer is verified before anything is
-- written.
importPackFile
    :: HashAlgorithm
    -> Repository
    -> FilePath
    -> (Int -> Int -> IO ())
    -> IO ()
importPackFile objectFormat repository packPath reportObjects = do
    validatePackFile objectFormat packPath
    header <- readPackHeaderFromFile packPath
    let objectCount = packHeaderObjectCount header
    reportObjects 0 objectCount
    withBinaryFile packPath ReadMode $ \handle -> do
        state <- consumeEntries handle objectCount 12 (ImportState Map.empty [])
        resolvePending state
    reportObjects objectCount objectCount
  where
    consumeEntries handle remaining offset state
        | remaining <= 0 = pure state
        | otherwise = do
            (rawObject, consumed) <-
                either (ioError . userError) pure =<< readPackObjectAtHandle objectFormat handle offset
            nextState <- consumeEntry offset rawObject state
            consumeEntries handle (remaining - 1) (offset + consumed) nextState

    consumeEntry offset rawObject state =
        case rawObject of
            PackRawFull objectKind payload -> do
                gitObjectValue <- either (ioError . userError) pure (toGitObject objectKind payload)
                writtenId <- writeLooseObject repository gitObjectValue
                pure (remember offset writtenId state)
            PackRawOffsetDelta baseDistance delta ->
                case Map.lookup (offset - baseDistance) (importOffsetToId state) of
                    Nothing ->
                        ioError
                            ( userError
                                ( "pack delta base is not available at offset "
                                    ++ show (offset - baseDistance)
                                )
                            )
                    Just baseId -> do
                        baseObject <- readBaseObject baseId
                        gitObjectValue <- either (ioError . userError) pure (applyDeltaToObject baseObject delta)
                        writtenId <- writeLooseObject repository gitObjectValue
                        pure (remember offset writtenId state)
            PackRawRefDelta baseObjectId delta -> do
                baseObject <- readObjectIfPresent baseObjectId
                case baseObject of
                    Nothing ->
                        pure
                            state
                                { importPending =
                                    (offset, baseObjectId, delta) : importPending state
                                }
                    Just base -> do
                        gitObjectValue <- either (ioError . userError) pure (applyDeltaToObject base delta)
                        writtenId <- writeLooseObject repository gitObjectValue
                        pure (remember offset writtenId state)

    resolvePending state
        | null (importPending state) = pure ()
        | otherwise = do
            (resolvedState, unresolved) <- foldM attempt (state, []) (importPending state)
            if length unresolved == length (importPending state)
                then
                    ioError
                        ( userError
                            ( "pack has "
                                ++ show (length unresolved)
                                ++ " objects whose delta bases are missing"
                            )
                        )
                else resolvePending (resolvedState {importPending = unresolved})

    attempt (state, unresolved) (offset, baseObjectId, delta) = do
        baseObject <- readObjectIfPresent baseObjectId
        case baseObject of
            Nothing -> pure (state, unresolved ++ [(offset, baseObjectId, delta)])
            Just base -> do
                gitObjectValue <- either (ioError . userError) pure (applyDeltaToObject base delta)
                writtenId <- writeLooseObject repository gitObjectValue
                pure (remember offset writtenId state, unresolved)

    readBaseObject baseObjectId = do
        maybeObject <- readObjectIfPresent baseObjectId
        case maybeObject of
            Just gitObjectValue -> pure gitObjectValue
            Nothing ->
                ioError
                    (userError ("pack delta base is missing: " ++ objectIdToHex baseObjectId))

    readObjectIfPresent objectId = do
        exists <- doesFileExist (looseObjectPath repository objectId)
        if exists then Just <$> readLooseObject repository objectId else pure Nothing

data ImportState = ImportState
    { importOffsetToId :: Map.Map Int ObjectId
    , importPending :: [(Int, ObjectId, ByteString)]
    }

remember :: Int -> ObjectId -> ImportState -> ImportState
remember offset objectId state =
    state
        { importOffsetToId =
            Map.insert offset objectId (importOffsetToId state)
        }

applyDeltaToObject :: GitObject -> ByteString -> Either String GitObject
applyDeltaToObject baseObject delta =
    case parseDelta delta of
        Left err -> Left err
        Right (baseSize, resultSize, instructions) ->
            case applyDelta baseSize resultSize (gitObjectPayload baseObject) instructions of
                Left err -> Left err
                Right payload -> Right (gitObject (gitObjectType baseObject) payload)

toGitObject :: PackObjectKind -> ByteString -> Either String GitObject
toGitObject objectKind payload =
    case objectKind of
        PackCommit -> Right (gitObject CommitObject payload)
        PackTree -> Right (gitObject TreeObject payload)
        PackBlob -> Right (gitObject BlobObject payload)
        PackTag -> Right (gitObject TagObject payload)
        _ -> Left "pack contains a delta entry where a full object was expected"
