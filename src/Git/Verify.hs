module Git.Verify
    ( VerifyReport (..)
    , renderVerifyReport
    , verifyRepository
    ) where

import Control.Exception (IOException, try)
import Control.Monad (forM)
import Git.Hash (ObjectId, objectIdToHex)
import Git.Object.Loose (listLooseObjectIds, objectIdForObjectFor, readLooseObject)
import Git.Pack.Index (packIndexObjectIds)
import Git.Pack.Stream (validatePackFile)
import Git.Refs (listRefs, resolveHeadReference)
import Git.Repo (Repository, repositoryObjectFormat)
import Git.Store
    ( PackFile (..)
    , Store
    , loadStore
    , objectExists
    , readObjectOrFail
    , storePacks
    )
import System.IO.Error (ioeGetErrorString)

data VerifyReport = VerifyReport
    { verifyObjectCount :: Int
    , verifyRefCount :: Int
    , verifyProblems :: [String]
    }
    deriving (Eq, Show)

-- | Check the object database and the references. The index and the working
-- tree are derived state and are not checked here.
verifyRepository :: Repository -> IO VerifyReport
verifyRepository repository = do
    looseObjectIds <- listLooseObjectIds repository
    looseProblems <- fmap concat (forM looseObjectIds (verifyLooseObject repository))
    storeAttempt <- try (loadStore repository)
    case storeAttempt of
        Left err ->
            pure
                VerifyReport
                    { verifyObjectCount = length looseObjectIds
                    , verifyRefCount = 0
                    , verifyProblems = looseProblems ++ ["object store: " ++ ioeGetErrorString err]
                    }
        Right store -> do
            let packedObjectIds =
                    concatMap (packIndexObjectIds . packFileIndex) (storePacks store)
            packProblems <- fmap concat (forM (storePacks store) (verifyPackTrailer repository))
            packedProblems <- fmap concat (forM packedObjectIds (verifyPackedObject store))
            (refProblems, refCount) <- verifyReferences repository store
            pure
                VerifyReport
                    { verifyObjectCount = length looseObjectIds + length packedObjectIds
                    , verifyRefCount = refCount
                    , verifyProblems = looseProblems ++ packProblems ++ packedProblems ++ refProblems
                    }

verifyLooseObject :: Repository -> ObjectId -> IO [String]
verifyLooseObject repository objectId = do
    attempt <- try (readLooseObject repository objectId)
    case attempt of
        Left err -> pure [describe objectId err]
        Right gitObjectValue ->
            if objectIdForObjectFor (repositoryObjectFormat repository) gitObjectValue == objectId
                then pure []
                else pure [objectIdToHex objectId ++ ": content does not hash to its file name"]

verifyPackTrailer :: Repository -> PackFile -> IO [String]
verifyPackTrailer repository pack = do
    attempt <- try (validatePackFile (repositoryObjectFormat repository) (packFilePath pack))
    case attempt of
        Left err -> pure [packFilePath pack ++ ": " ++ ioeGetErrorString err]
        Right () -> pure []

verifyPackedObject :: Store -> ObjectId -> IO [String]
verifyPackedObject store objectId = do
    attempt <- try (readObjectOrFail store objectId)
    case attempt of
        Left err -> pure [describe objectId err]
        Right _ -> pure []

verifyReferences :: Repository -> Store -> IO ([String], Int)
verifyReferences repository store = do
    refs <- listRefs repository
    refProblems <- fmap concat (forM refs (verifyRef store))
    headAttempt <- try (resolveHeadReference repository)
    headProblems <-
        case headAttempt of
            Left err -> pure ["HEAD: " ++ ioeGetErrorString err]
            Right Nothing -> pure []
            Right (Just headObjectId) -> do
                exists <- objectExists store headObjectId
                pure
                    ( if exists
                        then []
                        else ["HEAD: missing object " ++ objectIdToHex headObjectId]
                    )
    pure (refProblems ++ headProblems, length refs)

verifyRef :: Store -> (FilePath, ObjectId) -> IO [String]
verifyRef store (refPath, objectId) = do
    exists <- objectExists store objectId
    pure
        ( if exists
            then []
            else [refPath ++ ": missing object " ++ objectIdToHex objectId]
        )

describe :: ObjectId -> IOException -> String
describe objectId err = objectIdToHex objectId ++ ": " ++ ioeGetErrorString err

renderVerifyReport :: VerifyReport -> [String]
renderVerifyReport report =
    map ("problem: " ++) (verifyProblems report)
        ++ [ "verify: "
                ++ show (verifyObjectCount report)
                ++ " objects, "
                ++ show (verifyRefCount report)
                ++ " refs, "
                ++ show (length (verifyProblems report))
                ++ " problems"
           ]
