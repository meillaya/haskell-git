module Git.Pull
    ( pullRepository
    , pullRepositoryRebase
    , pullRepositoryWith
    ) where

import Data.Char (isHexDigit)
import Git.Fetch (FetchOptions, fetchRepository)
import Git.Hash (ObjectId, objectIdFromHexFor)
import Git.Merge (MergeOptions, MergeResult, defaultMergeOptions, mergeRepository, mergeRepositoryWith)
import Git.Rebase (RebaseResult, rebaseRepository)
import Git.Object (GitObjectType (..), gitObjectType)
import Git.Object.Tag (tagFromObjectFor, tagObject)
import Git.Repo
    ( Repository
    , repositoryFetchHeadPath
    , repositoryObjectFormat
    )
import Git.Store (Store, loadStore, readObjectOrFail)

-- | Fetch then merge the fetched head. Fast-forwarding and clean merges are
-- handled by 'mergeRepository'; conflicts are returned to the caller.
pullRepository :: Repository -> String -> FetchOptions -> IO MergeResult
pullRepository = pullRepositoryWith defaultMergeOptions

pullRepositoryWith :: MergeOptions -> Repository -> String -> FetchOptions -> IO MergeResult
pullRepositoryWith mergeOptions repository remote options = do
    (store, targetId) <- fetchTarget repository remote options
    mergeRepositoryWith mergeOptions repository store targetId

pullRepositoryRebase :: Repository -> String -> FetchOptions -> IO RebaseResult
pullRepositoryRebase repository remote options = do
    (store, targetId) <- fetchTarget repository remote options
    rebaseRepository repository store targetId

fetchTarget :: Repository -> String -> FetchOptions -> IO (Store, ObjectId)
fetchTarget repository remote options = do
    _ <- fetchRepository repository remote [] options
    fetchedId <- firstFetchHead repository
    store <- loadStore repository
    targetId <- peelToCommit repository store fetchedId
    pure (store, targetId)

firstFetchHead :: Repository -> IO ObjectId
firstFetchHead repository = do
    contents <- readFile (repositoryFetchHeadPath repository)
    case lines contents of
        [] -> ioError (userError "FETCH_HEAD is empty")
        firstLine : _ ->
            case takeWhile isHexDigit firstLine of
                hexText ->
                    case objectIdFromHexFor (repositoryObjectFormat repository) hexText of
                        Just objectId -> pure objectId
                        Nothing -> ioError (userError "FETCH_HEAD is malformed")

peelToCommit :: Repository -> Store -> ObjectId -> IO ObjectId
peelToCommit repository store objectId = do
    gitObjectValue <- readObjectOrFail store objectId
    case gitObjectType gitObjectValue of
        CommitObject -> pure objectId
        TagObject ->
            case tagFromObjectFor (repositoryObjectFormat repository) gitObjectValue of
                Left err -> ioError (userError err)
                Right currentTag -> peelToCommit repository store (tagObject currentTag)
        _ -> ioError (userError "fetched object is not a commit")
