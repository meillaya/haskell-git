module Git.Fetch
    ( FetchOptions (..)
    , FetchResult (..)
    , defaultFetchOptions
    , fetchRepository
    ) where

import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (find, isPrefixOf, isSuffixOf, nub)
import qualified Data.Set as Set
import Git.Config (Config, configGet, configGetAll, loadRepositoryConfig)
import Git.Hash (ObjectId, objectIdToHex)
import Git.Pack.Import (importPackFile)
import Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement
    , DiscoveryRef
    , discoveryCapabilities
    , discoveryObjectFormat
    , discoveryRefName
    , discoveryRefObjectId
    , discoveryRefs
    )
import Git.RefSpec
    ( RefSpec (..)
    , applyRefSpec
    , parseRefSpec
    )
import Git.Refs (deleteRef, listRefs, writeRefValue)
import Git.Repo
    ( Repository
    , repositoryFetchHeadPath
    , repositoryObjectFormat
    )
import Git.Transport
    ( Service (..)
    , Transport (..)
    , parseRemoteUrl
    , withTemporaryPack
    , withTransport
    )

data FetchOptions = FetchOptions
    { fetchTags :: Bool
    , fetchPrune :: Bool
    }
    deriving (Eq, Show)

defaultFetchOptions :: FetchOptions
defaultFetchOptions = FetchOptions True False

data FetchResult = FetchResult
    { fetchUpdatedRefs :: [(FilePath, ObjectId)]
    , fetchHeadObject :: Maybe ObjectId
    }
    deriving (Eq, Show)

data Remote = Remote
    { remoteName :: String
    , remoteUrl :: String
    , remoteRefSpecs :: [RefSpec]
    }

-- | Fetch from a configured remote (or a URL). The remote's tracking refs are
-- updated, FETCH_HEAD is written, and the pack is imported with the streaming
-- importer.
fetchRepository :: Repository -> String -> [String] -> FetchOptions -> IO FetchResult
fetchRepository repository remoteArgument explicitRefSpecs options = do
    config <- loadRepositoryConfig repository
    remote <- resolveRemote config remoteArgument
    requestedSpecs <-
        if null explicitRefSpecs
            then pure (remoteRefSpecs remote)
            else traverse (either (ioError . userError) pure . parseRefSpec) explicitRefSpecs
    withTransport config (remoteUrl remote) $ \transport -> do
        advertisement <- transportDiscovery transport UploadPackService
        let advertised = filter (not . isPeeledRef . discoveryRefName) (discoveryRefs advertisement)
            objectFormat = discoveryObjectFormat advertisement
        when (objectFormat /= repositoryObjectFormat repository) $
            ioError
                ( userError
                    "remote object format does not match the local repository"
                )
        let matched =
                [ (remoteRef, spec, destination)
                | remoteRef <- advertised
                , spec <- requestedSpecs
                , Just destination <- [applyRefSpec spec (bsToText (discoveryRefName remoteRef))]
                , shouldUpdate spec (bsToText (discoveryRefName remoteRef))
                ]
            tagRefs =
                if fetchTags options
                    then filter isTagRef advertised
                    else []
            fetchedRefs = map discoveryRefObjectId (map (\(ref, _, _) -> ref) matched)
            tagIds = map discoveryRefObjectId tagRefs
            wantedIds = nub (fetchedRefs ++ tagIds)
        when (not (null wantedIds)) $
            withTemporaryPack $ \packPath -> do
                transportUploadPackToFile
                    transport
                    (discoveryCapabilities advertisement)
                    wantedIds
                    packPath
                importPackFile objectFormat repository packPath
        updated <- updateRefs repository matched
        updatedTags <- writeTags repository tagRefs
        when (fetchPrune options) (pruneRemoteRefs repository remote matched)
        writeFetchHead repository remote matched
        let headObject =
                case matched of
                    ((remoteRef, _, _) : _) -> Just (discoveryRefObjectId remoteRef)
                    [] -> Nothing
        pure
            FetchResult
                { fetchUpdatedRefs = updated ++ updatedTags
                , fetchHeadObject = headObject
                }

shouldUpdate :: RefSpec -> String -> Bool
shouldUpdate _ remoteRef = not ("refs/pull/" `isPrefixOf` remoteRef)

resolveRemote :: Config -> String -> IO Remote
resolveRemote config argument = do
    let name = if null argument then "origin" else argument
    case configGet config ("remote." ++ name ++ ".url") of
        Just url -> do
            specs <- configuredSpecs name
            pure
                Remote
                    { remoteName = name
                    , remoteUrl = url
                    , remoteRefSpecs = specs
                    }
        Nothing ->
            case parseRemoteUrl name of
                Right _ -> do
                    specs <- traverse (either (ioError . userError) pure . parseRefSpec) [defaultSpec "origin"]
                    pure
                        Remote
                            { remoteName = "origin"
                            , remoteUrl = name
                            , remoteRefSpecs = specs
                            }
                Left _ ->
                    ioError (userError ("unknown remote: " ++ name))
  where
    configuredSpecs name =
        case configGetAll config ("remote." ++ name ++ ".fetch") of
            [] -> traverse (either (ioError . userError) pure . parseRefSpec) [defaultSpec name]
            specs -> traverse (either (ioError . userError) pure . parseRefSpec) specs
    defaultSpec name = "+refs/heads/*:refs/remotes/" ++ name ++ "/*"

updateRefs :: Repository -> [(DiscoveryRef, RefSpec, FilePath)] -> IO [(FilePath, ObjectId)]
updateRefs repository matched =
    forM matched $ \(remoteRef, _spec, destination) -> do
        let objectId = discoveryRefObjectId remoteRef
        writeRefValue repository destination objectId
        pure (destination, objectId)

writeTags :: Repository -> [DiscoveryRef] -> IO [(FilePath, ObjectId)]
writeTags repository tagRefs =
    forM tagRefs $ \tagRef -> do
        let name = bsToText (discoveryRefName tagRef)
            objectId = discoveryRefObjectId tagRef
        writeRefValue repository name objectId
        pure (name, objectId)

pruneRemoteRefs :: Repository -> Remote -> [(DiscoveryRef, RefSpec, FilePath)] -> IO ()
pruneRemoteRefs repository remote matched = do
    refs <- listRefs repository
    let prefix = "refs/remotes/" ++ remoteName remote ++ "/"
        kept = Set.fromList [destination | (_, _, destination) <- matched]
        stale =
            [ name
            | (name, _) <- refs
            , prefix `isPrefixOf` name
            , not (Set.member name kept)
            ]
    mapM_ (deleteRef repository) stale

writeFetchHead :: Repository -> Remote -> [(DiscoveryRef, RefSpec, FilePath)] -> IO ()
writeFetchHead repository remote matched =
    when (not (null matched)) $ do
        let url = remoteUrl remote
            linesToWrite =
                [ objectIdToHex (discoveryRefObjectId remoteRef)
                    ++ "\t"
                    ++ (if index == 0 then "\t" else "not-for-merge\t")
                    ++ description remoteRef
                | (index, (remoteRef, _, _)) <- zip [0 ..] matched
                ]
            description remoteRef =
                let refName = bsToText (discoveryRefName remoteRef)
                    shortName = stripPrefixes refName
                 in if "refs/tags/" `isPrefixOf` refName
                        then "tag '" ++ shortName ++ "' of " ++ url
                        else "branch '" ++ shortName ++ "' of " ++ url
        writeFile (repositoryFetchHeadPath repository) (unlines linesToWrite)

stripPrefixes :: String -> String
stripPrefixes name =
    case find (`isPrefixOf` name) ["refs/heads/", "refs/tags/", "refs/remotes/"] of
        Just prefix -> drop (length prefix) name
        Nothing -> name

isPeeledRef :: ByteString -> Bool
isPeeledRef name = "^{}" `isSuffixOf` bsToText name

isSuffixOfText :: String -> String -> Bool
isSuffixOfText suffix text = reverse suffix `isPrefixOf` reverse text

isTagRef :: DiscoveryRef -> Bool
isTagRef ref = "refs/tags/" `isPrefixOf` bsToText (discoveryRefName ref)

bsToText :: ByteString -> String
bsToText = BS8.unpack
