module Git.Push
    ( PushOptions (..)
    , PushResult (..)
    , defaultPushOptions
    , pushRepository
    ) where

import Control.Monad (filterM, forM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Git.Config (Config, appendConfigValue, configGet, loadRepositoryConfig)
import Git.Hash
    ( HashAlgorithm
    , ObjectId
    , objectIdToHex
    , zeroObjectIdFor
    )
import Git.Object
    ( GitObject
    , GitObjectType (..)
    , gitObject
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Commit (commitFromObjectFor, commitParents, commitTree)
import Git.Object.Loose (objectIdForObjectFor)
import Git.Object.Tag (tagFromObjectFor, tagObject)
import Git.Object.Tree (treeEntryObjectId, treeFromObjectFor)
import Git.Pack.Write (writePackFile)
import Git.Refs
    ( HeadReference (..)
    , deleteRef
    , readHeadReference
    , readRefValue
    , writeRefValue
    )
import Git.Protocol.PacketLine
    ( decodePacketLines
    , encodePacketLine
    , flushPacket
    , packetLine
    , packetLinePayload
    )
import Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement
    , DiscoveryRef
    , discoveryCapabilities
    , discoveryObjectFormat
    , discoveryRefName
    , discoveryRefObjectId
    , discoveryRefs
    )
import Git.RefSpec (RefSpec (..), parseRefSpec, refSpecMatches)
import Git.Refs (readHeadReference, resolveRevision, HeadReference (..))
import Git.Repo
    ( Repository
    , repositoryObjectFormat
    )
import Git.Store
    ( Store
    , loadStore
    , objectExists
    , readObjectOrFail
    , storeRepository
    )
import Git.Transport
    ( Service (..)
    , Transport (..)
    , parseRemoteUrl
    , withTemporaryPack
    , withTransport
    )

data PushOptions = PushOptions
    { pushForce :: Bool
    , pushDelete :: Bool
    , pushSetUpstream :: Bool
    , pushForceWithLease :: Bool
    }
    deriving (Eq, Show)

defaultPushOptions :: PushOptions
defaultPushOptions = PushOptions False False False False

data PushResult = PushResult
    { pushReportedRefs :: [(String, String)]
    }
    deriving (Eq, Show)

data Remote = Remote
    { pushRemoteName :: String
    , pushRemoteUrl :: String
    }

pushRepository :: Repository -> String -> [String] -> PushOptions -> IO PushResult
pushRepository repository remoteArgument explicitRefSpecs options = do
    config <- loadRepositoryConfig repository
    remote <- resolveRemote config remoteArgument
    store <- loadStore repository
    let objectFormat = repositoryObjectFormat repository
        zeroId = zeroObjectIdFor objectFormat
    withTransport config (pushRemoteUrl remote) $ \transport -> do
        advertisement <- transportDiscovery transport ReceivePackService
        let remoteRefs =
                Map.fromList
                    [ (bsToText (discoveryRefName ref), discoveryRefObjectId ref)
                    | ref <- filter (not . isPeeledRef) (discoveryRefs advertisement)
                    ]
        updates <- buildUpdates repository store options (pushRemoteName remote) (discoveryObjectFormat advertisement) remoteRefs explicitRefSpecs
        if null updates
            then pure (PushResult [])
            else do
                let newTips =
                        [ newId
                        | PushUpdate { pushUpdateNew = Just newId } <- updates
                        ]
                    oldTipCandidates =
                        [ oldId
                        | PushUpdate { pushUpdateOld = oldId } <- updates
                        , oldId /= zeroId
                        ]
                oldTips <- filterM (objectExists store) oldTipCandidates
                newClosure <- collectClosure store newTips
                oldClosure <- collectClosure store oldTips
                let newObjects = Set.toList (newClosure `Set.difference` oldClosure)
                objects <- traverse (readObjectOrFail store) newObjects
                reportedRefs <- withTemporaryPack $ \path -> do
                    when (not (null objects)) (writePackFile objectFormat objects path)
                    commands <- pure (encodePushCommands objectFormat (discoveryCapabilities advertisement) updates)
                    response <-
                        transportReceivePack
                            transport
                            commands
                            (if null objects then Nothing else Just path)
                    case parseReportStatus response of
                        Left err -> ioError (userError err)
                        Right reported -> pure (Just reported)
                let reported = maybe [] id reportedRefs
                updateTrackingRefs repository remote updates
                when (pushSetUpstream options) (setUpstream repository remote updates)
                pure (PushResult reported)
  where
    isPeeledRef ref = "^{}" `isSuffixOfText` bsToText (discoveryRefName ref)

resolveRemote :: Config -> String -> IO Remote
resolveRemote config argument = do
    let name = if null argument then "origin" else argument
    case configGet config ("remote." ++ name ++ ".url") of
        Just url -> pure (Remote name url)
        Nothing ->
            case parseRemoteUrl name of
                Right _ -> pure (Remote "origin" name)
                Left _ -> ioError (userError ("unknown remote: " ++ name))

data PushUpdate = PushUpdate
    { pushUpdateRef :: String
    , pushUpdateOld :: ObjectId
    , pushUpdateNew :: Maybe ObjectId
    }
    deriving (Show)

buildUpdates
    :: Repository
    -> Store
    -> PushOptions
    -> String
    -> HashAlgorithm
    -> Map.Map String ObjectId
    -> [String]
    -> IO [PushUpdate]
buildUpdates repository store options remoteName objectFormat remoteRefs explicitRefSpecs = do
    refspecs <- pushRefSpecs repository options explicitRefSpecs
    updates <- traverse (buildUpdate repository store options remoteName objectFormat remoteRefs) refspecs
    pure (filter somethingToDo updates)
  where
    somethingToDo update =
        pushUpdateOld update /= maybe (zeroObjectIdFor objectFormat) id (pushUpdateNew update)

pushRefSpecs :: Repository -> PushOptions -> [String] -> IO [RefSpec]
pushRefSpecs repository options explicitRefSpecs
    | pushDelete options =
        pure
            [ RefSpec
                { refSpecSource = ""
                , refSpecDestination = Just destination
                , refSpecForce = False
                }
            | spec <- explicitRefSpecs
            , let destination =
                    if "refs/" `isPrefixOf` spec then spec else "refs/heads/" ++ spec
            ]
    | null explicitRefSpecs = do
        headReference <- readHeadReference repository
        case headReference of
            SymbolicHead refPath ->
                pure [RefSpec refPath (Just refPath) False]
            DetachedHead _ ->
                ioError (userError "cannot infer a push refspec from a detached HEAD")
    | otherwise = traverse (either (ioError . userError) pure . parseRefSpec) explicitRefSpecs

buildUpdate
    :: Repository
    -> Store
    -> PushOptions
    -> String
    -> HashAlgorithm
    -> Map.Map String ObjectId
    -> RefSpec
    -> IO PushUpdate
buildUpdate repository store options remoteName objectFormat remoteRefs refSpec
    | null (refSpecSource refSpec) && pushDelete options =
        case refSpecDestination refSpec of
            Nothing -> ioError (userError "a delete refspec needs a destination")
            Just destination -> do
                let oldId = Map.findWithDefault (zeroObjectIdFor objectFormat) destination remoteRefs
                pure (PushUpdate destination oldId Nothing)
    | otherwise = do
        newId <-
            if pushDelete options
                then pure Nothing
                else do
                    maybeObjectId <- resolveRevisionInStore store (refSpecSource refSpec)
                    pure (Just maybeObjectId)
        let destination =
                case refSpecDestination refSpec of
                    Just value -> value
                    Nothing -> refSpecSource refSpec
            oldId = Map.findWithDefault (zeroObjectIdFor objectFormat) destination remoteRefs
        case newId of
            Nothing -> pure (PushUpdate destination oldId Nothing)
            Just newObjectId -> do
                let force = pushForce options || refSpecForce refSpec
                when (pushForceWithLease options && oldId /= zeroObjectIdFor objectFormat) $
                    checkLease repository remoteName destination oldId
                when (oldId /= zeroObjectIdFor objectFormat && not force) $ do
                    ancestor <- isAncestor store oldId newObjectId
                    if ancestor
                        then pure ()
                        else ioError (userError ("non-fast-forward update: " ++ destination))
                pure (PushUpdate destination oldId (Just newObjectId))

trackingRefFor :: String -> FilePath -> Maybe FilePath
trackingRefFor remoteName destination =
    case stripPrefix "refs/heads/" destination of
        Just branch -> Just ("refs/remotes/" ++ remoteName ++ "/" ++ branch)
        Nothing -> Nothing

checkLease :: Repository -> String -> FilePath -> ObjectId -> IO ()
checkLease repository remoteName destination oldId =
    case trackingRefFor remoteName destination of
        Nothing -> pure ()
        Just trackingRef -> do
            tracking <- readRefValue repository trackingRef
            case tracking of
                Just expected
                    | expected == oldId -> pure ()
                    | otherwise ->
                        ioError
                            ( userError
                                ( "stale info: "
                                    ++ trackingRef
                                    ++ " does not match the remote"
                                )
                            )
                Nothing ->
                    ioError (userError ("stale info: " ++ trackingRef ++ " is missing"))

updateTrackingRefs :: Repository -> Remote -> [PushUpdate] -> IO ()
updateTrackingRefs repository remote updates =
    forM_ updates $ \update ->
        case trackingRefFor (pushRemoteName remote) (pushUpdateRef update) of
            Nothing -> pure ()
            Just trackingRef ->
                case pushUpdateNew update of
                    Just newId -> writeRefValue repository trackingRef newId
                    Nothing -> deleteRef repository trackingRef

setUpstream :: Repository -> Remote -> [PushUpdate] -> IO ()
setUpstream repository remote updates =
    case [branch | update <- updates, Just branch <- [pushedBranch update]] of
        [] -> pure ()
        pushedBranches -> do
            headReference <- readHeadReference repository
            let currentBranch =
                    case headReference of
                        SymbolicHead refPath -> stripPrefix "refs/heads/" refPath
                        DetachedHead _ -> Nothing
            case currentBranch of
                Nothing -> pure ()
                Just branch -> do
                    appendConfigValue repository "branch" (Just branch) "remote" (pushRemoteName remote)
                    let mergeRef =
                            if branch `elem` pushedBranches
                                then "refs/heads/" ++ branch
                                else "refs/heads/" ++ head pushedBranches
                    appendConfigValue repository "branch" (Just branch) "merge" mergeRef
  where
    pushedBranch update =
        case pushUpdateNew update of
            Nothing -> Nothing
            Just _ -> stripPrefix "refs/heads/" (pushUpdateRef update)

resolveRevisionInStore :: Store -> String -> IO ObjectId
resolveRevisionInStore store revision = do
    let repository = storeRepository store
    maybeObjectId <- resolveRevision repository revision
    case maybeObjectId of
        Nothing -> ioError (userError ("unknown revision: " ++ revision))
        Just objectId -> do
            exists <- objectExists store objectId
            if exists then pure objectId else ioError (userError ("object not found: " ++ objectIdToHex objectId))

isAncestor :: Store -> ObjectId -> ObjectId -> IO Bool
isAncestor store ancestor descendant
    | ancestor == descendant = pure True
    | otherwise = go Set.empty [descendant]
  where
    objectFormat = repositoryObjectFormat (storeRepository store)

    go _ [] = pure False
    go seen (current : rest)
        | Set.member current seen = go seen rest
        | otherwise = do
            parentIds <- commitParentsOf current
            if ancestor `elem` parentIds
                then pure True
                else go (Set.insert current seen) (parentIds ++ rest)

    commitParentsOf objectId = do
        gitObjectValue <- readObjectOrFail store objectId
        case gitObjectType gitObjectValue of
            CommitObject ->
                case commitFromObjectFor objectFormat gitObjectValue of
                    Left _ -> pure []
                    Right currentCommit -> pure (commitParents currentCommit)
            _ -> pure []

collectClosure :: Store -> [ObjectId] -> IO (Set.Set ObjectId)
collectClosure store roots = go Set.empty roots
  where
    objectFormat = repositoryObjectFormat (storeRepository store)

    go seen [] = pure seen
    go seen (objectId : rest)
        | Set.member objectId seen = go seen rest
        | otherwise = do
            children <- childrenOf objectId
            go (Set.insert objectId seen) (children ++ rest)

    childrenOf objectId = do
        gitObjectValue <- readObjectOrFail store objectId
        case gitObjectType gitObjectValue of
            CommitObject ->
                case commitFromObjectFor objectFormat gitObjectValue of
                    Left _ -> pure []
                    Right currentCommit ->
                        pure (commitTree currentCommit : commitParents currentCommit)
            TreeObject ->
                case treeFromObjectFor objectFormat gitObjectValue of
                    Left _ -> pure []
                    Right entries -> pure (map treeEntryObjectId entries)
            TagObject ->
                case tagFromObjectFor objectFormat gitObjectValue of
                    Left _ -> pure []
                    Right currentTag -> pure [tagObject currentTag]
            BlobObject -> pure []

encodePushCommands :: HashAlgorithm -> [ByteString] -> [PushUpdate] -> ByteString
encodePushCommands objectFormat capabilities updates =
    BS.concat
        ( zipWith encodeUpdate [0 :: Int ..] updates
            ++ [encodePacketLine flushPacket]
        )
  where
    requestedCapabilities =
        [ capability
        | capability <- [BS8.pack "report-status", BS8.pack "delete-refs"]
        , capability `elem` capabilities
        ]
            ++ [ capability
               | capability <- capabilities
               , BS8.pack "object-format=" `BS.isPrefixOf` capability
               ]
    encodeUpdate index update =
        let oldId = objectIdToHex (pushUpdateOld update)
            newId = objectIdToHex (maybe (zeroObjectIdFor objectFormat) id (pushUpdateNew update))
            capabilitySuffix =
                if index == 0 && not (null requestedCapabilities)
                    then BS.singleton 0 <> BS8.intercalate (BS8.singleton ' ') requestedCapabilities
                    else BS.empty
         in encodePacketLine
                ( packetLine
                    ( BS8.pack (oldId ++ " " ++ newId ++ " " ++ pushUpdateRef update)
                        <> capabilitySuffix
                        <> BS8.singleton '\n'
                    )
                )

parseReportStatus :: ByteString -> Either String [(String, String)]
parseReportStatus body
    | BS.null body = Right []
    | otherwise = do
        packetLines <- decodePacketLines body
        let payloads = [payload | Just payload <- map packetLinePayload packetLines]
            cleaned = map (BS8.unpack . dropTrailingNewline) payloads
        case [line | line <- cleaned, "unpack " `isPrefixOf` line, line /= "unpack ok"] of
            errorLine : _ -> Left ("remote rejected the pack: " ++ errorLine)
            [] -> pure ()
        case [line | line <- cleaned, "ng " `isPrefixOf` line] of
            rejection : _ -> Left ("remote rejected the update: " ++ rejection)
            [] -> pure ()
        pure
            [ (refName, "ok")
            | line <- cleaned
            , "ok " `isPrefixOf` line
            , let refName = drop 3 line
            ]
  where
    dropTrailingNewline = BS8.reverse . BS8.dropWhile (== '\n') . BS8.reverse

bsToText :: ByteString -> String
bsToText = BS8.unpack

isSuffixOfText :: String -> String -> Bool
isSuffixOfText suffix text = reverse suffix `isPrefixOf` reverse text
