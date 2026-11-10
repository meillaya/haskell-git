module Git.Clone
    ( CloneResult (..)
    , cloneRepository
    ) where

import Control.Exception (onException)
import Control.Monad (forM, forM_, when)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isControl, isSpace)
import Data.List (find, isPrefixOf, nub)
import System.Directory
    ( Permissions (..)
    , createDirectoryIfMissing
    , createFileLink
    , doesDirectoryExist
    , doesPathExist
    , getPermissions
    , listDirectory
    , removePathForcibly
    , setPermissions
    )
import System.FilePath ((</>), takeDirectory)
import Git.Config (loadGlobalConfig)
import Git.Hash (HashAlgorithm, ObjectId)
import Git.Index (IndexEntry, indexEntryFromTree, writeIndexFor)
import Git.Object
    ( GitObjectType (..)
    , gitObjectPayload
    , gitObjectType
    )
import Git.Object.Commit (commitFromObjectFor, commitTree)
import Git.Object.Tree
    ( TreeEntry
    , TreeEntryMode (..)
    , treeEntryMode
    , treeEntryName
    , treeEntryObjectId
    )
import Git.Pack.Import (importPackFile)
import Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement
    , DiscoveryRef
    , capabilityValue
    , discoveryCapabilities
    , discoveryObjectFormat
    , discoveryRefName
    , discoveryRefObjectId
    , discoveryRefs
    )
import Git.Refs
    ( symbolicHead
    , writeHeadReference
    , writeRefValue
    , writeSymbolicRefValue
    )
import Git.Repo
    ( Repository (..)
    , initRepositoryWithFormat
    , repositoryConfigPath
    , repositoryIndexPath
    , validateReferenceName
    )
import Git.Store (Store, flattenTree, loadStore, readObjectOrFail, storeRepository)
import Git.Transport
    ( Service (..)
    , Transport (..)
    , parseRemoteUrl
    , withTemporaryPack
    , withTransport
    )

data CloneResult = CloneResult
    { cloneHead :: Maybe ObjectId
    , cloneLocalBranch :: FilePath
    }
    deriving (Eq, Show)

cloneRepository :: String -> FilePath -> IO CloneResult
cloneRepository remoteUrl destination = do
    validateRemoteUrl remoteUrl
    prepareDestination destination
    cloneInto remoteUrl destination `onException` removePathForcibly destination

cloneInto :: String -> FilePath -> IO CloneResult
cloneInto remoteUrl destination = do
    config <- loadGlobalConfig
    withTransport config remoteUrl $ \transport -> do
        advertisement <- transportDiscovery transport UploadPackService
        let objectFormat = discoveryObjectFormat advertisement
            advertisedRefs =
                filter (not . isPeeledRefName . discoveryRefName) (discoveryRefs advertisement)
            branchRefs = filter (isBranchRef . discoveryRefName) advertisedRefs
            tagRefs = filter (isTagRef . discoveryRefName) advertisedRefs
        if null branchRefs
            then cloneEmptyRepository objectFormat remoteUrl destination advertisement
            else do
                defaultBranchPath <- chooseDefaultBranch advertisement branchRefs
                let branchName = branchShortName defaultBranchPath
                validateBranchPath defaultBranchPath
                validateBranchPath ("refs/heads/" ++ branchName)
                repository <- initRepositoryWithFormat objectFormat branchName destination
                headObjectId <- requireDefaultHead defaultBranchPath branchRefs
                let wantedIds = nub (map discoveryRefObjectId (branchRefs ++ tagRefs))
                withTemporaryPack $ \packPath -> do
                    transportUploadPackToFile
                        transport
                        (discoveryCapabilities advertisement)
                        wantedIds
                        []
                        packPath
                    importPackFile objectFormat repository packPath
                store <- loadStore repository
                checkoutTree store repository headObjectId
                writeCheckoutIndex store repository headObjectId
                writeCloneRefs repository defaultBranchPath headObjectId branchRefs tagRefs
                writeRemoteConfig repository remoteUrl defaultBranchPath
                pure (CloneResult (Just headObjectId) defaultBranchPath)

cloneEmptyRepository
    :: HashAlgorithm
    -> String
    -> FilePath
    -> DiscoveryAdvertisement
    -> IO CloneResult
cloneEmptyRepository objectFormat remoteUrl destination advertisement = do
    let branchPath = defaultBranchForEmpty advertisement
    validateBranchPath branchPath
    repository <- initRepositoryWithFormat objectFormat (branchShortName branchPath) destination
    writeHeadReference repository (symbolicHead branchPath)
    writeRemoteConfig repository remoteUrl branchPath
    pure (CloneResult Nothing branchPath)

defaultBranchForEmpty :: DiscoveryAdvertisement -> FilePath
defaultBranchForEmpty advertisement =
    case symrefTarget advertisement of
        Just target -> BS8.unpack target
        Nothing -> "refs/heads/" ++ defaultBranchName

defaultBranchName :: String
defaultBranchName = "main"

requireDefaultHead :: FilePath -> [DiscoveryRef] -> IO ObjectId
requireDefaultHead defaultBranchPath branchRefs =
    case find ((== BS8.pack defaultBranchPath) . discoveryRefName) branchRefs of
        Just branchRef -> pure (discoveryRefObjectId branchRef)
        Nothing -> ioError (userError "remote advertisement lost its default branch")

chooseDefaultBranch :: DiscoveryAdvertisement -> [DiscoveryRef] -> IO FilePath
chooseDefaultBranch advertisement branchRefs =
    case symrefTarget advertisement of
        Just target
            | isBranchRef target && advertisedBy target -> pure (BS8.unpack target)
        _ -> fallback
  where
    advertisedBy target = any ((== target) . discoveryRefName) branchRefs

    fallback =
        case find ((== BS8.pack "HEAD") . discoveryRefName) (discoveryRefs advertisement) of
            Just headRef ->
                case find ((== discoveryRefObjectId headRef) . discoveryRefObjectId) branchRefs of
                    Just branchRef -> pure (BS8.unpack (discoveryRefName branchRef))
                    Nothing -> firstBranch
            Nothing -> firstBranch

    firstBranch =
        case branchRefs of
            branchRef:_ -> pure (BS8.unpack (discoveryRefName branchRef))
            [] -> ioError (userError "remote advertises no branches")

symrefTarget :: DiscoveryAdvertisement -> Maybe ByteString
symrefTarget advertisement = do
    value <- capabilityValue (BS8.pack "symref") (discoveryCapabilities advertisement)
    case BS8.break (== ':') value of
        (headName, rest)
            | headName == BS8.pack "HEAD" && not (BS.null rest) -> Just (BS.tail rest)
        _ -> Nothing

checkoutTree :: Store -> Repository -> ObjectId -> IO ()
checkoutTree store repository headObjectId = do
    flattened <- flattenHeadTree store headObjectId
    forM_ flattened $ \(relativePath, entry) ->
        materializeEntry store repository relativePath entry

writeCheckoutIndex :: Store -> Repository -> ObjectId -> IO ()
writeCheckoutIndex store repository headObjectId = do
    flattened <- flattenHeadTree store headObjectId
    entries <- forM flattened $ \(relativePath, entry) -> do
        size <- case treeEntryMode entry of
            Gitlink -> pure 0
            Directory -> pure 0
            _ -> fromIntegral . BS.length <$> blobPayload store entry
        pure (indexEntryFromTree relativePath entry size)
    writeIndexFile
        (repositoryObjectFormat repository)
        (repositoryIndexPath repository)
        entries

writeIndexFile :: HashAlgorithm -> FilePath -> [IndexEntry] -> IO ()
writeIndexFile objectFormat path entries =
    BS.writeFile path (writeIndexFor objectFormat entries)

flattenHeadTree :: Store -> ObjectId -> IO [(FilePath, TreeEntry)]
flattenHeadTree store headObjectId = do
    headObject <- readObjectOrFail store headObjectId
    currentCommit <-
        either
            (ioError . userError)
            pure
            (commitFromObjectFor (repositoryObjectFormat (storeRepository store)) headObject)
    flattenTree store (commitTree currentCommit)

materializeEntry :: Store -> Repository -> FilePath -> TreeEntry -> IO ()
materializeEntry store repository relativePath entry = do
    validateRelativePath relativePath
    let destinationPath = repositoryRootDir repository </> relativePath
    case treeEntryMode entry of
        Directory -> pure ()
        Gitlink -> createDirectoryIfMissing True destinationPath
        RegularFile -> writeBlob destinationPath False
        ExecutableFile -> writeBlob destinationPath True
        Symlink -> do
            target <- blobPayload store entry
            createDirectoryIfMissing True (takeDirectory destinationPath)
            removePathIfPresent destinationPath
            createFileLink (BS8.unpack target) destinationPath
  where
    writeBlob destinationPath isExecutable = do
        payload <- blobPayload store entry
        createDirectoryIfMissing True (takeDirectory destinationPath)
        BS.writeFile destinationPath payload
        if isExecutable
            then do
                permissions <- getPermissions destinationPath
                setPermissions destinationPath (permissions {executable = True})
            else pure ()

blobPayload :: Store -> TreeEntry -> IO ByteString
blobPayload store entry = do
    gitObjectValue <- readObjectOrFail store (treeEntryObjectId entry)
    case gitObjectType gitObjectValue of
        BlobObject -> pure (gitObjectPayload gitObjectValue)
        _ ->
            ioError
                ( userError
                    ( "tree entry does not point at a blob: "
                        ++ BS8.unpack (treeEntryName entry)
                    )
                )

validateRelativePath :: FilePath -> IO ()
validateRelativePath relativePath =
    case splitOn '/' relativePath of
        [] -> ioError (userError "tree contains an empty path")
        firstComponent : _
            | firstComponent == "" ->
                ioError (userError ("tree contains an absolute path: " ++ relativePath))
        components
            | any isDotComponent components ->
                ioError (userError ("tree contains a dot path component: " ++ relativePath))
            | otherwise -> pure ()
  where
    isDotComponent component = component == "." || component == ".."

writeCloneRefs
    :: Repository
    -> FilePath
    -> ObjectId
    -> [DiscoveryRef]
    -> [DiscoveryRef]
    -> IO ()
writeCloneRefs repository defaultBranchPath headObjectId branchRefs tagRefs = do
    writeHeadReference repository (symbolicHead defaultBranchPath)
    writeRefValue repository defaultBranchPath headObjectId
    forM_ branchRefs $ \branchRef -> do
        let refPath = BS8.unpack (discoveryRefName branchRef)
        validateBranchPath refPath
        writeRefValue
            repository
            (remoteTrackingRefPath refPath)
            (discoveryRefObjectId branchRef)
    forM_ tagRefs $ \tagRef -> do
        let refPath = BS8.unpack (discoveryRefName tagRef)
        validateBranchPath refPath
        writeRefValue repository refPath (discoveryRefObjectId tagRef)
    writeSymbolicRefValue
        repository
        "refs/remotes/origin/HEAD"
        (remoteTrackingRefPath defaultBranchPath)

writeRemoteConfig :: Repository -> String -> FilePath -> IO ()
writeRemoteConfig repository remoteUrl defaultBranchPath = do
    let branchName = branchShortName defaultBranchPath
    appendFile
        (repositoryConfigPath repository)
        ( unlines
            [ ""
            , "[remote \"origin\"]"
            , "\turl = " ++ remoteUrl
            , "\tfetch = +refs/heads/*:refs/remotes/origin/*"
            , "[branch \"" ++ branchName ++ "\"]"
            , "\tremote = origin"
            , "\tmerge = " ++ defaultBranchPath
            ]
        )

remoteTrackingRefPath :: FilePath -> FilePath
remoteTrackingRefPath branchPath = "refs/remotes/origin/" ++ branchShortName branchPath

branchShortName :: FilePath -> FilePath
branchShortName branchPath
    | "refs/heads/" `isPrefixOf` branchPath = drop (length "refs/heads/") branchPath
    | otherwise = branchPath

prepareDestination :: FilePath -> IO ()
prepareDestination destination = do
    destinationExists <- doesPathExist destination
    if not destinationExists
        then pure ()
        else do
            isDirectory <- doesDirectoryExist destination
            if not isDirectory
                then ioError (userError ("destination exists and is not a directory: " ++ destination))
                else do
                    entries <- listDirectory destination
                    if null entries
                        then pure ()
                        else
                            ioError
                                ( userError
                                    ("destination exists and is not empty: " ++ destination)
                                )

removePathIfPresent :: FilePath -> IO ()
removePathIfPresent path = do
    pathExists <- doesPathExist path
    when pathExists (removePathForcibly path)

validateRemoteUrl :: String -> IO ()
validateRemoteUrl remoteUrl
    | null remoteUrl = ioError (userError "remote URL is empty")
    | any invalidUrlCharacter remoteUrl =
        ioError (userError "remote URL contains whitespace or a control character")
    | otherwise =
        case parseRemoteUrl remoteUrl of
            Left err -> ioError (userError err)
            Right _ -> pure ()
  where
    invalidUrlCharacter character = isSpace character || isControl character

isBranchRef :: ByteString -> Bool
isBranchRef name = BS8.isPrefixOf (BS8.pack "refs/heads/") name

isTagRef :: ByteString -> Bool
isTagRef name = BS8.isPrefixOf (BS8.pack "refs/tags/") name

isPeeledRefName :: ByteString -> Bool
isPeeledRefName name = BS8.isSuffixOf (BS8.pack "^{}") name

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn separator text =
    let (before, rest) = break (== separator) text
     in case rest of
            [] -> [before]
            (_:after) -> before : splitOn separator after

validateBranchPath :: FilePath -> IO ()
validateBranchPath refPath =
    case validateReferenceName refPath of
        Left err -> ioError (userError err)
        Right () -> pure ()
