module Git.Refs
    ( HeadReference (..)
    , currentHeadCommit
    , headRefPath
    , listLooseRefs
    , listRefs
    , readHeadReference
    , readPackedRefs
    , readRefValue
    , resolveHeadReference
    , resolveRevision
    , symbolicHead
    , updateHeadReference
    , writeHeadReference
    , writeRefValue
    , writeSymbolicRefValue
    ) where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Repo
    ( Repository
    , repositoryGitDir
    , repositoryPackedRefsPath
    , repositoryRefsDir
    , validateReferenceName
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , renameFile
    )
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hClose, hPutStr, openTempFile)

data HeadReference
    = SymbolicHead FilePath
    | DetachedHead ObjectId
    deriving (Eq, Show)

headRefPath :: FilePath
headRefPath = "HEAD"

symbolicHead :: FilePath -> HeadReference
symbolicHead = SymbolicHead

headPath :: Repository -> FilePath
headPath repository = repositoryGitDir repository </> headRefPath

writeHeadReference :: Repository -> HeadReference -> IO ()
writeHeadReference repository headReference =
    case headReference of
        SymbolicHead refPath -> do
            validateRefPath refPath
            writeFileAtomically (headPath repository) ("ref: " ++ refPath ++ "\n")
        DetachedHead objectId ->
            writeFileAtomically (headPath repository) (objectIdToHex objectId ++ "\n")

readHeadReference :: Repository -> IO HeadReference
readHeadReference repository = do
    contents <- readFile (headPath repository)
    case stripPrefix "ref: " (trimTrailingNewline contents) of
        Just refPath -> pure (SymbolicHead refPath)
        Nothing ->
            case objectIdFromHex (trimTrailingNewline contents) of
                Just objectId -> pure (DetachedHead objectId)
                Nothing -> ioError (userError "HEAD does not name a reference or an object")

-- | Write a loose reference. The name is validated, so an absolute path or a
-- dot sequence from a remote advertisement cannot escape the repository.
writeRefValue :: Repository -> FilePath -> ObjectId -> IO ()
writeRefValue repository refPath objectId = do
    validateRefPath refPath
    let fullPath = repositoryGitDir repository </> refPath
    createDirectoryIfMissing True (takeDirectory fullPath)
    writeFileAtomically fullPath (objectIdToHex objectId ++ "\n")

-- | Read a loose reference. Packed references are read by 'listRefs'; use
-- 'resolveRevision' or 'lookupRef' when both kinds should be considered.
readRefValue :: Repository -> FilePath -> IO (Maybe ObjectId)
readRefValue repository refPath
    | Left _ <- validateReferenceName refPath = pure Nothing
    | otherwise = do
        let fullPath = repositoryGitDir repository </> refPath
        exists <- doesFileExist fullPath
        if not exists then pure Nothing else readReferenceFile fullPath

updateHeadReference :: Repository -> ObjectId -> IO ()
updateHeadReference repository objectId = do
    headReference <- readHeadReference repository
    case headReference of
        SymbolicHead refPath -> writeRefValue repository refPath objectId
        DetachedHead _ ->
            writeFileAtomically (headPath repository) (objectIdToHex objectId ++ "\n")

-- | Write a symbolic reference, such as @refs/remotes/origin/HEAD@, which
-- points at another reference instead of an object.
writeSymbolicRefValue :: Repository -> FilePath -> FilePath -> IO ()
writeSymbolicRefValue repository refPath targetRef = do
    validateRefPath refPath
    validateRefPath targetRef
    let fullPath = repositoryGitDir repository </> refPath
    createDirectoryIfMissing True (takeDirectory fullPath)
    writeFileAtomically fullPath ("ref: " ++ targetRef ++ "\n")

currentHeadCommit :: Repository -> IO (Maybe ObjectId)
currentHeadCommit = resolveHeadReference

-- | Resolve HEAD through any chain of symbolic references. A branch that does
-- not exist yet resolves to Nothing, which is the state after init.
resolveHeadReference :: Repository -> IO (Maybe ObjectId)
resolveHeadReference repository = follow (maxSymbolicDepth :: Int)
  where
    follow 0 = pure Nothing
    follow depth = do
        headReference <- readHeadReference repository
        case headReference of
            DetachedHead objectId -> pure (Just objectId)
            SymbolicHead refPath -> lookupRefWithDepth (depth - 1) repository refPath

-- | Every reference in the repository, loose files first, so a loose
-- reference overrides a packed one.
listRefs :: Repository -> IO [(FilePath, ObjectId)]
listRefs repository = do
    loose <- listLooseRefs repository
    packed <- readPackedRefs repository
    pure (Map.toList (Map.union (Map.fromList loose) (Map.fromList packed)))

listLooseRefs :: Repository -> IO [(FilePath, ObjectId)]
listLooseRefs repository = do
    let refsDirectory = repositoryRefsDir repository
    exists <- doesDirectoryExist refsDirectory
    if not exists then pure [] else walk refsDirectory ""
  where
    walk directory prefix = do
        entries <- listDirectory directory
        fmap concat $
            forM entries $ \entry -> do
                let fullPath = directory </> entry
                    relativeName = if null prefix then entry else prefix ++ "/" ++ entry
                isDirectory <- doesDirectoryExist fullPath
                if isDirectory
                    then walk fullPath relativeName
                    else do
                        maybeObjectId <- readReferenceFile fullPath
                        pure
                            ( case maybeObjectId of
                                Just objectId -> [("refs/" ++ relativeName, objectId)]
                                Nothing -> []
                            )

readPackedRefs :: Repository -> IO [(FilePath, ObjectId)]
readPackedRefs repository = do
    let packedRefsPath = repositoryPackedRefsPath repository
    exists <- doesFileExist packedRefsPath
    if not exists
        then pure []
        else do
            contents <- readFile packedRefsPath
            pure (mapMaybe parseLine (lines contents))
  where
    parseLine line
        | null stripped = Nothing
        | head stripped `elem` "#^" = Nothing
        | otherwise =
            case words stripped of
                [objectIdText, refName] ->
                    case objectIdFromHex objectIdText of
                        Just objectId -> Just (refName, objectId)
                        Nothing -> Nothing
                _ -> Nothing
      where
        stripped = trim line

-- | Resolve a revision the way git does for the forms this tool accepts: HEAD,
-- a full reference name, a branch or tag shorthand, or a full object id.
resolveRevision :: Repository -> String -> IO (Maybe ObjectId)
resolveRevision repository revision
    | null revision = pure Nothing
    | revision == "HEAD" = resolveHeadReference repository
    | otherwise =
        case objectIdFromHex revision of
            Just objectId -> pure (Just objectId)
            Nothing -> firstJustM (map lookupCandidate (candidateRefs revision))
  where
    lookupCandidate refPath = lookupRef repository refPath

candidateRefs :: String -> [FilePath]
candidateRefs revision =
    [ revision
    , "refs/heads/" ++ revision
    , "refs/tags/" ++ revision
    , "refs/remotes/" ++ revision
    , "refs/remotes/origin/" ++ revision
    ]

lookupRef :: Repository -> FilePath -> IO (Maybe ObjectId)
lookupRef repository refPath = lookupRefWithDepth maxSymbolicDepth repository refPath

lookupRefWithDepth :: Int -> Repository -> FilePath -> IO (Maybe ObjectId)
lookupRefWithDepth depth repository refPath
    | depth <= 0 = pure Nothing
    | Left _ <- validateReferenceName refPath = pure Nothing
    | otherwise = do
        let fullPath = repositoryGitDir repository </> refPath
        exists <- doesFileExist fullPath
        if exists
            then do
                contents <- readFile fullPath
                let firstLine = trimTrailingNewline contents
                case stripPrefix "ref: " firstLine of
                    Just targetRef -> lookupRefWithDepth (depth - 1) repository targetRef
                    Nothing -> pure (objectIdFromHex (trim firstLine))
            else lookupPacked refPath
  where
    lookupPacked name = do
        packed <- readPackedRefs repository
        pure (lookup name packed)

readReferenceFile :: FilePath -> IO (Maybe ObjectId)
readReferenceFile path = do
    contents <- readFile path
    pure (objectIdFromHex (trimTrailingNewline contents))

writeFileAtomically :: FilePath -> String -> IO ()
writeFileAtomically path contents = do
    createDirectoryIfMissing True (takeDirectory path)
    (temporaryPath, handle) <- openTempFile (takeDirectory path) (takeFileName path ++ ".tmp")
    hPutStr handle contents
    hClose handle
    renameFile temporaryPath path

validateRefPath :: FilePath -> IO ()
validateRefPath refPath =
    case validateReferenceName refPath of
        Left err -> ioError (userError err)
        Right () -> pure ()

maxSymbolicDepth :: Int
maxSymbolicDepth = 10

firstJustM :: [IO (Maybe a)] -> IO (Maybe a)
firstJustM [] = pure Nothing
firstJustM (action:rest) = do
    result <- action
    case result of
        Just value -> pure (Just value)
        Nothing -> firstJustM rest

trim :: String -> String
trim = trimTrailingNewline . dropWhile isSpaceCharacter
  where
    isSpaceCharacter character = character == ' ' || character == '\t'

trimTrailingNewline :: String -> String
trimTrailingNewline = reverse . dropWhile (`elem` "\r\n") . reverse

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix _ [] = Nothing
stripPrefix (x:xs) (y:ys)
    | x == y = stripPrefix xs ys
    | otherwise = Nothing
