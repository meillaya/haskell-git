module Git.Refs
    ( HeadReference
    , currentHeadCommit
    , headRefPath
    , readHeadReference
    , readRefValue
    , symbolicHead
    , writeHeadReference
    , updateHeadReference
    , writeRefValue
    ) where

import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Repo (Repository, repositoryGitDir)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

data HeadReference
    = SymbolicHead FilePath
    | DetachedHead ObjectId
    deriving (Eq, Show)

headRefPath :: FilePath
headRefPath = "HEAD"

symbolicHead :: FilePath -> HeadReference
symbolicHead = SymbolicHead

writeHeadReference :: Repository -> HeadReference -> IO ()
writeHeadReference repository headReference =
    case headReference of
        SymbolicHead refPath ->
            writeFile (repositoryGitDir repository </> headRefPath) ("ref: " ++ refPath ++ "\n")
        DetachedHead objectId ->
            writeFile (repositoryGitDir repository </> headRefPath) (objectIdToHex objectId ++ "\n")

readHeadReference :: Repository -> IO HeadReference
readHeadReference repository = do
    contents <- readFile (repositoryGitDir repository </> headRefPath)
    case stripPrefix "ref: " (trimTrailingNewline contents) of
        Just refPath -> pure (SymbolicHead refPath)
        Nothing ->
            case objectIdFromHex (trimTrailingNewline contents) of
                Just objectId -> pure (DetachedHead objectId)
                Nothing -> ioError (userError "Invalid HEAD reference")

writeRefValue :: Repository -> FilePath -> ObjectId -> IO ()
writeRefValue repository refPath objectId = do
    let fullPath = repositoryGitDir repository </> refPath
    createDirectoryIfMissing True (takeDirectory fullPath)
    writeFile fullPath (objectIdToHex objectId ++ "\n")

readRefValue :: Repository -> FilePath -> IO (Maybe ObjectId)
readRefValue repository refPath = do
    let fullPath = repositoryGitDir repository </> refPath
    exists <- doesFileExist fullPath
    if not exists
        then pure Nothing
        else objectIdFromHex . trimTrailingNewline <$> readFile fullPath

updateHeadReference :: Repository -> ObjectId -> IO ()
updateHeadReference repository objectId = do
    headReference <- readHeadReference repository
    case headReference of
        SymbolicHead refPath -> writeRefValue repository refPath objectId
        DetachedHead _ ->
            writeFile (repositoryGitDir repository </> headRefPath) (objectIdToHex objectId ++ "\n")

currentHeadCommit :: Repository -> IO (Maybe ObjectId)
currentHeadCommit repository = do
    headReference <- readHeadReference repository
    case headReference of
        SymbolicHead refPath -> readRefValue repository refPath
        DetachedHead objectId -> pure (Just objectId)

trimTrailingNewline :: String -> String
trimTrailingNewline = reverse . dropWhile (`elem` "\r\n") . reverse

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix _ [] = Nothing
stripPrefix (x:xs) (y:ys)
    | x == y = stripPrefix xs ys
    | otherwise = Nothing
