module Git.Commit
    ( commitCurrentIndex
    , commitIndexWithParents
    , commitIndexWithParentsAuthor
    , resolveIdentity
    ) where

import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (catMaybes)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.LocalTime (getCurrentTimeZone, timeZoneMinutes)
import System.Directory (doesFileExist, removeFile)
import System.Environment (lookupEnv)
import Git.Hash (ObjectId, objectIdFromHexFor)
import Git.Index
    ( IndexEntry (..)
    , buildTreeFromIndex
    , parseIndexFor
    , writeIndexFor
    )
import Git.Object.Commit
    ( commit
    , commitToObject
    , personIdent
    )
import Git.Object.Loose (writeLooseObject)
import Git.Object.Tree (treeToObject)
import Git.Object.Person (PersonIdent)
import Git.Refs
    ( resolveHeadReference
    , updateHeadReference
    )
import Git.Repo
    ( Repository
    , readConfigValue
    , repositoryIndexPath
    , repositoryMergeHeadPath
    , repositoryMergeMsgPath
    , repositoryObjectFormat
    , repositoryOrigHeadPath
    )
import Git.Store (Store)

-- | Commit the index. For a merge, MERGE_HEAD supplies the second parent and
-- MERGE_MSG supplies a default message when none is given.
commitCurrentIndex :: Repository -> Store -> String -> IO ObjectId
commitCurrentIndex repository _store messageText = do
    parents <- currentParents repository
    commitIndexWithParents repository parents messageText

commitIndexWithParents :: Repository -> [ObjectId] -> String -> IO ObjectId
commitIndexWithParents repository parents messageText = do
    author <- resolveIdentity repository "GIT_AUTHOR" "AUTHOR"
    committer <- resolveIdentity repository "GIT_COMMITTER" "COMMITTER"
    commitIndexWithParentsAuthor repository parents author committer messageText

commitIndexWithParentsAuthor
    :: Repository
    -> [ObjectId]
    -> PersonIdent
    -> PersonIdent
    -> String
    -> IO ObjectId
commitIndexWithParentsAuthor repository parents author committer messageText = do
    entries <- readIndexEntries repository
    let unmerged = [entry | entry <- entries, indexEntryStage entry /= 0]
    when (not (null unmerged)) $
        ioError (userError "cannot commit: the index has unresolved conflicts")
    let resolved = [entry | entry <- entries, indexEntryStage entry == 0]
    treeId <-
        if null resolved
            then writeLooseObject repository (treeToObject [])
            else buildTreeFromIndex repository resolved
    let message = normalizeMessage (BS8.pack messageText)
        currentCommit = commit treeId parents author committer message
    commitId <- writeLooseObject repository (commitToObject currentCommit)
    updateHeadReference repository commitId
    clearMergeState repository
    pure commitId

currentParents :: Repository -> IO [ObjectId]
currentParents repository = do
    headValue <- resolveHeadReference repository
    mergeValue <- readMergeHead repository
    pure (catMaybes [headValue, mergeValue])

readMergeHead :: Repository -> IO (Maybe ObjectId)
readMergeHead repository = do
    let path = repositoryMergeHeadPath repository
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            contents <- readFile path
            pure
                ( objectIdFromHexFor
                    (repositoryObjectFormat repository)
                    (trim contents)
                )

readIndexEntries :: Repository -> IO [IndexEntry]
readIndexEntries repository = do
    let path = repositoryIndexPath repository
    exists <- doesFileExist path
    if not exists
        then ioError (userError "nothing to commit: the index is empty")
        else do
            indexBytes <- BS.readFile path
            either
                (ioError . userError . (("index " ++ path ++ ": ") ++))
                pure
                (parseIndexFor (repositoryObjectFormat repository) indexBytes)

resolveIdentity :: Repository -> String -> String -> IO PersonIdent
resolveIdentity repository environmentPrefix configSuffix = do
    nameValue <- identityValue "NAME" "name"
    emailValue <- identityValue "EMAIL" "email"
    timestamp <- round <$> getPOSIXTime
    timeZone <- getCurrentTimeZone
    pure
        ( personIdent
            (stringToUtf8 nameValue)
            (stringToUtf8 emailValue)
            timestamp
            (timezoneText (timeZoneMinutes timeZone))
        )
  where
    identityValue environmentSuffix configKey = do
        environmentValue <- lookupEnvCompat (environmentPrefix ++ "_" ++ environmentSuffix)
        case environmentValue of
            Just value -> pure value
            Nothing -> do
                configValue <- readConfigValue repository "user" configKey
                case configValue of
                    Just value -> pure value
                    Nothing ->
                        ioError
                            ( userError
                                ( "cannot determine identity: set "
                                    ++ environmentPrefix
                                    ++ "_"
                                    ++ environmentSuffix
                                    ++ " or user."
                                    ++ configKey
                                )
                            )

timezoneText :: Int -> BS.ByteString
timezoneText offsetMinutes =
    let sign = if offsetMinutes < 0 then "-" else "+"
        absoluteMinutes = abs offsetMinutes
        hours = absoluteMinutes `div` 60
        minutes = absoluteMinutes `mod` 60
        padTwo value = if value < 10 then '0' : show value else show value
     in BS8.pack (sign ++ padTwo hours ++ padTwo minutes)

trim :: String -> String
trim = reverse . dropWhile (`elem` " \r\n\t") . reverse . dropWhile (`elem` " \r\n\t")

normalizeMessage :: BS.ByteString -> BS.ByteString
normalizeMessage message
    | BS.null message = BS8.singleton '\n'
    | BS8.last message == '\n' = message
    | otherwise = message <> BS8.singleton '\n'

stringToUtf8 :: String -> BS.ByteString
stringToUtf8 = BS8.pack

clearMergeState :: Repository -> IO ()
clearMergeState repository =
    forM_
        [repositoryMergeHeadPath repository, repositoryMergeMsgPath repository]
        removeIfPresent

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
    exists <- doesFileExist path
    when exists (removeFile path)

lookupEnvCompat :: String -> IO (Maybe String)
lookupEnvCompat = lookupEnv
