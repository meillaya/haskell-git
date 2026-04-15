module Git.CLI
    ( banner
    , runCLI
    ) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import Git.Clone (cloneHead, cloneRepository)
import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Object.Blob (blobContents, blobFromBytes, blobFromObject, blobToObject)
import Git.Object.Commit
    ( Commit,
      PersonIdent,
      commit,
      commitAuthor,
      commitCommitter,
      commitFromObject,
      commitMessage,
      commitParents,
      commitToObject,
      commitTree,
      personIdent,
      personIdentEmail,
      personIdentName,
      personIdentTimestamp,
      personIdentTimezone )
import Git.Object.Loose (objectIdForObject, readLooseObject, writeLooseObject)
import Git.Object.Tree (renderTreeEntry, treeFromObject)
import Git.Refs (updateHeadReference)
import Git.Repo (ensureRepository, initRepository, repositoryGitDir)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO (stdout)
import Data.Time.Clock.POSIX (getPOSIXTime)

banner :: String
banner =
    "haskell-git scaffold initialized. Supported commands: init [PATH], hash-object [-w] FILE, cat-file -p OID."

data Command
    = InitCommand FilePath
    | HashObjectCommand Bool FilePath
    | CatFilePrettyCommand String
    | CommitTreeCommand String [String] String
    | CloneCommand String FilePath

runCLI :: IO ()
runCLI = do
    args <- getArgs
    case parseCommand args of
        Left err -> die err
        Right command -> runCommand command

parseCommand :: [String] -> Either String Command
parseCommand [] = Left banner
parseCommand ["init"] = Right (InitCommand ".")
parseCommand ["init", path] = Right (InitCommand path)
parseCommand ["hash-object", filePath] = Right (HashObjectCommand False filePath)
parseCommand ["hash-object", "-w", filePath] = Right (HashObjectCommand True filePath)
parseCommand ["cat-file", "-p", objectId] = Right (CatFilePrettyCommand objectId)
parseCommand ("commit-tree":treeId:rest) = parseCommitTree treeId rest
parseCommand ["clone", remoteUrl, destination] = Right (CloneCommand remoteUrl destination)
parseCommand _ = Left banner

parseCommitTree :: String -> [String] -> Either String Command
parseCommitTree treeId args = go [] Nothing args
  where
    go parents maybeMessage [] =
        case maybeMessage of
            Just message -> Right (CommitTreeCommand treeId (reverse parents) message)
            Nothing -> Left "commit-tree requires -m MESSAGE"
    go parents maybeMessage ("-p":parentId:remaining) = go (parentId : parents) maybeMessage remaining
    go parents Nothing ("-m":message:remaining) = go parents (Just message) remaining
    go _ (Just _) ("-m":_) = Left "commit-tree only supports one -m MESSAGE"
    go _ _ _ = Left "Usage: commit-tree TREE [-p PARENT]... -m MESSAGE"

runCommand :: Command -> IO ()
runCommand (InitCommand path) = do
    repository <- initRepository path
    putStrLn ("Initialized empty Git repository in " ++ show (repositoryGitDir repository))
runCommand (HashObjectCommand shouldWrite filePath) = do
    payload <- BS.readFile filePath
    let objectId = gitObjectId payload
    when shouldWrite $ do
        repository <- ensureRepository "."
        _ <- writeLooseObject repository (blobToObject (blobFromBytes payload))
        pure ()
    putStrLn (objectIdToHex objectId)
runCommand (CatFilePrettyCommand objectIdHex) =
    case objectIdFromHex objectIdHex of
        Nothing -> die ("Invalid object id: " ++ objectIdHex)
        Just objectId -> do
            repository <- ensureRepository "."
            gitObject <- readLooseObject repository objectId
            case blobFromObject gitObject of
                Right blob -> BS.hPut stdout (blobContents blob)
                Left _ ->
                    case treeFromObject gitObject of
                        Right treeEntries ->
                            putStrLn
                                (intercalate "\n" (map renderTreeEntry treeEntries))
                        Left _ ->
                            case commitFromObject gitObject of
                                Right currentCommit -> BS.hPut stdout (renderCommitPretty currentCommit)
                                Left err -> die err
runCommand (CommitTreeCommand treeIdHex parentIdHexes messageText) =
    case (objectIdFromHex treeIdHex, traverse objectIdFromHex parentIdHexes) of
        (Nothing, _) -> die ("Invalid tree object id: " ++ treeIdHex)
        (_, Nothing) -> die "One of the parent object ids is invalid"
        (Just treeId, Just parentIds) -> do
            repository <- ensureRepository "."
            author <- defaultPersonIdent
            let currentCommit = commit treeId parentIds author author (BS8.pack messageText)
            commitId <- writeLooseObject repository (commitToObject currentCommit)
            updateHeadReference repository commitId
            putStrLn (objectIdToHex commitId)
runCommand (CloneCommand remoteUrl destination) = do
    cloneResult <- cloneRepository remoteUrl destination
    putStrLn ("Cloned " ++ objectIdToHex (cloneHead cloneResult) ++ " into " ++ destination)

gitObjectId :: BS.ByteString -> ObjectId
gitObjectId payload =
    let gitObject = blobToObject (blobFromBytes payload)
     in objectIdForObject gitObject

defaultPersonIdent :: IO PersonIdent
defaultPersonIdent = do
    timestamp <- round <$> getPOSIXTime
    authorName <- BS8.pack <$> lookupEnvDefault "GIT_AUTHOR_NAME" "haskell-git"
    authorEmail <- BS8.pack <$> lookupEnvDefault "GIT_AUTHOR_EMAIL" "haskell-git@example.com"
    pure (personIdent authorName authorEmail timestamp (BS8.pack "+0000"))

renderCommitPretty :: Commit -> BS.ByteString
renderCommitPretty currentCommit =
    BS8.intercalate
        (BS8.pack "\n")
        ( [ BS8.pack ("tree " ++ objectIdToHex (commitTree currentCommit))
          ]
            ++ map (\parentId -> BS8.pack ("parent " ++ objectIdToHex parentId)) (commitParents currentCommit)
            ++ [ BS8.pack "author " <> renderPerson (commitAuthor currentCommit)
               , BS8.pack "committer " <> renderPerson (commitCommitter currentCommit)
               , BS.empty
               , commitMessage currentCommit
               ]
        )

renderPerson :: PersonIdent -> BS.ByteString
renderPerson ident =
    personIdentName ident
        <> BS8.pack " <"
        <> personIdentEmail ident
        <> BS8.pack "> "
        <> BS8.pack (show (personIdentTimestamp ident))
        <> BS8.pack " "
        <> personIdentTimezone ident

lookupEnvDefault :: String -> String -> IO String
lookupEnvDefault key fallbackValue = do
    maybeValue <- lookupEnv key
    pure (maybe fallbackValue id maybeValue)
