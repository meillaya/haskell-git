module Git.Reflog
    ( appendReflog
    , reflogMessage
    ) where

import Data.Char (isSpace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO (IOMode (AppendMode), withFile, hPutStr)
import Git.Hash (ObjectId, objectIdToHex, zeroObjectIdFor)
import Git.Repo (Repository (..), repositoryGitDir, repositoryObjectFormat)

-- | Append a Git-compatible reflog entry. The message is kept to one line.
appendReflog
    :: Repository
    -> FilePath
    -> Maybe ObjectId
    -> Maybe ObjectId
    -> String
    -> IO ()
appendReflog repository refPath oldValue newValue message = do
    let logPath = repositoryGitDir repository </> "logs" </> refPath
        format = repositoryObjectFormat repository
        oldId = maybe (zeroObjectIdFor format) id oldValue
        newId = maybe (zeroObjectIdFor format) id newValue
    createDirectoryIfMissing True (takeDirectory logPath)
    name <- lookupEnv "GIT_COMMITTER_NAME"
    email <- lookupEnv "GIT_COMMITTER_EMAIL"
    now <- getCurrentTime
    let identity =
            maybe "haskell-git" id name
                ++ " <"
                ++ maybe "haskell-git@localhost" id email
                ++ ">"
        timestamp = formatTime defaultTimeLocale "%s %z" now
        cleanMessage = map (\character -> if character == '\n' || character == '\r' then ' ' else character) message
    withFile logPath AppendMode $ \handle ->
        hPutStr
            handle
            ( objectIdToHex oldId
                ++ " "
                ++ objectIdToHex newId
                ++ " "
                ++ identity
                ++ " "
                ++ timestamp
                ++ "\t"
                ++ cleanMessage
                ++ "\n"
            )

reflogMessage :: String -> String
reflogMessage = map (\character -> if isSpace character then ' ' else character)
