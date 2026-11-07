module Git.TransportSpec
    ( run
    ) where

import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Git.Clone (CloneResult (..), cloneRepository)
import Git.Fetch (defaultFetchOptions, fetchRepository)
import Git.Repo (Repository, findRepository)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Process
    ( CreateProcess (std_err, std_out)
    , ProcessHandle
    , StdStream (CreatePipe)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import TestSupport
    ( assert
    , assertEqual
    , runBash
    , runGit
    , withTempDirectory
    )

run :: IO ()
run = do
    sshCloneAndFetch
    gitDaemonCloneAndFetch

sshCloneAndFetch :: IO ()
sshCloneAndFetch =
    withTempDirectory "haskell-git-ssh" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            bareRepository = temporaryDir </> "repo.git"
            destination = temporaryDir </> "clone"
            sshScript = temporaryDir </> "fake-ssh.sh"
        _ <- createSource sourceDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        writeFile
            sshScript
            ( unlines
                [ "#!/bin/sh"
                , "exec git-upload-pack " ++ show bareRepository ++ " 2>/dev/null"
                ]
            )
        _ <- runBash ("chmod +x " ++ sshScript)
        withEnvironment "GIT_SSH_COMMAND" sshScript $ do
            result <- cloneRepository "ssh://example/repo.git" destination
            assert (cloneHead result /= Nothing) "SSH clone should fetch a HEAD"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "SSH clone should be clean"
            _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
            writeFile (sourceDirectory </> "second.txt") "second\n"
            _ <- runGit ["-C", sourceDirectory, "add", "-A"]
            _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "second"]
            _ <- runGit ["-C", sourceDirectory, "push", "-q", bareRepository, "main"]
            repository <- requireRepository destination
            _ <- fetchRepository repository "origin" [] defaultFetchOptions
            _ <- runGit ["-C", destination, "fsck", "--strict"]
            pure ()

gitDaemonCloneAndFetch :: IO ()
gitDaemonCloneAndFetch =
    withTempDirectory "haskell-git-gitdaemon" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        port <- trim <$> runBash "python3 -c 'import socket; s=socket.socket(); s.bind((\"127.0.0.1\",0)); print(s.getsockname()[1]); s.close()'"
        bracket
            (startDaemon serveDirectory port)
            stopDaemon
            (\_ -> do
                result <- cloneRepository ("git://127.0.0.1:" ++ port ++ "/repo.git") destination
                assert (cloneHead result /= Nothing) "git:// clone should fetch a HEAD"
                statusText <- runGit ["-C", destination, "status", "--porcelain"]
                assertEqual "" statusText "git:// clone should be clean"
                _ <- runGit ["-C", destination, "fsck", "--strict"]
                pure ()
            )

startDaemon :: FilePath -> String -> IO ProcessHandle
startDaemon serveDirectory port = do
    (_, _, _, processHandle) <-
        createProcess
            ( proc
                "git"
                [ "daemon"
                , "--reuseaddr"
                , "--export-all"
                , "--base-path=" ++ serveDirectory
                , "--port=" ++ port
                , "--listen=127.0.0.1"
                ]
            )
                { std_out = CreatePipe
                , std_err = CreatePipe
                }
    threadDelay 300000
    pure processHandle

stopDaemon :: ProcessHandle -> IO ()
stopDaemon processHandle = do
    terminateProcess processHandle
    _ <- waitForProcess processHandle
    pure ()

createSource :: FilePath -> IO ()
createSource directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "hello.txt") "hello transport\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "first"]
    pure ()

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value action =
    bracket
        (do previous <- lookupEnv name; setEnv name value; pure previous)
        restore
        (\_ -> action)
  where
    restore Nothing = unsetEnv name
    restore (Just previous) = setEnv name previous

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
