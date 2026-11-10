module Git.TransportSpec
    ( run
    ) where

import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as BS8
import Git.Clone (CloneResult (..), cloneRepository)
import Git.Config (loadGlobalConfig)
import Git.Fetch (defaultFetchOptions, fetchRepository)
import Git.Protocol.SmartHTTP
    ( discoveryCapabilities
    , discoveryRefObjectId
    , discoveryRefs
    )
import Git.Push (defaultPushOptions, pushRepository)
import Git.Repo (Repository, findRepository)
import Git.Transport
    ( FetchProgress (..)
    , Service (..)
    , Transport (..)
    , transportUploadPackToFileFor
    , withTemporaryPack
    , withTransport
    )
import System.Directory (createDirectoryIfMissing, getFileSize)
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
    , assertContains
    , assertEqual
    , runBash
    , runGit
    , withHttpBackendServerV2
    , withTempDirectory
    )

run :: IO ()
run = do
    sshCloneAndFetch
    sshProtocolV2CloneAndFetch
    sshProtocolV0FallbackClone
    sshPush
    gitDaemonCloneAndFetch
    gitDaemonV2ProgressReporting
    gitDaemonPush
    httpProtocolV2CloneAndFetch
    httpProtocolV2Sha256Clone

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

-- | The fake ssh server refuses to serve unless the client forwarded
-- @GIT_PROTOCOL=version=2@, so a successful clone proves the SSH transport
-- negotiated protocol v2.
sshProtocolV2CloneAndFetch :: IO ()
sshProtocolV2CloneAndFetch =
    withTempDirectory "haskell-git-ssh-v2" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            bareRepository = temporaryDir </> "repo.git"
            destination = temporaryDir </> "clone"
            sshScript = temporaryDir </> "fake-ssh-v2.sh"
        _ <- createSource sourceDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        writeFile
            sshScript
            ( unlines
                [ "#!/bin/sh"
                , "if [ \"$GIT_PROTOCOL\" != \"version=2\" ]; then"
                , "  echo 'expected GIT_PROTOCOL=version=2' >&2"
                , "  exit 97"
                , "fi"
                , "exec git-upload-pack " ++ show bareRepository ++ " 2>/dev/null"
                ]
            )
        _ <- runBash ("chmod +x " ++ sshScript)
        withEnvironment "GIT_SSH_COMMAND" sshScript $ do
            result <- cloneRepository "ssh://example/repo.git" destination
            assert (cloneHead result /= Nothing) "SSH v2 clone should fetch a HEAD"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "SSH v2 clone should be clean"
            _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
            writeFile (sourceDirectory </> "second.txt") "second\n"
            _ <- runGit ["-C", sourceDirectory, "add", "-A"]
            _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "second"]
            _ <- runGit ["-C", sourceDirectory, "push", "-q", bareRepository, "main"]
            repository <- requireRepository destination
            _ <- fetchRepository repository "origin" [] defaultFetchOptions
            fetchedHead <- trim <$> runGit ["-C", destination, "rev-parse", "refs/remotes/origin/main"]
            sourceHead <- trim <$> runGit ["-C", sourceDirectory, "rev-parse", "HEAD"]
            assertEqual sourceHead fetchedHead "SSH v2 fetch should update the tracking ref"
            _ <- runGit ["-C", destination, "fsck", "--strict"]
            pure ()

-- | A server that ignores the forwarded @GIT_PROTOCOL@ answers with a v0
-- advertisement; the transport must fall back to v0 instead of failing.
sshProtocolV0FallbackClone :: IO ()
sshProtocolV0FallbackClone =
    withTempDirectory "haskell-git-ssh-v0" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            bareRepository = temporaryDir </> "repo.git"
            destination = temporaryDir </> "clone"
            sshScript = temporaryDir </> "fake-ssh-v0.sh"
        _ <- createSource sourceDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        writeFile
            sshScript
            ( unlines
                [ "#!/bin/sh"
                , "unset GIT_PROTOCOL"
                , "exec git-upload-pack " ++ show bareRepository ++ " 2>/dev/null"
                ]
            )
        _ <- runBash ("chmod +x " ++ sshScript)
        withEnvironment "GIT_SSH_COMMAND" sshScript $ do
            result <- cloneRepository "ssh://example/repo.git" destination
            assert (cloneHead result /= Nothing) "SSH v0 fallback clone should fetch a HEAD"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "SSH v0 fallback clone should be clean"
            _ <- runGit ["-C", destination, "fsck", "--strict"]
            pure ()

sshPush :: IO ()
sshPush =
    withTempDirectory "haskell-git-ssh-push" $ \temporaryDir -> do
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
                , "case \"$*\" in"
                , "  *git-receive-pack*) exec git-receive-pack " ++ show bareRepository ++ " 2>/dev/null ;;"
                , "  *) exec git-upload-pack " ++ show bareRepository ++ " 2>/dev/null ;;"
                , "esac"
                ]
            )
        _ <- runBash ("chmod +x " ++ sshScript)
        withEnvironment "GIT_SSH_COMMAND" sshScript $ do
            _ <- cloneRepository "ssh://example/repo.git" destination
            _ <- runGit ["-C", destination, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", destination, "config", "user.name", "Ada"]
            writeFile (destination </> "pushed.txt") "pushed\n"
            _ <- runGit ["-C", destination, "add", "-A"]
            _ <- runGit ["-C", destination, "commit", "-qm", "ssh push"]
            repository <- requireRepository destination
            _ <- pushRepository repository "origin" ["refs/heads/main:refs/heads/main"] defaultPushOptions
            remoteText <- runGit ["-C", bareRepository, "rev-parse", "refs/heads/main"]
            localText <- runGit ["-C", destination, "rev-parse", "HEAD"]
            assertEqual localText remoteText "SSH push should update the remote branch"

httpProtocolV2CloneAndFetch :: IO ()
httpProtocolV2CloneAndFetch =
    withTempDirectory "haskell-git-http-v2" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withHttpBackendServerV2 serveDirectory "repo.git" $ \remoteUrl -> do
            result <- cloneRepository remoteUrl destination
            assert (cloneHead result /= Nothing) "protocol v2 clone should fetch a HEAD"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "protocol v2 clone should be clean"
            _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
            writeFile (sourceDirectory </> "second.txt") "second\n"
            _ <- runGit ["-C", sourceDirectory, "add", "-A"]
            _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "second"]
            _ <- runGit ["-C", sourceDirectory, "push", "-q", bareRepository, "main"]
            repository <- requireRepository destination
            _ <- fetchRepository repository "origin" [] defaultFetchOptions
            fetchedHead <- trim <$> runGit ["-C", destination, "rev-parse", "refs/remotes/origin/main"]
            sourceHead <- trim <$> runGit ["-C", sourceDirectory, "rev-parse", "HEAD"]
            assertEqual sourceHead fetchedHead "protocol v2 fetch should update the tracking ref"
            _ <- runGit ["-C", destination, "fsck", "--strict"]
            pure ()

-- | SHA-256 repositories over protocol v2: the object format comes from the
-- advertisement capability, which v2 terminates with a newline.
httpProtocolV2Sha256Clone :: IO ()
httpProtocolV2Sha256Clone =
    withTempDirectory "haskell-git-http-v2-sha256" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- runGit ["init", "-q", "-b", "main", "--object-format=sha256", sourceDirectory]
        _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
        writeFile (sourceDirectory </> "hello.txt") "hello sha256\n"
        _ <- runGit ["-C", sourceDirectory, "add", "-A"]
        _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "first"]
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withHttpBackendServerV2 serveDirectory "repo.git" $ \remoteUrl -> do
            result <- cloneRepository remoteUrl destination
            assert (cloneHead result /= Nothing) "protocol v2 sha256 clone should fetch a HEAD"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "protocol v2 sha256 clone should be clean"
            _ <- runGit ["-C", destination, "fsck", "--strict"]
            format <- trim <$> runGit ["-C", destination, "rev-parse", "--show-object-format"]
            assertEqual "sha256" format "protocol v2 clone should keep the sha256 object format"
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
                _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
                _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
                writeFile (sourceDirectory </> "second.txt") "second\n"
                _ <- runGit ["-C", sourceDirectory, "add", "-A"]
                _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "second"]
                _ <- runGit ["-C", sourceDirectory, "push", "-q", bareRepository, "main"]
                repository <- requireRepository destination
                _ <- fetchRepository repository "origin" [] defaultFetchOptions
                fetchedHead <- trim <$> runGit ["-C", destination, "rev-parse", "refs/remotes/origin/main"]
                sourceHead <- trim <$> runGit ["-C", sourceDirectory, "rev-parse", "HEAD"]
                assertEqual sourceHead fetchedHead "protocol v2 git:// fetch should update the tracking ref"
                _ <- runGit ["-C", destination, "fsck", "--strict"]
                pure ()
            )

-- | A protocol v2 fetch over git:// must surface the server's sideband
-- progress messages and report the number of pack bytes received.
gitDaemonV2ProgressReporting :: IO ()
gitDaemonV2ProgressReporting =
    withTempDirectory "haskell-git-gitdaemon-progress" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        port <- trim <$> runBash "python3 -c 'import socket; s=socket.socket(); s.bind((\"127.0.0.1\",0)); print(s.getsockname()[1]); s.close()'"
        bracket
            (startDaemon serveDirectory port)
            stopDaemon
            (\_ -> do
                recordedMessages <- newIORef []
                recordedBytes <- newIORef 0
                let progress =
                        FetchProgress
                            (\bytes -> writeIORef recordedBytes bytes)
                            (\message -> modifyIORef' recordedMessages (message :))
                config <- loadGlobalConfig
                withTransport config ("git://127.0.0.1:" ++ port ++ "/repo.git") $ \transport -> do
                    advertisement <- transportDiscovery transport UploadPackService
                    withTemporaryPack $ \packPath -> do
                        transportUploadPackToFileFor
                            transport
                            advertisement
                            (discoveryCapabilities advertisement)
                            (map discoveryRefObjectId (discoveryRefs advertisement))
                            []
                            progress
                            packPath
                        packSize <- getFileSize packPath
                        reportedBytes <- readIORef recordedBytes
                        assertEqual
                            (fromIntegral packSize)
                            reportedBytes
                            "the transport should report every received pack byte"
                        messages <- readIORef recordedMessages
                        assert (not (null messages)) "protocol v2 should deliver remote progress messages"
                        let messageText = BS8.unpack (BS8.concat (reverse messages))
                        assertContains "objects" messageText "remote progress should mention objects"
            )

gitDaemonPush :: IO ()
gitDaemonPush =
    withTempDirectory "haskell-git-gitdaemon-push" $ \temporaryDir -> do
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
                _ <- cloneRepository ("git://127.0.0.1:" ++ port ++ "/repo.git") destination
                _ <- runGit ["-C", destination, "config", "user.email", "ada@example.com"]
                _ <- runGit ["-C", destination, "config", "user.name", "Ada"]
                writeFile (destination </> "pushed.txt") "pushed\n"
                _ <- runGit ["-C", destination, "add", "-A"]
                _ <- runGit ["-C", destination, "commit", "-qm", "git push"]
                repository <- requireRepository destination
                _ <- pushRepository repository "origin" ["refs/heads/main:refs/heads/main"] defaultPushOptions
                remoteText <- runGit ["-C", bareRepository, "rev-parse", "refs/heads/main"]
                localText <- runGit ["-C", destination, "rev-parse", "HEAD"]
                assertEqual localText remoteText "git:// push should update the remote branch"
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
                , "--enable=receive-pack"
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
