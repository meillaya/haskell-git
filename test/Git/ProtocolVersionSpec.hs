module Git.ProtocolVersionSpec
    ( run
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.List (isInfixOf)
import Git.Clone (CloneResult (..), cloneRepository)
import Git.Fetch (defaultFetchOptions, fetchRepository)
import Git.Repo (Repository, findRepository)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (die)
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
    , withTempDirectory
    )

-- | Independent, black-box verification that the SSH and git:// transports
-- negotiate Git protocol version 2 and that they gracefully fall back to
-- protocol v0 when the server speaks only v0.
--
-- Nothing here inspects the Haskell implementation: every check drives the
-- public transport entry points (clone/fetch) against a real server and then
-- looks for wire-level evidence.
--
-- SSH: a fake @GIT_SSH_COMMAND@ wrapper records the @GIT_PROTOCOL@ environment
-- variable the client passes to the server process. In the v2 test the wrapper
-- refuses to run @git-upload-pack@ unless it sees @GIT_PROTOCOL=version=2@, so
-- a successful clone is itself proof of the v2 request. In the fallback test
-- the wrapper records that the client offered v2 and then unsets the variable
-- before exec'ing @git-upload-pack@, emulating a server that ignores it.
--
-- git://: a Python TCP proxy sits in front of a real @git daemon@ and records
-- every byte in both directions (printable bytes verbatim, others as @?@, so
-- marker substrings stay contiguous). The v2 test asserts the captured stream
-- contains @version=2@, @command=ls-refs@ and @command=fetch@. The fallback
-- test makes the proxy strip the @version=2@ request parameter from the first
-- client pkt-line (fixing up the pkt-line length) to emulate an old daemon, and
-- then asserts the conversation stayed on v0.
--
-- All servers/proxies bind ephemeral localhost ports; no internet access is
-- required and no fixed port is used.
run :: IO ()
run = do
    putStrLn "ProtocolVersionSpec: SSH v2 clone+fetch"
    sshProtocolV2CloneAndFetch
    putStrLn "ProtocolVersionSpec: SSH v2 empty repository"
    sshProtocolV2EmptyRepository
    putStrLn "ProtocolVersionSpec: SSH v0 fallback"
    sshProtocolV0Fallback
    putStrLn "ProtocolVersionSpec: git:// v2 through logging proxy"
    gitDaemonProtocolV2
    putStrLn "ProtocolVersionSpec: git:// v0 fallback through stripping proxy"
    gitDaemonProtocolV0Fallback
    putStrLn "ProtocolVersionSpec: SSH v2 SHA-256"
    sshProtocolV2Sha256

-- * SSH: protocol v2 clone, fetch, and empty repository

sshProtocolV2CloneAndFetch :: IO ()
sshProtocolV2CloneAndFetch =
    withTempDirectory "haskell-git-proto-ssh-v2" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            bareRepository = temporaryDir </> "repo.git"
            destination = temporaryDir </> "clone"
            evidenceLog = temporaryDir </> "ssh-evidence.log"
            sshScript = temporaryDir </> "fake-ssh.sh"
        _ <- createSource sourceDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        writeFile sshScript (fakeSshScript evidenceLog bareRepository True)
        _ <- runBash ("chmod +x " ++ sshScript)
        withEnvironment "GIT_SSH_COMMAND" sshScript $ do
            result <- cloneRepository "ssh://example/repo.git" destination
            assert (cloneHead result /= Nothing) "SSH v2 clone should fetch a HEAD"
            assertCloneIsValid destination "SSH v2 clone"
            evidence <- readFile evidenceLog
            assertContains
                "protocol=[version=2]"
                evidence
                "the SSH client must offer GIT_PROTOCOL=version=2 to every git-upload-pack child"
            assertContains
                "UPLOAD_PACK"
                evidence
                "the fake SSH server should have exec'd git-upload-pack"
            assertEveryUploadPackInvocationOffered "version=2" evidence
            commitAndPush sourceDirectory bareRepository "second.txt"
            fetchAndAssertTrackingRef destination bareRepository "SSH v2 clone"
            assertCloneIsValid destination "SSH v2 clone after fetch"

sshProtocolV2EmptyRepository :: IO ()
sshProtocolV2EmptyRepository =
    withTempDirectory "haskell-git-proto-ssh-v2-empty" $ \temporaryDir -> do
        let bareRepository = temporaryDir </> "empty.git"
            destination = temporaryDir </> "clone"
            evidenceLog = temporaryDir </> "ssh-evidence.log"
            sshScript = temporaryDir </> "fake-ssh.sh"
        _ <- runGit ["init", "-q", "--bare", bareRepository]
        writeFile sshScript (fakeSshScript evidenceLog bareRepository True)
        _ <- runBash ("chmod +x " ++ sshScript)
        withEnvironment "GIT_SSH_COMMAND" sshScript $ do
            result <- cloneRepository "ssh://example/repo.git" destination
            assertEqual
                Nothing
                (cloneHead result)
                "cloning an empty repository over SSH v2 should not fetch a HEAD"
            assertCloneIsValid destination "empty SSH v2 clone"
            evidence <- readFile evidenceLog
            assertContains
                "protocol=[version=2]"
                evidence
                "the SSH client must offer GIT_PROTOCOL=version=2 when discovering an empty repository"
            assertEveryUploadPackInvocationOffered "version=2" evidence

-- * SSH: protocol v0 fallback

sshProtocolV0Fallback :: IO ()
sshProtocolV0Fallback =
    withTempDirectory "haskell-git-proto-ssh-v0" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            bareRepository = temporaryDir </> "repo.git"
            destination = temporaryDir </> "clone"
            evidenceLog = temporaryDir </> "ssh-evidence.log"
            sshScript = temporaryDir </> "fake-ssh.sh"
        _ <- createSource sourceDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        writeFile sshScript (fakeSshScript evidenceLog bareRepository False)
        _ <- runBash ("chmod +x " ++ sshScript)
        withEnvironment "GIT_SSH_COMMAND" sshScript $ do
            result <- cloneRepository "ssh://example/repo.git" destination
            assert
                (cloneHead result /= Nothing)
                "SSH v0 fallback clone should fetch a HEAD when the server ignores GIT_PROTOCOL"
            assertCloneIsValid destination "SSH v0 fallback clone"
            evidence <- readFile evidenceLog
            assertContains
                "SERVER_IGNORES_GIT_PROTOCOL"
                evidence
                "the fake SSH server should have simulated an old server that ignores GIT_PROTOCOL"
            assertContains
                "protocol=[version=2]"
                evidence
                "the SSH client should attempt protocol v2 before falling back"
            -- After the server has proven to be v0-only, the follow-up v0 upload
            -- request path may legitimately stop offering version=2, so the
            -- evidence only needs to show that v2 was attempted (above) and
            -- that the server saw an unset GIT_PROTOCOL (the wrapper marker).
            commitAndPush sourceDirectory bareRepository "second.txt"
            fetchAndAssertTrackingRef destination bareRepository "SSH v0 fallback clone"
            assertCloneIsValid destination "SSH v0 fallback clone after fetch"

-- * git://: protocol v2 through a logging proxy in front of a real git daemon

gitDaemonProtocolV2 :: IO ()
gitDaemonProtocolV2 =
    withTempDirectory "haskell-git-proto-git-v2" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
            clientLog = temporaryDir </> "wire-client.log"
            serverLog = temporaryDir </> "wire-server.log"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withGitDaemon serveDirectory $ \daemonPort ->
            withLoggingProxy temporaryDir clientLog serverLog daemonPort False $ \proxyPort -> do
                let remoteUrl = "git://127.0.0.1:" ++ proxyPort ++ "/repo.git"
                result <- cloneRepository remoteUrl destination
                assert (cloneHead result /= Nothing) "git:// v2 clone should fetch a HEAD"
                assertCloneIsValid destination "git:// v2 clone"
                threadDelay 200000
                clientEvidence <- readFile clientLog
                serverEvidence <- readFile serverLog
                assertContains
                    "version=2"
                    clientEvidence
                    "the git:// client must offer version=2 in its initial request packet"
                assertContains
                    "command=ls-refs"
                    clientEvidence
                    "the git:// v2 conversation must contain a ls-refs command"
                assertContains
                    "command=fetch"
                    clientEvidence
                    "the git:// v2 conversation must contain a fetch command"
                assertContains
                    "version 2"
                    serverEvidence
                    "the git daemon must answer with a protocol v2 capability advertisement"

-- * git://: protocol v0 fallback by stripping the version=2 request parameter

gitDaemonProtocolV0Fallback :: IO ()
gitDaemonProtocolV0Fallback =
    withTempDirectory "haskell-git-proto-git-v0" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
            clientLog = temporaryDir </> "wire-client.log"
            serverLog = temporaryDir </> "wire-server.log"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withGitDaemon serveDirectory $ \daemonPort ->
            withLoggingProxy temporaryDir clientLog serverLog daemonPort True $ \proxyPort -> do
                let remoteUrl = "git://127.0.0.1:" ++ proxyPort ++ "/repo.git"
                result <- cloneRepository remoteUrl destination
                assert
                    (cloneHead result /= Nothing)
                    "git:// v0 fallback clone should fetch a HEAD when the daemon does not speak v2"
                assertCloneIsValid destination "git:// v0 fallback clone"
                threadDelay 200000
                clientEvidence <- readFile clientLog
                serverEvidence <- readFile serverLog
                assertContains
                    "version=2"
                    clientEvidence
                    "the git:// client should attempt protocol v2 before falling back"
                assertContains
                    "STRIPPED_VERSION2"
                    clientEvidence
                    "the proxy should have stripped the version=2 parameter to emulate an old daemon"
                assertContains
                    "want "
                    clientEvidence
                    "the fallback conversation should contain a v0 want line"
                assertNotContains
                    "command=ls-refs"
                    clientEvidence
                    "a v0-only daemon must never receive a v2 ls-refs command"
                assertNotContains
                    "command=fetch"
                    clientEvidence
                    "a v0-only daemon must never receive a v2 fetch command"
                assertNotContains
                    "version 2"
                    serverEvidence
                    "the emulated old daemon must not advertise protocol v2"
                assertContains
                    "multi_ack"
                    serverEvidence
                    "the emulated old daemon should have sent a v0 ref advertisement"

-- * SSH: protocol v2 with a SHA-256 repository

sshProtocolV2Sha256 :: IO ()
sshProtocolV2Sha256 =
    withTempDirectory "haskell-git-proto-ssh-sha256" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            bareRepository = temporaryDir </> "repo.git"
            destination = temporaryDir </> "clone"
            evidenceLog = temporaryDir </> "ssh-evidence.log"
            sshScript = temporaryDir </> "fake-ssh.sh"
        createDirectoryIfMissing True sourceDirectory
        _ <- runGit ["init", "-q", "-b", "main", "--object-format=sha256", sourceDirectory]
        _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
        writeFile (sourceDirectory </> "hello.txt") "hello sha256 protocol version\n"
        _ <- runGit ["-C", sourceDirectory, "add", "-A"]
        _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "first"]
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        writeFile sshScript (fakeSshScript evidenceLog bareRepository True)
        _ <- runBash ("chmod +x " ++ sshScript)
        withEnvironment "GIT_SSH_COMMAND" sshScript $ do
            result <- cloneRepository "ssh://example/repo.git" destination
            assert (cloneHead result /= Nothing) "SHA-256 SSH v2 clone should fetch a HEAD"
            assertCloneIsValid destination "SHA-256 SSH v2 clone"
            objectFormat <- trim <$> runGit ["-C", destination, "rev-parse", "--show-object-format"]
            assertEqual "sha256" objectFormat "the SHA-256 clone should keep the remote object format"
            evidence <- readFile evidenceLog
            assertContains
                "protocol=[version=2]"
                evidence
                "the SSH client must offer GIT_PROTOCOL=version=2 for SHA-256 repositories"
            assertEveryUploadPackInvocationOffered "version=2" evidence

-- * Source repositories and assertions shared by the tests above

createSource :: FilePath -> IO ()
createSource directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "hello.txt") "hello protocol version\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "first"]
    pure ()

commitAndPush :: FilePath -> FilePath -> String -> IO ()
commitAndPush sourceDirectory bareRepository fileName = do
    writeFile (sourceDirectory </> fileName) (fileName ++ "\n")
    _ <- runGit ["-C", sourceDirectory, "add", "-A"]
    _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "second"]
    _ <- runGit ["-C", sourceDirectory, "push", "-q", bareRepository, "main"]
    pure ()

fetchAndAssertTrackingRef :: FilePath -> FilePath -> String -> IO ()
fetchAndAssertTrackingRef destination bareRepository label = do
    repository <- requireRepository destination
    _ <- fetchRepository repository "origin" [] defaultFetchOptions
    localTrackingHead <- trim <$> runGit ["-C", destination, "rev-parse", "refs/remotes/origin/main"]
    remoteHead <- trim <$> runGit ["-C", bareRepository, "rev-parse", "refs/heads/main"]
    assertEqual
        remoteHead
        localTrackingHead
        (label ++ ": fetch should update refs/remotes/origin/main to the new remote head")
    _ <- runGit ["-C", destination, "fsck", "--strict"]
    pure ()

assertCloneIsValid :: FilePath -> String -> IO ()
assertCloneIsValid destination label = do
    statusText <- runGit ["-C", destination, "status", "--porcelain"]
    assertEqual "" statusText (label ++ ": working tree should be clean")
    _ <- runGit ["-C", destination, "fsck", "--strict"]
    pure ()

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found

assertEveryUploadPackInvocationOffered :: String -> String -> IO ()
assertEveryUploadPackInvocationOffered expectedProtocol evidence = do
    let expected = "protocol=[" ++ expectedProtocol ++ "]"
        invocationLines = filter (isInfixOf "git-upload-pack") (lines evidence)
    assert
        (not (null invocationLines))
        ("no git-upload-pack SSH invocation was recorded; evidence:\n" ++ evidence)
    mapM_
        ( \line ->
            assert
                (expected `isInfixOf` line)
                ( "an SSH git-upload-pack invocation did not offer "
                    ++ expected
                    ++ ": "
                    ++ line
                    ++ "\nevidence:\n"
                    ++ evidence
                )
        )
        invocationLines

assertNotContains :: String -> String -> String -> IO ()
assertNotContains needle haystack message =
    when (needle `isInfixOf` haystack) $
        die
            ( "Test failure: "
                ++ message
                ++ "\n  unexpected substring: "
                ++ show needle
                ++ "\n  actual: "
                ++ show haystack
            )

-- * Fake SSH server

-- | A fake @ssh@ that logs the @GIT_PROTOCOL@ environment variable passed by
-- the client and then runs the real server for the bare repository.
--
-- The client invokes it as @fake-ssh.sh <host> git-upload-pack '<path>'@. The
-- wrapper also tolerates capability probes (@ssh -G ...@) and receive-pack
-- invocations, which are not under test here.
fakeSshScript :: FilePath -> FilePath -> Bool -> String
fakeSshScript evidenceLog bareRepository requiresVersion2 =
    unlines
        [ "#!/bin/sh"
        , "printf 'args=[%s] protocol=[%s]\\n' \"$*\" \"${GIT_PROTOCOL-<unset>}\" >> " ++ shellQuote evidenceLog
        , "case \"$*\" in"
        , "  *git-upload-pack*)"
        , uploadPackBranch
        , "    printf 'UPLOAD_PACK\\n' >> " ++ shellQuote evidenceLog
        , "    " ++ serverEnvironment
        , "    exec git-upload-pack " ++ shellQuote bareRepository
        , "    ;;"
        , "  *git-receive-pack*)"
        , "    exec git-receive-pack " ++ shellQuote bareRepository
        , "    ;;"
        , "  *)"
        , "    exit 0"
        , "    ;;"
        , "esac"
        ]
  where
    shellQuote path = "'" ++ path ++ "'"
    serverEnvironment = if requiresVersion2 then ":" else "unset GIT_PROTOCOL"
    uploadPackBranch
        | requiresVersion2 =
            "    if [ \"${GIT_PROTOCOL-}\" != \"version=2\" ]; then"
                ++ " printf 'PROTOCOL_MISMATCH\\n' >> "
                ++ shellQuote evidenceLog
                ++ "; echo 'fake ssh: expected GIT_PROTOCOL=version=2' >&2; exit 42; fi"
        | otherwise =
            "    printf 'SERVER_IGNORES_GIT_PROTOCOL\\n' >> " ++ shellQuote evidenceLog

-- * git daemon and logging TCP proxy

withGitDaemon :: FilePath -> (String -> IO a) -> IO a
withGitDaemon serveDirectory action = do
    port <- trim <$> runBash freePortCommand
    bracket
        (startGitDaemon serveDirectory port)
        stopProcess
        (\_ -> action port)

startGitDaemon :: FilePath -> String -> IO ProcessHandle
startGitDaemon serveDirectory port = do
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

withLoggingProxy
    :: FilePath
    -> FilePath
    -> FilePath
    -> String
    -> Bool
    -> (String -> IO a)
    -> IO a
withLoggingProxy workDirectory clientLog serverLog targetPort strip action = do
    let scriptPath = workDirectory </> "tcp-proxy.py"
        portPath = workDirectory </> "proxy-port.txt"
        modeText = if strip then "strip" else "observe"
    writeFile scriptPath loggingProxyScript
    bracket
        (do
            (_, _, _, processHandle) <-
                createProcess
                    ( proc
                        "python3"
                        [scriptPath, clientLog, serverLog, portPath, targetPort, modeText]
                    )
                        { std_out = CreatePipe
                        , std_err = CreatePipe
                        }
            waitForFilePath portPath 100
            pure processHandle
        )
        stopProcess
        (\_ -> do
            portText <- trim <$> readFile portPath
            action portText
        )

freePortCommand :: String
freePortCommand =
    "python3 -c 'import socket; s=socket.socket(); s.bind((\"127.0.0.1\",0)); print(s.getsockname()[1]); s.close()'"

waitForFilePath :: FilePath -> Int -> IO ()
waitForFilePath _ 0 = die "timed out waiting for the logging TCP proxy to start"
waitForFilePath path attemptsRemaining = do
    exists <- doesFileExist path
    if exists
        then pure ()
        else do
            threadDelay 100000
            waitForFilePath path (attemptsRemaining - 1)

stopProcess :: ProcessHandle -> IO ()
stopProcess processHandle = do
    terminateProcess processHandle
    _ <- waitForProcess processHandle
    pure ()

-- | The proxy logs each direction of the byte stream into its own file. Every
-- byte that is not printable ASCII is logged as @?@, which keeps the log valid
-- UTF-8 while also keeping ASCII markers such as @version=2@,
-- @command=ls-refs@ and @command=fetch@ contiguous. In @strip@ mode the proxy
-- parses the client's first pkt-line, removes the @version=2@ extra request
-- parameter, fixes up the pkt-line length, and forwards it, emulating a daemon
-- that only speaks protocol v0.
loggingProxyScript :: String
loggingProxyScript =
    unlines
        [ "import socket"
        , "import sys"
        , "import threading"
        , "import pathlib"
        , ""
        , "c2s_path = pathlib.Path(sys.argv[1])"
        , "s2c_path = pathlib.Path(sys.argv[2])"
        , "port_path = pathlib.Path(sys.argv[3])"
        , "target_port = int(sys.argv[4])"
        , "mode = sys.argv[5]"
        , ""
        , "PRINTABLE = bytes(byte if 32 <= byte < 127 else 63 for byte in range(256))"
        , ""
        , "c2s_log = open(c2s_path, 'ab', buffering=0)"
        , "s2c_log = open(s2c_path, 'ab', buffering=0)"
        , ""
        , "server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)"
        , "server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)"
        , "server.bind(('127.0.0.1', 0))"
        , "server.listen(8)"
        , "port_path.write_text(str(server.getsockname()[1]))"
        , ""
        , "def read_exact(connection, count):"
        , "    data = b''"
        , "    while len(data) < count:"
        , "        chunk = connection.recv(count - len(data))"
        , "        if not chunk:"
        , "            return None"
        , "        data += chunk"
        , "    return data"
        , ""
        , "def pump(source, destination, log):"
        , "    try:"
        , "        while True:"
        , "            data = source.recv(65536)"
        , "            if not data:"
        , "                break"
        , "            log.write(data.translate(PRINTABLE))"
        , "            destination.sendall(data)"
        , "    except OSError:"
        , "        pass"
        , "    finally:"
        , "        try:"
        , "            destination.shutdown(socket.SHUT_WR)"
        , "        except OSError:"
        , "            pass"
        , ""
        , "def handle(client):"
        , "    upstream = socket.create_connection(('127.0.0.1', target_port))"
        , "    if mode == 'strip':"
        , "        header = read_exact(client, 4)"
        , "        if header is not None:"
        , "            try:"
        , "                length = int(header, 16)"
        , "            except ValueError:"
        , "                length = 0"
        , "            payload = b''"
        , "            if 4 <= length <= 65520:"
        , "                received = read_exact(client, length - 4)"
        , "                if received is not None:"
        , "                    payload = received"
        , "            c2s_log.write(b'\\nFIRST_PACKET|' + payload.translate(PRINTABLE) + b'\\n')"
        , "            if b'version=2' in payload:"
        , "                index = payload.find(b'version=2')"
        , "                payload = payload[:index] + payload[index + len(b'version=2'):]"
        , "                payload = payload.rstrip(b'\\x00') + b'\\x00'"
        , "                c2s_log.write(b'STRIPPED_VERSION2\\n')"
        , "                header = ('%04x' % (len(payload) + 4)).encode()"
        , "            upstream.sendall(header + payload)"
        , "    first = threading.Thread(target=pump, args=(client, upstream, c2s_log))"
        , "    second = threading.Thread(target=pump, args=(upstream, client, s2c_log))"
        , "    first.start()"
        , "    second.start()"
        , "    first.join()"
        , "    second.join()"
        , "    client.close()"
        , "    upstream.close()"
        , ""
        , "while True:"
        , "    connection, _address = server.accept()"
        , "    threading.Thread(target=handle, args=(connection,), daemon=True).start()"
        ]

-- * Small environment and string helpers

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value action =
    bracket
        (do
            previous <- lookupEnv name
            setEnv name value
            pure previous
        )
        restore
        (\_ -> action)
  where
    restore Nothing = unsetEnv name
    restore (Just previous) = setEnv name previous

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
