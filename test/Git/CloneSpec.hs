module Git.CloneSpec
    ( run
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Clone (cloneHead, cloneLocalBranch, cloneRepository)
import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Protocol.PacketLine (encodePacketLine, flushPacket, packetLine)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.Process
    ( CreateProcess (std_err, std_out),
      StdStream (CreatePipe),
      createProcess,
      proc,
      readCreateProcessWithExitCode,
      terminateProcess,
      waitForProcess )
import TestSupport (assert, assertContains, assertEqual, withTempDirectory)

run :: IO ()
run =
    withTempDirectory "haskell-git-clone" $ \temporaryDir -> do
        let sourceRepo = temporaryDir </> "source"
            destinationRepo = temporaryDir </> "destination"
            packPath = temporaryDir </> "pack.pack"
            uploadPackPath = temporaryDir </> "upload-pack.bin"
            discoveryPath = temporaryDir </> "discovery.bin"
            serverScriptPath = temporaryDir </> "smart_http_server.py"
            portFilePath = temporaryDir </> "port.txt"
        commitId <- createSourceRepository sourceRepo packPath
        writeUploadPackFixture packPath uploadPackPath
        writeDiscoveryFixture discoveryPath commitId
        writeSmartHttpServer serverScriptPath
        withSmartHttpServer serverScriptPath discoveryPath uploadPackPath portFilePath $ \remoteUrl -> do
            cloneResult <- cloneRepository remoteUrl destinationRepo
            assertEqual commitId (cloneHead cloneResult) "Clone should return the cloned HEAD object id"
            assertEqual "refs/heads/main" (cloneLocalBranch cloneResult) "Clone should track refs/heads/main"
            clonedContents <- readFile (destinationRepo </> "hello.txt")
            assertEqual "hello clone\n" clonedContents "Clone should materialize the working tree file"
            secondaryContents <- readFile (destinationRepo </> "hello-3.txt")
            assertContains "BBBBBBBBBB" secondaryContents "Clone should materialize deltified blob contents correctly"
            remoteRef <- readFile (destinationRepo </> ".git" </> "refs" </> "remotes" </> "origin" </> "main")
            assertContains
                (objectIdToHex commitId)
                remoteRef
                "Clone should write the remote-tracking branch"
            headRef <- readFile (destinationRepo </> ".git" </> "HEAD")
            assertEqual "ref: refs/heads/main\n" headRef "Clone should rewrite HEAD to the remote default branch"
            (headExit, headStdout, headStderr) <-
                readCreateProcessWithExitCode
                    (proc "git" ["-C", destinationRepo, "rev-parse", "HEAD"])
                    ""
            assert (headExit == ExitSuccess) ("git rev-parse HEAD failed in clone destination: " ++ headStderr)
            assertEqual (objectIdToHex commitId ++ "\n") headStdout "Git should recognize the cloned HEAD commit"

createSourceRepository :: FilePath -> FilePath -> IO ObjectId
createSourceRepository sourceRepo packPath = do
    createDirectoryIfMissing True sourceRepo
    _ <- runGit ["init", "-b", "main", sourceRepo] ""
    _ <- runGit ["-C", sourceRepo, "config", "user.name", "Ada Lovelace"] ""
    _ <- runGit ["-C", sourceRepo, "config", "user.email", "ada@example.com"] ""
    writeFile (sourceRepo </> "hello.txt") "hello clone\n"
    writeFile (sourceRepo </> "hello-2.txt") ("hello clone " ++ replicate 4000 'A' ++ "\n")
    writeFile (sourceRepo </> "hello-3.txt") ("hello clone " ++ replicate 3990 'A' ++ "BBBBBBBBBB\n")
    _ <- runGit ["-C", sourceRepo, "add", "hello.txt", "hello-2.txt", "hello-3.txt"] ""
    _ <- runGit ["-C", sourceRepo, "commit", "-m", "Initial commit"] ""
    headStdout <- runGit ["-C", sourceRepo, "rev-parse", "HEAD"] ""
    let headText = trim headStdout
    _ <-
        runBash
            ( "printf '%s\\n' "
                ++ shellQuote headText
                ++ " | git -C "
                ++ shellQuote sourceRepo
                ++ " pack-objects --stdout --revs --window=10 --depth=10 > "
                ++ shellQuote packPath
            )
    _ <- runGit ["index-pack", packPath] ""
    verifyOutput <- runGit ["verify-pack", "-v", takeDirectory packPath </> "pack.idx"] ""
    assertContains "chain length =" verifyOutput "Clone fixture pack should contain a deltified object"
    case objectIdFromHex headText of
        Just commitId -> pure commitId
        Nothing -> fail "Source repository did not yield a valid HEAD object id"

writeUploadPackFixture :: FilePath -> FilePath -> IO ()
writeUploadPackFixture packPath uploadPackPath = do
    packBytes <- BS.readFile packPath
    BS.writeFile uploadPackPath (BS8.pack "0008NAK\n" <> packBytes)

writeDiscoveryFixture :: FilePath -> ObjectId -> IO ()
writeDiscoveryFixture discoveryPath commitId =
    BS.writeFile
        discoveryPath
        ( mconcat
            [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
            , encodePacketLine flushPacket
            , encodePacketLine
                (packetLine
                    (BS8.pack
                        ( objectIdToHex commitId
                            ++ " HEAD\NULmulti_ack thin-pack no-progress ofs-delta\n"
                        )))
            , encodePacketLine
                (packetLine
                    (BS8.pack (objectIdToHex commitId ++ " refs/heads/main\n")))
            ]
        )

writeSmartHttpServer :: FilePath -> IO ()
writeSmartHttpServer serverScriptPath =
    writeFile
        serverScriptPath
        (unlines
            [ "import http.server"
            , "import pathlib"
            , "import sys"
            , "discovery = pathlib.Path(sys.argv[1]).read_bytes()"
            , "upload_pack = pathlib.Path(sys.argv[2]).read_bytes()"
            , "port_file = pathlib.Path(sys.argv[3])"
            , "class Handler(http.server.BaseHTTPRequestHandler):"
            , "    def log_message(self, fmt, *args):"
            , "        return"
            , "    def do_GET(self):"
            , "        if self.path == '/repo.git/info/refs?service=git-upload-pack':"
            , "            self.send_response(200)"
            , "            self.send_header('Content-Type', 'application/x-git-upload-pack-advertisement')"
            , "            self.send_header('Content-Length', str(len(discovery)))"
            , "            self.end_headers()"
            , "            self.wfile.write(discovery)"
            , "        else:"
            , "            self.send_error(404)"
            , "    def do_POST(self):"
            , "        if self.path == '/repo.git/git-upload-pack':"
            , "            length = int(self.headers.get('Content-Length', '0'))"
            , "            self.rfile.read(length)"
            , "            self.send_response(200)"
            , "            self.send_header('Content-Type', 'application/x-git-upload-pack-result')"
            , "            self.send_header('Content-Length', str(len(upload_pack)))"
            , "            self.end_headers()"
            , "            self.wfile.write(upload_pack)"
            , "        else:"
            , "            self.send_error(404)"
            , "server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)"
            , "port_file.write_text(str(server.server_address[1]))"
            , "server.serve_forever()"
            ])

withSmartHttpServer :: FilePath -> FilePath -> FilePath -> FilePath -> (String -> IO a) -> IO a
withSmartHttpServer serverScriptPath discoveryPath uploadPackPath portFilePath action =
    bracket startServer stopServer useServer
  where
    startServer = do
        (_, _, _, processHandle) <- createProcess
            (proc "python3" [serverScriptPath, discoveryPath, uploadPackPath, portFilePath])
                {std_out = CreatePipe, std_err = CreatePipe}
        waitForPortFile portFilePath 50
        portText <- readFile portFilePath
        pure (trim portText, processHandle)

    stopServer (_, processHandle) = do
        terminateProcess processHandle
        _ <- waitForProcess processHandle
        pure ()

    useServer (portText, _) = action ("http://127.0.0.1:" ++ portText ++ "/repo.git")

waitForPortFile :: FilePath -> Int -> IO ()
waitForPortFile _ 0 = fail "Timed out waiting for Smart HTTP test server to start"
waitForPortFile portFilePath attemptsRemaining = do
    exists <- doesFileExist portFilePath
    if exists
        then pure ()
        else do
            threadDelay 100000
            waitForPortFile portFilePath (attemptsRemaining - 1)

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse

shellQuote :: FilePath -> String
shellQuote path = "'" ++ concatMap escape path ++ "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]

runGit :: [String] -> String -> IO String
runGit arguments stdinText = do
    (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (proc "git" arguments) stdinText
    assert (exitCode == ExitSuccess) ("git command failed: " ++ unwords arguments ++ "\n" ++ stderrText)
    pure stdoutText

runBash :: String -> IO String
runBash command = do
    (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (proc "bash" ["-lc", command]) ""
    assert (exitCode == ExitSuccess) ("bash command failed: " ++ command ++ "\n" ++ stderrText)
    pure stdoutText
