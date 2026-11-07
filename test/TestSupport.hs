module TestSupport
    ( assert
    , assertContains
    , assertEqual
    , runBash
    , runGit
    , withHttpBackendServer
    , withAuthenticatedHttpBackendServer
    , withTempDirectory
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory
    ( createDirectory
    , doesFileExist
    , doesPathExist
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.Exit (ExitCode (ExitSuccess), die)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process
    ( CreateProcess (std_err, std_out)
    , StdStream (CreatePipe)
    , createProcess
    , proc
    , readCreateProcessWithExitCode
    , terminateProcess
    , waitForProcess
    )

assert :: Bool -> String -> IO ()
assert condition message =
    unless condition (die ("Test failure: " ++ message))

assertContains :: String -> String -> String -> IO ()
assertContains needle haystack message =
    unless (needle `isInfixOf` haystack) $
        die
            ( "Test failure: "
                ++ message
                ++ "\n  expected substring: "
                ++ show needle
                ++ "\n  actual: "
                ++ show haystack
            )

assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual expected actual message =
    unless (expected == actual) $
        die
            ( "Test failure: "
                ++ message
                ++ "\n  expected: "
                ++ show expected
                ++ "\n  actual: "
                ++ show actual
            )

runGit :: [String] -> IO String
runGit arguments = do
    (exitCode, stdoutText, stderrText) <-
        readCreateProcessWithExitCode (proc "git" arguments) ""
    case exitCode of
        ExitSuccess -> pure stdoutText
        _ -> die ("git failed: " ++ unwords arguments ++ "\n" ++ stderrText)

runBash :: String -> IO String
runBash command = do
    (exitCode, stdoutText, stderrText) <-
        readCreateProcessWithExitCode (proc "bash" ["-lc", command]) ""
    case exitCode of
        ExitSuccess -> pure stdoutText
        _ -> die ("bash failed: " ++ command ++ "\n" ++ stderrText)

withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory prefix action = do
    temporaryRoot <- getTemporaryDirectory
    (placeholderPath, handle) <- openTempFile temporaryRoot (prefix ++ "-placeholder")
    hClose handle
    removeFile placeholderPath
    createDirectory placeholderPath
    bracket (pure placeholderPath) cleanupTempDirectory action

cleanupTempDirectory :: FilePath -> IO ()
cleanupTempDirectory directoryPath = do
    exists <- doesPathExist directoryPath
    if exists then removePathForcibly directoryPath else pure ()

-- | Serve a bare repository over Smart HTTP with git's own http-backend. The
-- action receives the clone URL. The project root is the directory that holds
-- the repository directory named by the second argument.
withHttpBackendServer :: FilePath -> String -> (String -> IO a) -> IO a
withHttpBackendServer projectRoot repositoryName action =
    withTempDirectory "haskell-git-http" $ \serverDirectory -> do
        let scriptPath = serverDirectory </> "server.py"
            portPath = serverDirectory </> "port.txt"
        writeFile scriptPath httpBackendServerScript
        bracket
            (startServer scriptPath portPath)
            stopServer
            (\(portText, _) -> action ("http://127.0.0.1:" ++ portText ++ "/" ++ repositoryName))
  where
    startServer scriptPath portPath = do
        (_, _, _, processHandle) <-
            createProcess
                (proc "python3" [scriptPath, projectRoot, portPath])
                    {std_out = CreatePipe, std_err = CreatePipe}
        waitForPortFile portPath 100
        portText <- readFile portPath
        pure (trim portText, processHandle)

    stopServer (_, processHandle) = do
        terminateProcess processHandle
        _ <- waitForProcess processHandle
        pure ()

waitForPortFile :: FilePath -> Int -> IO ()
waitForPortFile _ 0 = die "Timed out waiting for the Smart HTTP server to start"
waitForPortFile portFilePath attemptsRemaining = do
    exists <- doesFileExist portFilePath
    if exists
        then pure ()
        else do
            threadDelay 100000
            waitForPortFile portFilePath (attemptsRemaining - 1)

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse

httpBackendServerScript :: String
httpBackendServerScript =
    unlines
        [ "import http.server"
        , "import os"
        , "import pathlib"
        , "import subprocess"
        , "import sys"
        , ""
        , "def run_backend(project_root, method, path_info, query_string, body, content_type):"
        , "    env = {"
        , "        'GIT_PROJECT_ROOT': project_root,"
        , "        'GIT_HTTP_EXPORT_ALL': '1',"
        , "        'REQUEST_METHOD': method,"
        , "        'PATH_INFO': path_info,"
        , "        'QUERY_STRING': query_string,"
        , "        'CONTENT_TYPE': content_type,"
        , "        'CONTENT_LENGTH': str(len(body)),"
        , "        'REMOTE_ADDR': '127.0.0.1',"
        , "        'PATH': os.environ.get('PATH', '/usr/bin:/bin'),"
        , "    }"
        , "    result = subprocess.run(['git', 'http-backend'], input=body, env=env, capture_output=True)"
        , "    stdout = result.stdout"
        , "    separator = stdout.find(b'\\r\\n\\r\\n')"
        , "    if separator < 0:"
        , "        return 500, b'text/plain', b'bad CGI response'"
        , "    headers = stdout[:separator].decode('utf-8', 'replace').split('\\r\\n')"
        , "    payload = stdout[separator + 4:]"
        , "    status = 200"
        , "    content_type = 'application/octet-stream'"
        , "    for header in headers:"
        , "        lowered = header.lower()"
        , "        if lowered.startswith('status:'):"
        , "            status = int(header.split(':', 1)[1].strip().split(' ')[0])"
        , "        if lowered.startswith('content-type:'):"
        , "            content_type = header.split(':', 1)[1].strip()"
        , "    return status, content_type, payload"
        , ""
        , "class Handler(http.server.BaseHTTPRequestHandler):"
        , "    def log_message(self, fmt, *args):"
        , "        return"
        , "    def handle_request(self, body):"
        , "        if '?' in self.path:"
        , "            path_info, query_string = self.path.split('?', 1)"
        , "        else:"
        , "            path_info, query_string = self.path, ''"
        , "        status, content_type, payload = run_backend(project_root, self.command, path_info, query_string, body, self.headers.get('Content-Type', ''))"
        , "        self.send_response(status)"
        , "        self.send_header('Content-Type', content_type)"
        , "        self.send_header('Content-Length', str(len(payload)))"
        , "        self.end_headers()"
        , "        self.wfile.write(payload)"
        , "    def do_GET(self):"
        , "        self.handle_request(b'')"
        , "    def do_POST(self):"
        , "        length = int(self.headers.get('Content-Length', '0'))"
        , "        self.handle_request(self.rfile.read(length))"
        , ""
        , "project_root = sys.argv[1]"
        , "port_file = pathlib.Path(sys.argv[2])"
        , "server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)"
        , "port_file.write_text(str(server.server_address[1]))"
        , "server.serve_forever()"
        ]

-- | Serve a bare repository over Smart HTTP with HTTP Basic authentication.
withAuthenticatedHttpBackendServer
    :: FilePath
    -> String
    -> String
    -> String
    -> (String -> IO a)
    -> IO a
withAuthenticatedHttpBackendServer projectRoot repositoryName username password action =
    withTempDirectory "haskell-git-http-auth" $ \serverDirectory -> do
        let scriptPath = serverDirectory </> "auth_server.py"
            portPath = serverDirectory </> "port.txt"
        writeFile scriptPath (authenticatedHttpBackendServerScript username password)
        bracket
            (startServer scriptPath portPath)
            stopServer
            (\(portText, _) ->
                action
                    ( "http://"
                        ++ username
                        ++ ":"
                        ++ password
                        ++ "@127.0.0.1:"
                        ++ portText
                        ++ "/"
                        ++ repositoryName
                    )
            )
  where
    startServer scriptPath portPath = do
        (_, _, _, processHandle) <-
            createProcess
                (proc "python3" [scriptPath, projectRoot, portPath])
                    {std_out = CreatePipe, std_err = CreatePipe}
        waitForPortFile portPath 100
        portText <- readFile portPath
        pure (trim portText, processHandle)

    stopServer (_, processHandle) = do
        terminateProcess processHandle
        _ <- waitForProcess processHandle
        pure ()

authenticatedHttpBackendServerScript :: String -> String -> String
authenticatedHttpBackendServerScript username password =
    unlines
        [ "import base64"
        , "import http.server"
        , "import os"
        , "import pathlib"
        , "import subprocess"
        , "import sys"
        , ""
        , "EXPECTED = 'Basic ' + base64.b64encode(b'"
            ++ username
            ++ ":"
            ++ password
            ++ "').decode()"
        , ""
        , "def run_backend(project_root, method, path_info, query_string, body, content_type):"
        , "    env = {"
        , "        'GIT_PROJECT_ROOT': project_root,"
        , "        'GIT_HTTP_EXPORT_ALL': '1',"
        , "        'REQUEST_METHOD': method,"
        , "        'PATH_INFO': path_info,"
        , "        'QUERY_STRING': query_string,"
        , "        'CONTENT_TYPE': content_type,"
        , "        'CONTENT_LENGTH': str(len(body)),"
        , "        'REMOTE_ADDR': '127.0.0.1',"
        , "        'PATH': os.environ.get('PATH', '/usr/bin:/bin'),"
        , "    }"
        , "    result = subprocess.run(['git', 'http-backend'], input=body, env=env, capture_output=True)"
        , "    stdout = result.stdout"
        , "    separator = stdout.find(b'\\r\\n\\r\\n')"
        , "    if separator < 0:"
        , "        return 500, b'text/plain', b'bad CGI response'"
        , "    headers = stdout[:separator].decode('utf-8', 'replace').split('\\r\\n')"
        , "    payload = stdout[separator + 4:]"
        , "    status = 200"
        , "    content_type = 'application/octet-stream'"
        , "    for header in headers:"
        , "        lowered = header.lower()"
        , "        if lowered.startswith('status:'):"
        , "            status = int(header.split(':', 1)[1].strip().split(' ')[0])"
        , "        if lowered.startswith('content-type:'):"
        , "            content_type = header.split(':', 1)[1].strip()"
        , "    return status, content_type, payload"
        , ""
        , "class Handler(http.server.BaseHTTPRequestHandler):"
        , "    def log_message(self, fmt, *args):"
        , "        return"
        , "    def handle_request(self, body):"
        , "        if self.headers.get('Authorization', '') != EXPECTED:"
        , "            self.send_response(401)"
        , "            self.send_header('WWW-Authenticate', 'Basic realm=\"repo\"')"
        , "            self.send_header('Content-Length', '0')"
        , "            self.end_headers()"
        , "            return"
        , "        if '?' in self.path:"
        , "            path_info, query_string = self.path.split('?', 1)"
        , "        else:"
        , "            path_info, query_string = self.path, ''"
        , "        status, content_type, payload = run_backend(project_root, self.command, path_info, query_string, body, self.headers.get('Content-Type', ''))"
        , "        self.send_response(status)"
        , "        self.send_header('Content-Type', content_type)"
        , "        self.send_header('Content-Length', str(len(payload)))"
        , "        self.end_headers()"
        , "        self.wfile.write(payload)"
        , "    def do_GET(self):"
        , "        self.handle_request(b'')"
        , "    def do_POST(self):"
        , "        length = int(self.headers.get('Content-Length', '0'))"
        , "        self.handle_request(self.rfile.read(length))"
        , ""
        , "project_root = sys.argv[1]"
        , "port_file = pathlib.Path(sys.argv[2])"
        , "server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)"
        , "port_file.write_text(str(server.server_address[1]))"
        , "server.serve_forever()"
        ]
