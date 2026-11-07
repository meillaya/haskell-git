module Git.PushSpec
    ( run
    ) where

import Control.Exception (IOException, try)
import Git.Clone (cloneRepository)
import Git.Push (PushOptions (..), PushResult (..), defaultPushOptions, pushRepository)
import Git.Repo (Repository, findRepository)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import TestSupport
    ( assertEqual
    , runGit
    , withHttpBackendServer
    , withTempDirectory
    )

run :: IO ()
run = do
    pushesNewCommit
    rejectsNonFastForward

pushesNewCommit :: IO ()
pushesNewCommit =
    withTempDirectory "haskell-git-push" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        _ <- runGit ["-C", bareRepository, "config", "http.receivepack", "true"]
        withHttpBackendServer serveDirectory "repo.git" $ \remoteUrl -> do
            _ <- cloneRepository remoteUrl destination
            _ <- runGit ["-C", destination, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", destination, "config", "user.name", "Ada"]
            writeFile (destination </> "pushed.txt") "pushed\n"
            _ <- runGit ["-C", destination, "add", "-A"]
            _ <- runGit ["-C", destination, "commit", "-qm", "pushed"]
            pushedHead <- trim <$> runGit ["-C", destination, "rev-parse", "HEAD"]
            repository <- requireRepository destination
            _ <- pushRepository repository "origin" ["refs/heads/main:refs/heads/main"] defaultPushOptions
            remoteHead <- trim <$> runGit ["-C", bareRepository, "rev-parse", "refs/heads/main"]
            assertEqual pushedHead remoteHead "Push should update the remote branch"

rejectsNonFastForward :: IO ()
rejectsNonFastForward =
    withTempDirectory "haskell-git-push-reject" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        _ <- runGit ["-C", bareRepository, "config", "http.receivepack", "true"]
        withHttpBackendServer serveDirectory "repo.git" $ \remoteUrl -> do
            _ <- cloneRepository remoteUrl destination
            _ <- runGit ["-C", destination, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", destination, "config", "user.name", "Ada"]
            writeFile (destination </> "first.txt") "first\n"
            _ <- runGit ["-C", destination, "add", "-A"]
            _ <- runGit ["-C", destination, "commit", "-qm", "first"]
            firstHead <- trim <$> runGit ["-C", destination, "rev-parse", "HEAD"]
            _ <- runGit ["-C", destination, "push", "-q", bareRepository, "main"]
            _ <- runGit ["-C", destination, "reset", "-q", "--hard", "HEAD~1"]
            writeFile (destination </> "second.txt") "second\n"
            _ <- runGit ["-C", destination, "add", "-A"]
            _ <- runGit ["-C", destination, "commit", "-qm", "second"]
            repository <- requireRepository destination
            result <-
                try
                    (pushRepository repository "origin" ["refs/heads/main:refs/heads/main"] defaultPushOptions) ::
                    IO (Either IOException PushResult)
            case result of
                Left _ -> pure ()
                Right _ -> fail "A non-fast-forward push should fail"
            remoteHead <- trim <$> runGit ["-C", bareRepository, "rev-parse", "refs/heads/main"]
            assertEqual firstHead remoteHead "A rejected push must not move the remote branch"
            _ <- runGit ["-C", bareRepository, "fsck", "--strict"]
            pure ()

createSource :: FilePath -> IO ()
createSource directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "hello.txt") "hello push\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "first"]
    pure ()

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
