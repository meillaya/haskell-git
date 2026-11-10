module Git.AuthSpec
    ( run
    ) where

import Control.Exception (IOException, try)
import Data.List (isInfixOf)
import Git.Clone (CloneResult (..), cloneRepository)
import Git.Fetch (defaultFetchOptions, fetchRepository)
import Git.Repo (Repository, findRepository)
import System.Directory (createDirectoryIfMissing)
import System.Environment (unsetEnv, setEnv)
import System.FilePath ((</>))
import TestSupport
    ( assert
    , assertEqual
    , runBash
    , runGit
    , withAuthenticatedHttpBackendServer
    , withTempDirectory
    )

run :: IO ()
run = do
    clonesWithUrlCredentials
    rejectsWrongPassword
    clonesWithAskpass

clonesWithUrlCredentials :: IO ()
clonesWithUrlCredentials =
    withTempDirectory "haskell-git-auth" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withAuthenticatedHttpBackendServer serveDirectory "repo.git" "ada" "secret" $ \remoteUrl -> do
            result <- cloneRepository remoteUrl destination
            assert (cloneHead result /= Nothing) "An authenticated clone should fetch the HEAD"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "An authenticated clone should be clean"
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

rejectsWrongPassword :: IO ()
rejectsWrongPassword =
    withTempDirectory "haskell-git-auth-bad" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, serveDirectory </> "repo.git"]
        withAuthenticatedHttpBackendServer serveDirectory "repo.git" "ada" "secret" $ \remoteUrl -> do
            let wrongUrl = replaceCredentials remoteUrl "ada:wrong"
            result <- try (cloneRepository wrongUrl (temporaryDir </> "clone")) :: IO (Either IOException CloneResult)
            case result of
                Left err -> do
                    let message = show err
                    assert (not ("wrong" `isInfixOf` message)) "Authentication errors must not echo the password"
                Right _ -> fail "A wrong password should not clone"

clonesWithAskpass :: IO ()
clonesWithAskpass =
    withTempDirectory "haskell-git-auth-askpass" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            askpassPath = temporaryDir </> "askpass.sh"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, serveDirectory </> "repo.git"]
        writeFile
            askpassPath
            ( unlines
                [ "#!/bin/sh"
                , "case \"$1\" in"
                , "  Username*) echo ada ;;"
                , "  *) echo secret ;;"
                , "esac"
                ]
            )
        _ <- runBash ("chmod +x " ++ askpassPath)
        withAuthenticatedHttpBackendServer serveDirectory "repo.git" "ada" "secret" $ \remoteUrl -> do
            setEnv "GIT_ASKPASS" askpassPath
            result <-
                try
                    (cloneRepository (stripCredentials remoteUrl) destination) ::
                    IO (Either IOException CloneResult)
            unsetEnv "GIT_ASKPASS"
            case result of
                Left err -> fail ("askpass clone failed: " ++ show err)
                Right cloneResult ->
                    assert (cloneHead cloneResult /= Nothing) "askpass should authenticate the clone"

createSource :: FilePath -> IO ()
createSource directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "hello.txt") "hello auth\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "first"]
    pure ()

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found

replaceCredentials :: String -> String -> String
replaceCredentials url userInfo =
    case break (== ':') url of
        (scheme, ':' : _) ->
            case dropWhile (/= '@') url of
                '@' : rest -> scheme ++ "://" ++ userInfo ++ "@" ++ rest
                _ -> url
        _ -> url

stripCredentials :: String -> String
stripCredentials url =
    case dropWhile (/= '@') url of
        '@' : rest ->
            case break (== ':') url of
                (scheme, ':' : _) -> scheme ++ "://" ++ rest
                _ -> url
        _ -> url
