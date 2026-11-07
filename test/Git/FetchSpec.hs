module Git.FetchSpec
    ( run
    ) where

import Git.Clone (cloneRepository)
import Git.Config
    ( configGet
    , configGetAll
    , parseConfig
    )
import Git.Fetch
    ( FetchOptions (..)
    , FetchResult (..)
    , defaultFetchOptions
    , fetchRepository
    )
import qualified Git.Merge
import Git.Pull (pullRepository)
import Git.RefSpec (applyRefSpec, parseRefSpec, refSpecMatches)
import Git.Repo (Repository, findRepository)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import TestSupport
    ( assert
    , assertContains
    , assertEqual
    , runGit
    , withHttpBackendServer
    , withTempDirectory
    )

run :: IO ()
run = do
    configParsing
    refSpecParsing
    fetchesNewCommit
    pullsFastForward
    fetchesAndPrunesBranches

configParsing :: IO ()
configParsing = do
    let config =
            parseConfig
                ( unlines
                    [ "[remote \"origin\"]"
                    , "\turl = https://example.com/repo.git"
                    , "\tfetch = +refs/heads/*:refs/remotes/origin/*"
                    , "[http]"
                    , "\textraHeader = Authorization: Bearer token"
                    , "[credential]"
                    , "\thelper = /tmp/helper"
                    ]
                )
    assertEqual
        (Just "https://example.com/repo.git")
        (configGet config "remote.origin.url")
        "A quoted remote subsection should be readable"
    assertEqual
        ["+refs/heads/*:refs/remotes/origin/*"]
        (configGetAll config "remote.origin.fetch")
        "Repeated keys should be readable"
    assertEqual
        (Just "Authorization: Bearer token")
        (configGet config "http.extraheader")
        "Config keys are case-insensitive"
    assertEqual
        (Just "/tmp/helper")
        (configGet config "credential.helper")
        "A simple section key should be readable"

refSpecParsing :: IO ()
refSpecParsing = do
    spec <- either fail pure (parseRefSpec "+refs/heads/*:refs/remotes/origin/*")
    assert (refSpecMatches spec "refs/heads/main") "A wildcard refspec should match a branch"
    assertEqual
        (Just "refs/remotes/origin/main")
        (applyRefSpec spec "refs/heads/main")
        "The wildcard should be substituted in the destination"
    assertEqual
        Nothing
        (applyRefSpec spec "refs/tags/v1")
        "A branch refspec should not match a tag"
    case parseRefSpec "refs/heads/main:refs/heads/local" of
        Left err -> fail err
        Right exact -> do
            assert (refSpecMatches exact "refs/heads/main") "An exact refspec should match"
            assertEqual
                (Just "refs/heads/local")
                (applyRefSpec exact "refs/heads/main")
                "The destination should be used"

fetchesNewCommit :: IO ()
fetchesNewCommit =
    withTempDirectory "haskell-git-fetch" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withHttpBackendServer serveDirectory "repo.git" $ \remoteUrl -> do
            _ <- cloneRepository remoteUrl destination
            _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
            writeFile (sourceDirectory </> "second.txt") "second commit\n"
            _ <- runGit ["-C", sourceDirectory, "add", "-A"]
            _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "second"]
            secondHead <- trim <$> runGit ["-C", sourceDirectory, "rev-parse", "HEAD"]
            _ <- runGit ["-C", sourceDirectory, "push", "-q", bareRepository, "main"]
            repository <- requireRepository destination
            result <- fetchRepository repository "origin" [] defaultFetchOptions
            assert (not (null (fetchUpdatedRefs result))) "fetch should report updated refs"
            remoteHead <- trim <$> runGit ["-C", destination, "rev-parse", "refs/remotes/origin/main"]
            assertEqual secondHead remoteHead "fetch should update the remote tracking ref"
            fetchHead <- readFile (destination </> ".git" </> "FETCH_HEAD")
            assertContains (trim secondHead) fetchHead "FETCH_HEAD should name the fetched commit"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "fetch should not touch the working tree"
            _ <- runGit ["-C", destination, "fsck", "--strict"]
            pure ()

pullsFastForward :: IO ()
pullsFastForward =
    withTempDirectory "haskell-git-pull" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withHttpBackendServer serveDirectory "repo.git" $ \remoteUrl -> do
            _ <- cloneRepository remoteUrl destination
            _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
            writeFile (sourceDirectory </> "pulled.txt") "pulled\n"
            _ <- runGit ["-C", sourceDirectory, "add", "-A"]
            _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "pulled"]
            _ <- runGit ["-C", sourceDirectory, "push", "-q", bareRepository, "main"]
            repository <- requireRepository destination
            result <- pullRepository repository "origin" defaultFetchOptions
            case result of
                Git.Merge.MergeFastForward _ -> pure ()
                Git.Merge.MergeUpToDate -> fail "pull should not be up to date"
                _ -> fail ("unexpected pull result: " ++ show result)
            contents <- readFile (destination </> "pulled.txt")
            assertEqual "pulled\n" contents "pull should check out the fetched file"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "pull should leave a clean worktree"

fetchesAndPrunesBranches :: IO ()
fetchesAndPrunesBranches =
    withTempDirectory "haskell-git-fetch-prune" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSource sourceDirectory
        _ <- runGit ["-C", sourceDirectory, "branch", "feature"]
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withHttpBackendServer serveDirectory "repo.git" $ \remoteUrl -> do
            _ <- cloneRepository remoteUrl destination
            remoteFeature <- trim <$> runGit ["-C", destination, "rev-parse", "refs/remotes/origin/feature"]
            assert (not (null remoteFeature)) "feature ref should exist"
            _ <- runGit ["-C", bareRepository, "branch", "-D", "feature"]
            repository <- requireRepository destination
            result <-
                fetchRepository
                    repository
                    "origin"
                    []
                    defaultFetchOptions {fetchPrune = True}
            assert (not (null (fetchUpdatedRefs result))) "prune fetch should still report updates"
            doesRefExist <- doesFileExist (destination </> ".git" </> "refs" </> "remotes" </> "origin" </> "feature")
            assert (not doesRefExist) "prune should remove the stale remote tracking ref"
            _ <- runGit ["-C", destination, "fsck", "--strict"]
            pure ()
            pure ()

createSource :: FilePath -> IO String
createSource directory = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "hello.txt") "hello fetch\n"
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "first"]
    _ <- runGit ["-C", directory, "tag", "-a", "v1", "-m", "release"]
    trim <$> runGit ["-C", directory, "rev-parse", "HEAD"]

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
