module Git.SubmoduleSpec
    ( run
    ) where

import Data.List (isInfixOf, isPrefixOf)
import Git.Clone (cloneRepository)
import Git.Repo (Repository, findRepository)
import Git.Store (loadStore)
import Git.Submodule
    ( Submodule (..)
    , addSubmodule
    , deinitSubmodule
    , initialiseSubmodules
    , readSubmodules
    , resolveSubmoduleUrl
    , submoduleStatus
    , submoduleSync
    , updateSubmodules
    , updateSubmodulesRemote
    )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import TestSupport
    ( assert
    , assertEqual
    , runGit
    , withHttpBackendServer
    , withTempDirectory
    )

run :: IO ()
run = do
    parsesGitmodules
    resolvesRelativeUrls
    clonesRecursively
    addsAndDeinitialises
    reportsStatus
    updatesExistingSubmodule

parsesGitmodules :: IO ()
parsesGitmodules =
    withTempDirectory "haskell-git-submodule-parse" $ \temporaryDir -> do
        createDirectoryIfMissing True temporaryDir
        writeFile
            (temporaryDir </> ".gitmodules")
            ( unlines
                [ "[submodule \"docs\"]"
                , "\tpath = docs"
                , "\turl = https://example.com/docs.git"
                ]
            )
        _ <- runGit ["init", "-q", temporaryDir]
        repository <- requireRepository temporaryDir
        submodules <- readSubmodules repository
        assertEqual 1 (length submodules) "one submodule should parse"
        assertEqual "docs" (submodulePath (head submodules)) "the path should parse"
        assertEqual
            "https://example.com/docs.git"
            (submoduleUrl (head submodules))
            "the url should parse"

resolvesRelativeUrls :: IO ()
resolvesRelativeUrls = do
    assertEqual
        (Right "https://example.com/org/sub.git")
        (resolveSubmoduleUrl "https://example.com/org/super.git" "../sub.git")
        "One parent level should stay in the superproject directory"
    assertEqual
        (Right "https://example.com/org/sub.git")
        (resolveSubmoduleUrl "https://example.com/org/super.git" "./sub.git")
        "A dot-relative URL should stay in the superproject directory"

clonesRecursively :: IO ()
clonesRecursively =
    withTempDirectory "haskell-git-submodule-clone" $ \temporaryDir -> do
        let subSource = temporaryDir </> "sub-source"
            subServe = temporaryDir </> "sub-serve"
            superSource = temporaryDir </> "super-source"
            superServe = temporaryDir </> "super-serve"
            destination = temporaryDir </> "clone"
        _ <- createRepository subSource "sub.txt" "sub\n"
        createDirectoryIfMissing True subServe
        _ <- runGit ["clone", "-q", "--bare", subSource, subServe </> "sub.git"]
        withHttpBackendServer subServe "sub.git" $ \subUrl -> do
            createDirectoryIfMissing True superSource
            _ <- runGit ["init", "-q", "-b", "main", superSource]
            _ <- runGit ["-C", superSource, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", superSource, "config", "user.name", "Ada"]
            _ <- runGit ["-C", superSource, "-c", "protocol.file.allow=always", "submodule", "add", "-q", subUrl, "sub"]
            _ <- runGit ["-C", superSource, "add", "-A"]
            _ <- runGit ["-C", superSource, "commit", "-qm", "with submodule"]
            createDirectoryIfMissing True superServe
            _ <- runGit ["clone", "-q", "--bare", superSource, superServe </> "super.git"]
            withHttpBackendServer superServe "super.git" $ \superUrl -> do
                _ <- cloneRepository superUrl destination
                repository <- requireRepository destination
                store <- loadStore repository
                initialiseSubmodules repository store True
                subFile <- doesFileExist (destination </> "sub" </> "sub.txt")
                assert subFile "recursive clone should check out the submodule worktree"
                modulesDir <- doesDirectoryExist (destination </> ".git" </> "modules" </> "sub")
                assert modulesDir "the submodule git dir should live under .git/modules"
                gitFile <- readFile (destination </> "sub" </> ".git")
                assert ("gitdir:" `isPrefixOf` gitFile) "the submodule should use a gitfile"
                _ <- runGit ["-C", destination, "fsck", "--strict"]
                statusText <- runGit ["-C", destination, "submodule", "status"]
                assert (not (null statusText)) "git submodule status should see the submodule"
                submoduleSync repository
                updateSubmodulesRemote repository store False
                remoteStatus <- runGit ["-C", destination, "status", "--porcelain"]
                assertEqual "" remoteStatus "submodule update --remote should leave the worktree clean"
                configText <- readFile (destination </> ".git" </> "config")
                assert ("[submodule \"sub\"]" `isInfixOf` configText) "submodule sync should write a submodule section"
                assert ("url = " `isInfixOf` configText) "submodule sync should write the URL"

addsAndDeinitialises :: IO ()
addsAndDeinitialises =
    withTempDirectory "haskell-git-submodule-add" $ \temporaryDir -> do
        let subSource = temporaryDir </> "sub-source"
            subServe = temporaryDir </> "sub-serve"
            superRepo = temporaryDir </> "super"
        _ <- createRepository subSource "sub.txt" "sub\n"
        createDirectoryIfMissing True subServe
        _ <- runGit ["clone", "-q", "--bare", subSource, subServe </> "sub.git"]
        withHttpBackendServer subServe "sub.git" $ \subUrl -> do
            _ <- runGit ["init", "-q", "-b", "main", superRepo]
            _ <- runGit ["-C", superRepo, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", superRepo, "config", "user.name", "Ada"]
            repository <- requireRepository superRepo
            (path, _commitId) <- addSubmodule repository subUrl (Just "sub") (Just "main")
            assertEqual "sub" path "addSubmodule should use the requested path"
            modulesText <- readFile (superRepo </> ".gitmodules")
            assert ("[submodule \"sub\"]" `isInfixOf` modulesText) ".gitmodules should get a section"
            assert ("branch = main" `isInfixOf` modulesText) ".gitmodules should record the branch"
            staged <- runGit ["-C", superRepo, "ls-files", "--stage"]
            assert ("160000" `isInfixOf` staged) ("The superproject index should hold a gitlink; got " ++ show staged)
            modulesDir <- doesDirectoryExist (superRepo </> ".git" </> "modules" </> "sub")
            assert modulesDir "addSubmodule should move the git dir under .git/modules"
            submodules <- readSubmodules repository
            assertEqual (Just "main") (submoduleBranch (head submodules)) "The branch should parse back"
            deinitSubmodule repository "sub"
            worktreeGone <- doesFileExist (superRepo </> "sub" </> "sub.txt")
            assert (not worktreeGone) "deinitSubmodule should remove the worktree"
            modulesGone <- doesDirectoryExist (superRepo </> ".git" </> "modules" </> "sub")
            assert (not modulesGone) "deinitSubmodule should remove the module git dir"

reportsStatus :: IO ()
reportsStatus =
    withTempDirectory "haskell-git-submodule-status" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", temporaryDir]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        status <- submoduleStatus repository store
        assertEqual [] status "a repository without .gitmodules has no submodule status"

updatesExistingSubmodule :: IO ()
updatesExistingSubmodule =
    withTempDirectory "haskell-git-submodule-update" $ \temporaryDir -> do
        writeFile (temporaryDir </> "keep.txt") "keep\n"
        _ <- runGit ["init", "-q", temporaryDir]
        repository <- requireRepository temporaryDir
        store <- loadStore repository
        updateSubmodules repository store False
        pure ()

createRepository :: FilePath -> FilePath -> String -> IO ()
createRepository directory relativePath contents = do
    createDirectoryIfMissing True directory
    _ <- runGit ["init", "-q", "-b", "main", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> relativePath) contents
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "initial"]
    pure ()

requireRepository :: FilePath -> IO Repository
requireRepository path = do
    found <- findRepository path
    maybe (fail ("not a repository: " ++ path)) pure found
