module Git.CloneSpec
    ( run
    ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort)
import Git.Clone (CloneResult (..), cloneRepository)
import Git.Hash (ObjectId, objectIdFromHex)
import Git.Object.Commit (commitFromObject, commitTree)
import Git.Object.Tree (TreeEntry (..), renderTreeEntry)
import Git.Repo (findRepository)
import Git.Store (flattenTree, loadStore, readObjectOrFail)
import System.Directory
    ( Permissions (..)
    , createDirectoryIfMissing
    , doesFileExist
    , getPermissions
    , getSymbolicLinkTarget
    )
import System.FilePath ((</>))
import TestSupport
    ( assert
    , assertContains
    , assertEqual
    , runBash
    , runGit
    , withHttpBackendServer
    , withTempDirectory
    )

-- | Check an end-to-end clone over real git http-backend: checkout, index,
-- branches, tags, symlinks, executable bits, empty repositories, and the
-- refusal to write into a non-empty destination.
run :: IO ()
run = do
    clonesRepositoryWithModesAndRefs
    clonesEmptyRepository
    refusesNonEmptyDestination

-- | Serve a repository and clone it; check status, refs, modes, and tree.
clonesRepositoryWithModesAndRefs :: IO ()
clonesRepositoryWithModesAndRefs =
    withTempDirectory "haskell-git-clone" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            destination = temporaryDir </> "clone"
        headObjectId <- createSourceRepository sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, serveDirectory </> "repo.git"]
        withHttpBackendServer serveDirectory "repo.git" $ \remoteUrl -> do
            result <- cloneRepository remoteUrl destination
            assertEqual (Just headObjectId) (cloneHead result) "Clone should report the remote HEAD"
            assertEqual "refs/heads/main" (cloneLocalBranch result) "Clone should report the local branch"
            indexExists <- doesFileExist (destination </> ".git" </> "index")
            assert indexExists "Clone should write an index"
            packs <- runBash ("find " ++ show (destination </> ".git" </> "objects" </> "pack") ++ " -name '*.idx' -print 2>/dev/null | wc -l")
            loose <- runBash ("find " ++ show (destination </> ".git" </> "objects") ++ " -type f ! -path '*/pack/*' | wc -l")
            assert (read (trim packs) > (0 :: Int)) "Clone should retain a pack index"
            assertEqual "0" (trim loose) "Clone should remove loose copies after retaining the pack"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "git status should be clean after clone"
            fsckText <- runGit ["-C", destination, "fsck", "--strict"]
            assertEqual "" fsckText "git fsck should accept the clone"
            branchList <- runGit ["-C", destination, "branch", "-a"]
            forM_ ["origin/main", "origin/feature", "origin/HEAD"] $ \branchName ->
                assertContains branchName branchList ("Clone should create " ++ branchName)
            tagList <- runGit ["-C", destination, "tag"]
            assert ("v1" `elem` lines tagList) "Clone should fetch annotated tags"
            symbolicTarget <- getSymbolicLinkTarget (destination </> "link.md")
            assertEqual "README.md" symbolicTarget "Clone should create symlinks with their target"
            permissions <- getPermissions (destination </> "run.sh")
            assert (executable permissions) "Clone should restore the executable bit"
            contents <- readFile (destination </> "README.md")
            assertEqual "hello clone\n" contents "Clone should write the file contents"
            gitListing <- gitTreeListing destination
            ourListing <- ourTreeListing destination
            assertEqual
                (sort (filter (not . null) (lines gitListing)))
                (sort ourListing)
                "Clone should produce the same tree as git ls-tree"

-- | Serve an empty repository and clone it.
clonesEmptyRepository :: IO ()
clonesEmptyRepository =
    withTempDirectory "haskell-git-empty-clone" $ \temporaryDir -> do
        let serveDirectory = temporaryDir </> "serve"
            destination = temporaryDir </> "clone"
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["init", "-q", "--bare", serveDirectory </> "empty.git"]
        withHttpBackendServer serveDirectory "empty.git" $ \remoteUrl -> do
            result <- cloneRepository remoteUrl destination
            assertEqual Nothing (cloneHead result) "An empty remote should clone without a head commit"
            headText <- runGit ["-C", destination, "symbolic-ref", "HEAD"]
            assertEqual "refs/heads/main\n" headText "An empty clone should still have a default branch"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "An empty clone should be clean"

-- | Point clone at a non-empty directory.
refusesNonEmptyDestination :: IO ()
refusesNonEmptyDestination =
    withTempDirectory "haskell-git-clone-refuse" $ \temporaryDir -> do
        let destination = temporaryDir </> "existing"
        createDirectoryIfMissing True destination
        writeFile (destination </> "keep.txt") "do not overwrite\n"
        result <-
            try (cloneRepository "http://127.0.0.1:1/repo.git" destination) ::
                IO (Either IOException CloneResult)
        case result of
            Left _ -> pure ()
            Right _ -> fail "Clone should refuse a non-empty destination"

createSourceRepository :: FilePath -> IO ObjectId
createSourceRepository sourceDirectory = do
    createDirectoryIfMissing True sourceDirectory
    _ <- runGit ["init", "-q", "-b", "main", sourceDirectory]
    _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
    writeFile (sourceDirectory </> "README.md") "hello clone\n"
    createDirectoryIfMissing True (sourceDirectory </> "docs")
    writeFile (sourceDirectory </> "docs" </> "nested.txt") "nested\n"
    writeFile (sourceDirectory </> "run.sh") "#!/bin/sh\necho hi\n"
    _ <- runBash ("chmod +x " ++ sourceDirectory </> "run.sh")
    _ <- runBash ("ln -s README.md " ++ sourceDirectory </> "link.md")
    _ <- runGit ["-C", sourceDirectory, "add", "-A"]
    _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "initial"]
    _ <- runGit ["-C", sourceDirectory, "branch", "feature"]
    _ <- runGit ["-C", sourceDirectory, "tag", "-a", "v1", "-m", "release"]
    headText <- trim <$> runGit ["-C", sourceDirectory, "rev-parse", "HEAD"]
    maybe (fail "git returned an invalid head id") pure (objectIdFromHex headText)

gitTreeListing :: FilePath -> IO String
gitTreeListing destination = runGit ["-C", destination, "ls-tree", "-r", "HEAD"]

ourTreeListing :: FilePath -> IO [String]
ourTreeListing destination = do
    found <- findRepository destination
    repository <- maybe (fail "the clone destination should be a repository") pure found
    store <- loadStore repository
    headText <- trim <$> runGit ["-C", destination, "rev-parse", "HEAD"]
    headObjectId <-
        maybe (fail "git returned an invalid head id") pure (objectIdFromHex headText)
    headObject <- readObjectOrFail store headObjectId
    currentCommit <- either fail pure (commitFromObject headObject)
    flattened <- flattenTree store (commitTree currentCommit)
    pure
        [ BS8.unpack (renderTreeEntry entry {treeEntryName = BS8.pack path})
        | (path, entry) <- flattened
        ]

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
