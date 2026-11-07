module Git.VerifySpec
    ( run
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bits (xor)
import Data.List (isSuffixOf)
import Git.Object.Blob (blobFromBytes, blobToObject)
import Git.Object.Loose (looseObjectPath, writeLooseObject)
import Git.Refs (writeRefValue)
import Git.Repo (Repository, findRepository)
import qualified Git.Repo as Repo
import Git.Verify (verifyProblems, verifyRepository)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import TestSupport (assert, assertEqual, runBash, runGit, withTempDirectory)

-- | Check that verify reports clean repositories and finds corruption in both
-- the loose store and a pack file.
run :: IO ()
run = do
    cleanLooseRepository
    cleanPackedRepository
    corruptLooseObject
    corruptPack

-- | Verify a repository with one loose object and a ref.
cleanLooseRepository :: IO ()
cleanLooseRepository =
    withTempDirectory "haskell-git-verify-loose" $ \temporaryDir -> do
        repository <- Repo.initRepository temporaryDir
        objectId <- writeLooseObject repository (blobToObject (blobFromBytes (BS8.pack "payload")))
        writeRefValue repository "refs/heads/main" objectId
        report <- verifyRepository repository
        assertEqual [] (verifyProblems report) "A clean loose repository should have no problems"

-- | Verify a packed repository created by git.
cleanPackedRepository :: IO ()
cleanPackedRepository =
    withTempDirectory "haskell-git-verify-packed" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "-b", "main", temporaryDir]
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        writeFile (temporaryDir </> "file.txt") "contents\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "first"]
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        repository <- requireRepository temporaryDir
        report <- verifyRepository repository
        assertEqual [] (verifyProblems report) "A clean packed repository should have no problems"

-- | Corrupt a loose object and expect a problem.
corruptLooseObject :: IO ()
corruptLooseObject =
    withTempDirectory "haskell-git-verify-corrupt-loose" $ \temporaryDir -> do
        repository <- Repo.initRepository temporaryDir
        objectId <- writeLooseObject repository (blobToObject (blobFromBytes (BS8.pack "payload")))
        let objectPath = looseObjectPath repository objectId
        _ <- runBash ("chmod +w " ++ objectPath)
        BS.writeFile objectPath (BS8.pack "this is not zlib")
        report <- verifyRepository repository
        assert (not (null (verifyProblems report))) "A corrupt loose object should be reported"

-- | Flip a byte in a pack and expect a problem.
corruptPack :: IO ()
corruptPack =
    withTempDirectory "haskell-git-verify-corrupt-pack" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "-b", "main", temporaryDir]
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        writeFile (temporaryDir </> "file.txt") "contents\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "first"]
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        let packDirectory = temporaryDir </> ".git" </> "objects" </> "pack"
        names <- listDirectory packDirectory
        case [name | name <- names, ".pack" `isSuffixOf` name] of
            packName : _ -> do
                let packPath = packDirectory </> packName
                _ <- runBash ("chmod +w " ++ packPath)
                packBytes <- BS.readFile packPath
                let tampered =
                        BS.take 40 packBytes
                            <> BS.singleton (BS.index packBytes 40 `xor` 0xff)
                            <> BS.drop 41 packBytes
                BS.writeFile packPath tampered
                repository <- requireRepository temporaryDir
                report <- verifyRepository repository
                assert (not (null (verifyProblems report))) "A corrupt pack should be reported"
            [] -> fail "git gc did not create a pack"

requireRepository :: FilePath -> IO Repository
requireRepository temporaryDir = do
    found <- findRepository temporaryDir
    maybe (fail "the fixture directory should be a repository") pure found
