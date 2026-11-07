module Git.Pack.StreamingSpec
    ( run
    ) where

import Control.Exception (IOException, try)
import qualified Codec.Compression.Zlib as Zlib
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Git.Hash (HashAlgorithm (..))
import Git.Pack.Stream (decompressMemberFromHandle, validatePackFile)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hClose, openBinaryFile)
import TestSupport (assertEqual, runGit, withTempDirectory)

-- | Streaming primitives: incremental member decompression leaves the handle
-- at the next member, and whole-pack checksum validation catches corruption
-- without reading the pack into memory.
run :: IO ()
run = do
    roundTripsCompressedMember
    validatesPackedRepository
    rejectsCorruptPack

roundTripsCompressedMember :: IO ()
roundTripsCompressedMember =
    withTempDirectory "haskell-git-stream-member" $ \temporaryDir -> do
        let payload = BS8.pack (concat (replicate 200 "streamed member payload\n"))
            compressed = BL.toStrict (Zlib.compress (BL.fromStrict payload))
            path = temporaryDir </> "member.bin"
        BS.writeFile path (compressed <> BS8.pack "TAIL")
        handle <- openBinaryFile path ReadMode
        result <- decompressMemberFromHandle handle
        case result of
            Left err -> fail err
            Right (decoded, consumed) -> do
                assertEqual payload decoded "The member should inflate to the original bytes"
                assertEqual (BS.length compressed) consumed "The consumed count should cover the zlib member"
                trailing <- BS.hGet handle 4
                assertEqual "TAIL" (BS8.unpack trailing) "The handle should point at the bytes after the member"
        hClose handle

validatesPackedRepository :: IO ()
validatesPackedRepository =
    withTempDirectory "haskell-git-stream-validate" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "-b", "main", temporaryDir]
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        writeFile (temporaryDir </> "hello.txt") "hello stream\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "first"]
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        packPath <- firstPackFile (temporaryDir </> ".git" </> "objects" </> "pack")
        validatePackFile Sha1 packPath

rejectsCorruptPack :: IO ()
rejectsCorruptPack =
    withTempDirectory "haskell-git-stream-corrupt" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "-b", "main", temporaryDir]
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        writeFile (temporaryDir </> "hello.txt") "hello stream\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "first"]
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        packPath <- firstPackFile (temporaryDir </> ".git" </> "objects" </> "pack")
        packBytes <- BS.readFile packPath
        let corruptPath = temporaryDir </> "corrupt.pack"
            corruptBytes =
                BS.take (BS.length packBytes - 1) packBytes
                    <> BS.singleton (BS.last packBytes `xor` 1)
        BS.writeFile corruptPath corruptBytes
        result <- try (validatePackFile Sha1 corruptPath) :: IO (Either IOException ())
        case result of
            Left _ -> pure ()
            Right _ -> fail "A pack with a bad trailer should not validate"

firstPackFile :: FilePath -> IO FilePath
firstPackFile directory = do
    createDirectoryIfMissing True directory
    names <- listDirectory directory
    case [name | name <- names, ".pack" `isSuffixOf` name] of
        name : _ -> pure (directory </> name)
        [] -> fail "git did not create a pack file"
