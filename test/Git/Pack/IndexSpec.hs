module Git.Pack.IndexSpec
    ( run
    ) where

import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Git.Pack (packHeaderObjectCount, parsePackHeader)
import Git.Pack.Index
    ( PackIndexEntry (..)
    , lookupPackIndexOffset
    , packIndexEntries
    , packIndexObjectIds
    , parsePackIndex
    )
import TestSupport (assert, assertEqual, runGit, withTempDirectory)

-- | Check that a pack index written by git parses, that its offsets point at
-- the pack, and that a tampered index is refused.
run :: IO ()
run = do
    indexReading
    checksumRejection

-- | Parse the index git wrote, compare counts, and look up an id.
indexReading :: IO ()
indexReading =
    withPackedRepository $ \temporaryDir -> do
        (indexPath, packPath) <- findPackFiles temporaryDir
        indexBytes <- BS.readFile indexPath
        packIndex <- either fail pure (parsePackIndex indexBytes)
        packBytes <- BS.readFile packPath
        let entries = packIndexEntries packIndex
        expectedCount <- length . lines <$> runGit ["-C", temporaryDir, "cat-file", "--batch-all-objects", "--batch-check"]
        assertEqual expectedCount (length entries) "Pack index should list every object"
        assert (not (null entries)) "The fixture pack should not be empty"
        let packLength = BS.length packBytes
            offsetsAreInside = all (\entry -> offsetIsInside packLength (packIndexEntryOffset entry)) entries
        assert offsetsAreInside "Every pack index offset should point inside the pack file"
        (header, _) <- either fail pure (parsePackHeader packBytes)
        assertEqual (packHeaderObjectCount header) (length entries) "Pack header count should match the index"
        case packIndexObjectIds packIndex of
            firstObjectId : _ ->
                case lookupPackIndexOffset packIndex firstObjectId of
                    Just _ -> pure ()
                    Nothing -> fail "Looking up a known object id in the index should find it"
            [] -> fail "The fixture pack index should not be empty"

offsetIsInside :: Int -> Int -> Bool
offsetIsInside packLength offset = offset >= 12 && offset < packLength - 20

-- | Flip a byte in the index and expect a refusal.
checksumRejection :: IO ()
checksumRejection =
    withPackedRepository $ \temporaryDir -> do
        (indexPath, _) <- findPackFiles temporaryDir
        indexBytes <- BS.readFile indexPath
        let tampered =
                BS.take 30 indexBytes
                    <> BS.singleton (BS.index indexBytes 30 `xor` 0xff)
                    <> BS.drop 31 indexBytes
        case parsePackIndex tampered of
            Left _ -> pure ()
            Right _ -> fail "A tampered pack index should be rejected"

withPackedRepository :: (FilePath -> IO a) -> IO a
withPackedRepository action =
    withTempDirectory "haskell-git-pack-index" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "-b", "main", temporaryDir]
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        writeFile (temporaryDir </> "one.txt") "one\n"
        writeFile (temporaryDir </> "two.txt") "two\n"
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "first"]
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        action temporaryDir

findPackFiles :: FilePath -> IO (FilePath, FilePath)
findPackFiles temporaryDir = do
    let packDirectory = temporaryDir </> ".git" </> "objects" </> "pack"
    names <- listDirectory packDirectory
    case [name | name <- names, ".idx" `isSuffixOf` name] of
        indexName : _ -> do
            let indexPath = packDirectory </> indexName
                packPath = take (length indexPath - length ".idx") indexPath ++ ".pack"
            pure (indexPath, packPath)
        [] -> fail "git gc did not create a pack index"
