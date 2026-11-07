module Git.IndexSpec
    ( run
    ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Index
    ( IndexEntry (..)
    , buildTreeFromIndex
    , parseIndex
    , upsertIndexEntry
    , writeIndex
    )
import Git.Object.Blob (blobFromBytes, blobToObject)
import Git.Object.Loose (writeLooseObject)
import Git.Object.Tree (TreeEntryMode (..))
import Git.Repo (Repository, repositoryIndexPath, repositoryRootDir)
import qualified Git.Repo as Repo
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import TestSupport (assertEqual, runBash, runGit, withTempDirectory)

-- | Check the index codec and that git reads the written file: a clean status,
-- the same ls-files output, and the same tree the index describes.
run :: IO ()
run = do
    roundTrip
    gitReadsTheIndex
    writesTrees

-- | Write an index and parse it back.
roundTrip :: IO ()
roundTrip = do
    entries <- sampleEntries
    decoded <- either fail pure (parseIndex (writeIndex entries))
    assertEqual
        (map indexEntryPath entries)
        (map indexEntryPath decoded)
        "Index codec should round-trip paths"
    assertEqual
        (map indexEntryMode entries)
        (map indexEntryMode decoded)
        "Index codec should round-trip modes"
    assertEqual
        (map indexEntryObjectId entries)
        (map indexEntryObjectId decoded)
        "Index codec should round-trip object ids"

-- | Write files and an index, commit with git, and check status and ls-files.
gitReadsTheIndex :: IO ()
gitReadsTheIndex =
    withTempDirectory "haskell-git-index" $ \temporaryDir -> do
        repository <- Repo.initRepository temporaryDir
        entries <-
            writeFilesAndStage
                repository
                [ ("a.txt", "hello\n", RegularFile)
                , ("run.sh", "#!/bin/sh\necho hi\n", ExecutableFile)
                ]
        BS.writeFile (repositoryIndexPath repository) (writeIndex entries)
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "first"]
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "git status should be clean after the index is written"
        listing <- runGit ["-C", temporaryDir, "ls-files", "-s"]
        let expected =
                [ "100644 " ++ objectIdToHex (indexEntryObjectId (entries !! 0)) ++ " 0\ta.txt"
                , "100755 " ++ objectIdToHex (indexEntryObjectId (entries !! 1)) ++ " 0\trun.sh"
                ]
        assertEqual expected (filter (not . null) (lines listing)) "git ls-files should show the staged entries"
        fsckText <- runGit ["-C", temporaryDir, "fsck", "--strict"]
        assertEqual "" fsckText "git fsck should accept the repository"

-- | Build trees from the index and compare with git ls-tree.
writesTrees :: IO ()
writesTrees =
    withTempDirectory "haskell-git-write-tree" $ \temporaryDir -> do
        repository <- Repo.initRepository temporaryDir
        entries <-
            writeFilesAndStage
                repository
                [ ("a.txt", "hello\n", RegularFile)
                , ("docs/b.txt", "doc\n", RegularFile)
                , ("run.sh", "#!/bin/sh\n", ExecutableFile)
                ]
        treeId <- buildTreeFromIndex repository entries
        listing <- runGit ["-C", temporaryDir, "ls-tree", "-r", objectIdToHex treeId]
        let expected =
                [ "100644 blob " ++ objectIdToHex (indexEntryObjectId (entries !! 0)) ++ "\ta.txt"
                , "100644 blob " ++ objectIdToHex (indexEntryObjectId (entries !! 1)) ++ "\tdocs/b.txt"
                , "100755 blob " ++ objectIdToHex (indexEntryObjectId (entries !! 2)) ++ "\trun.sh"
                ]
        assertEqual expected (filter (not . null) (lines listing)) "git ls-tree should match the tree written from the index"

sampleEntries :: IO [IndexEntry]
sampleEntries = do
    first <- requireObjectId "ce013625030ba8dba906f756967f9e9ca394464a"
    second <- requireObjectId "4163036efa65bd4a469e752267498f01ea36a55c"
    third <- requireObjectId "42061c01a1c70097d1e4579f29a5adf40abdec95"
    fourth <- requireObjectId "8e695ec83aa8b1d596183b26206a514576570fff"
    pure
        ( sortOn
            indexEntryPath
            [ IndexEntry (BS8.pack "a.txt") RegularFile 6 first 0
            , IndexEntry (BS8.pack "run.sh") ExecutableFile 18 second 0
            , IndexEntry (BS8.pack "link") Symlink 9 third 0
            , IndexEntry (BS8.pack "docs/b.txt") RegularFile 4 fourth 0
            ]
        )

writeFilesAndStage :: Repository -> [(FilePath, String, TreeEntryMode)] -> IO [IndexEntry]
writeFilesAndStage repository files = foldFiles [] files
  where
    foldFiles accumulated [] = pure accumulated
    foldFiles accumulated ((relativePath, contents, mode) : rest) = do
        let destination = repositoryRootDir repository </> relativePath
            payload = BS8.pack contents
        createDirectoryIfMissing True (takeDirectory destination)
        BS.writeFile destination payload
        case mode of
            ExecutableFile -> do
                _ <- runBash ("chmod +x " ++ show destination)
                pure ()
            _ -> pure ()
        objectId <- writeLooseObject repository (blobToObject (blobFromBytes payload))
        let entry =
                IndexEntry
                    { indexEntryPath = BS8.pack relativePath
                    , indexEntryMode = mode
                    , indexEntrySize = fromIntegral (BS.length payload)
                    , indexEntryObjectId = objectId
                    , indexEntryStage = 0
                    }
        foldFiles (upsertIndexEntry entry accumulated) rest

requireObjectId :: String -> IO ObjectId
requireObjectId hexText =
    maybe (fail ("invalid fixture object id: " ++ hexText)) pure (objectIdFromHex hexText)
