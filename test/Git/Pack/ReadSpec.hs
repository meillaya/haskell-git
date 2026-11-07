module Git.Pack.ReadSpec
    ( run
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isSuffixOf)
import Git.Hash (objectIdFromHex)
import Git.Object (GitObjectType (BlobObject), gitObject)
import Git.Object.Loose (objectIdForObject)
import Git.Pack (PackObjectKind (PackBlob), packHeaderObjectCount, parsePackHeader)
import Git.Pack.Index (PackIndex, lookupPackIndexOffset, parsePackIndex)
import Git.Pack.Read (PackRawObject (..), parsePackObjectAt)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import TestSupport (assertEqual, runGit, withTempDirectory)

-- | Check that pack objects parse at their index offsets, that a large blob
-- decompresses to the right bytes, that walking the pack consumes it exactly,
-- and that a truncated pack is refused.
run :: IO ()
run = do
    fullObjectPayload
    sequentialWalkReachesTheEnd
    truncatedPackIsRejected

-- | Read a large blob at its index offset and hash the payload.
fullObjectPayload :: IO ()
fullObjectPayload =
    withPackedRepository $ \temporaryDir -> do
        (_, packPath) <- findPackFiles temporaryDir
        packBytes <- BS.readFile packPath
        packIndex <- readIndex (temporaryDir </> ".git" </> "objects" </> "pack")
        objectIdText <- trimTrailing <$> runGit ["-C", temporaryDir, "rev-parse", "HEAD:big.bin"]
        objectId <-
            maybe (fail "git returned an invalid object id") pure (objectIdFromHex objectIdText)
        offset <-
            maybe (fail "the big blob should be in the pack index") pure (lookupPackIndexOffset packIndex objectId)
        (rawObject, _) <- either fail pure (parsePackObjectAt packBytes offset)
        case rawObject of
            PackRawFull PackBlob payload -> do
                assertEqual bigBlobLength (BS.length payload) "The big blob should decompress to its full length"
                assertEqual objectId (objectIdForObject (gitObject BlobObject payload)) "The blob payload should hash to its id"
                assertEqual (BS.take 400 bigBlobContents) (BS.take 400 payload) "The big blob should start with the committed bytes"
            _ -> fail "The big blob should be stored as a full object in this fixture"

-- | Walk entries by consumed length and end at the trailer.
sequentialWalkReachesTheEnd :: IO ()
sequentialWalkReachesTheEnd =
    withPackedRepository $ \temporaryDir -> do
        (_, packPath) <- findPackFiles temporaryDir
        packBytes <- BS.readFile packPath
        (header, _) <- either fail pure (parsePackHeader packBytes)
        finalOffset <- walk packBytes (packHeaderObjectCount header) 12
        assertEqual (BS.length packBytes - 20) finalOffset "Walking the pack should end at the trailer"
  where
    walk _ 0 offset = pure offset
    walk packBytes remaining offset = do
        (_, consumed) <- either fail pure (parsePackObjectAt packBytes offset)
        walk packBytes (remaining - 1) (offset + consumed)

-- | Give the parser a truncated pack.
truncatedPackIsRejected :: IO ()
truncatedPackIsRejected =
    withPackedRepository $ \temporaryDir -> do
        (_, packPath) <- findPackFiles temporaryDir
        packBytes <- BS.readFile packPath
        case parsePackObjectAt (BS.take 40 packBytes) 12 of
            Left _ -> pure ()
            Right _ -> fail "A truncated pack should be rejected"

withPackedRepository :: (FilePath -> IO a) -> IO a
withPackedRepository action =
    withTempDirectory "haskell-git-pack-read" $ \temporaryDir -> do
        _ <- runGit ["init", "-q", "-b", "main", temporaryDir]
        _ <- runGit ["-C", temporaryDir, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", temporaryDir, "config", "user.name", "Ada"]
        BS.writeFile (temporaryDir </> "big.bin") bigBlobContents
        _ <- runGit ["-C", temporaryDir, "add", "-A"]
        _ <- runGit ["-C", temporaryDir, "commit", "-qm", "first"]
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        action temporaryDir

readIndex :: FilePath -> IO PackIndex
readIndex packDirectory = do
    names <- listDirectory packDirectory
    case [name | name <- names, ".idx" `isSuffixOf` name] of
        indexName : _ -> do
            indexBytes <- BS.readFile (packDirectory </> indexName)
            either fail pure (parsePackIndex indexBytes)
        [] -> fail "git gc did not create a pack index"

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

bigBlobLength :: Int
bigBlobLength = BS.length bigBlobContents

bigBlobContents :: BS.ByteString
bigBlobContents =
    BS.concat
        [ BS8.pack ("line " ++ show lineNumber ++ " ")
            <> BS.replicate 120 (fromIntegral (lineNumber `mod` 94) + 33)
            <> BS8.singleton '\n'
        | lineNumber <- [1 .. 2000 :: Int]
        ]

trimTrailing :: String -> String
trimTrailing = reverse . dropWhile (`elem` "\r\n") . reverse
