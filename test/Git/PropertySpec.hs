module Git.PropertySpec
    ( run
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Hash (HashAlgorithm (..))
import Git.Object (GitObject, GitObjectType (..), gitObject, gitObjectPayload, gitObjectType)
import Git.Pack (PackHeader (..), PackObjectKind (..))
import Git.Pack.Read (PackRawObject (..))
import Git.Pack.Stream
    ( readPackHeaderFromFile
    , readPackObjectAtFile
    , validatePackFile
    )
import Git.Pack.Write (writePackFile)
import System.FilePath ((</>))
import TestSupport (assertEqual, withTempDirectory)

run :: IO ()
run = do
    packRoundTripsFor Sha1
    packRoundTripsFor Sha256

packRoundTripsFor :: HashAlgorithm -> IO ()
packRoundTripsFor objectFormat =
    withTempDirectory ("haskell-git-pack-property-" ++ show objectFormat) $ \temporaryDir -> do
        let path = temporaryDir </> "roundtrip.pack"
            objects =
                [ gitObject BlobObject (BS8.pack ("blob-" ++ show index ++ "\n"))
                | index <- [0 .. 19]
                ]
                    ++ [gitObject TreeObject (BS8.pack "tree payload")]
                    ++ [ gitObject BlobObject (BS.replicate 200 (fromIntegral (65 + index)))
                       | index <- [0 .. 5]
                       ]
            expected =
                [ (gitObjectPayload value, objectKind (gitObjectType value))
                | value <- objects
                ]
        writePackFile objectFormat objects path
        validatePackFile objectFormat path
        header <- readPackHeaderFromFile path
        assertEqual (length objects) (packHeaderObjectCount header) "pack header should count every object"
        actual <- readEntries objectFormat path (length objects) 12 []
        assertEqual expected actual "pack entries should round-trip through the streaming reader"
  where
    readEntries _ _ 0 _ accumulated = pure (reverse accumulated)
    readEntries format path remaining offset accumulated = do
        result <- readPackObjectAtFile format path offset
        case result of
            Left err -> fail ("pack property read failed: " ++ err)
            Right (raw, consumed) ->
                let nextOffset = offset + consumed
                    payload =
                        case raw of
                            PackRawFull _ bytes -> bytes
                            _ -> BS.empty
                    kind =
                        case raw of
                            PackRawFull packKind _ -> packKind
                            _ -> PackBlob
                 in readEntries format path (remaining - 1) nextOffset ((payload, kind) : accumulated)

objectKind :: GitObjectType -> PackObjectKind
objectKind BlobObject = PackBlob
objectKind TreeObject = PackTree
objectKind CommitObject = PackCommit
objectKind TagObject = PackTag
