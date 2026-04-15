module Git.Object.Loose
    ( LooseObject
    , looseObject
    , looseObjectId
    , looseObjectType
    , objectIdForObject
    , readLooseObject
    , writeLooseObject
    ) where

import Control.Exception (SomeException, displayException, try, evaluate)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Git.Hash (ObjectId)
import Git.Hash (hashBytes, objectIdPath)
import Git.Object (GitObject, GitObjectType, decodeGitObject, encodeGitObject)
import Git.Repo (Repository, repositoryObjectsDir)

data LooseObject = LooseObject
    { looseObjectId :: ObjectId
    , looseObjectType :: GitObjectType
    }
    deriving (Eq, Show)

looseObject :: ObjectId -> GitObjectType -> LooseObject
looseObject = LooseObject

objectIdForObject :: GitObject -> ObjectId
objectIdForObject = hashBytes . encodeGitObject

writeLooseObject :: Repository -> GitObject -> IO ObjectId
writeLooseObject repository currentObject = do
    let encodedObject = encodeGitObject currentObject
        objectId = hashBytes encodedObject
        objectPath = looseObjectPath repository objectId
    createDirectoryIfMissing True (takeDirectory objectPath)
    BL.writeFile objectPath (Zlib.compress (BL.fromStrict encodedObject))
    pure objectId

readLooseObject :: Repository -> ObjectId -> IO GitObject
readLooseObject repository objectId = do
    let objectPath = looseObjectPath repository objectId
    objectExists <- doesFileExist objectPath
    if not objectExists
        then ioError (userError ("Object not found: " ++ objectPath))
        else do
            compressedObject <- BL.readFile objectPath
            decompressedResult <-
                try
                    (evaluate (BL.toStrict (Zlib.decompress compressedObject))) ::
                    IO (Either SomeException BS.ByteString)
            case decompressedResult of
                Left decompressionError ->
                    ioError
                        (userError
                            ("Failed to decompress loose object: " ++ displayException decompressionError))
                Right rawObject ->
                    case decodeGitObject rawObject of
                        Left err -> ioError (userError err)
                        Right gitObjectValue -> pure gitObjectValue

looseObjectPath :: Repository -> ObjectId -> FilePath
looseObjectPath repository objectId =
    let (prefix, suffix) = objectIdPath objectId
     in repositoryObjectsDir repository </> prefix </> suffix
