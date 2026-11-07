module Git.Object.Loose
    ( LooseObject
    , listLooseObjectIds
    , looseObject
    , looseObjectId
    , looseObjectPath
    , looseObjectType
    , objectIdForObject
    , objectIdForObjectFor
    , readLooseObject
    , writeLooseObject
    ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM)
import qualified Codec.Compression.Zlib as Zlib
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , emptyPermissions
    , getPermissions
    , listDirectory
    , removeFile
    , renameFile
    , setOwnerReadable
    , setOwnerWritable
    , setPermissions
    )
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hClose, openTempFile)
import Git.Hash
    ( HashAlgorithm
    , ObjectId
    , hashBytes
    , hashBytesFor
    , objectIdBytes
    , objectIdFromHexFor
    , objectIdLengthFor
    , objectIdPath
    , objectIdToHex
    )
import Git.Object (GitObject, GitObjectType, decodeGitObject, encodeGitObject)
import Git.Repo (Repository (..), repositoryObjectsDir)

data LooseObject = LooseObject
    { looseObjectId :: ObjectId
    , looseObjectType :: GitObjectType
    }
    deriving (Eq, Show)

looseObject :: ObjectId -> GitObjectType -> LooseObject
looseObject = LooseObject

objectIdForObject :: GitObject -> ObjectId
objectIdForObject = hashBytes . encodeGitObject

objectIdForObjectFor :: HashAlgorithm -> GitObject -> ObjectId
objectIdForObjectFor hashAlgorithm = hashBytesFor hashAlgorithm . encodeGitObject

-- | Write one object as a zlib-compressed loose file. The write goes through a
-- temporary file and a rename so an interrupted process cannot leave a
-- truncated object under a valid id, and an existing object is left alone.
writeLooseObject :: Repository -> GitObject -> IO ObjectId
writeLooseObject repository currentObject = do
    let encodedObject = encodeGitObject currentObject
        objectId = objectIdForObjectFor (repositoryObjectFormat repository) currentObject
        objectPath = looseObjectPath repository objectId
        objectDirectory = takeDirectory objectPath
    createDirectoryIfMissing True objectDirectory
    objectExists <- doesFileExist objectPath
    if objectExists
        then pure objectId
        else do
            (temporaryPath, handle) <-
                openTempFile objectDirectory (takeFileName objectPath ++ ".tmp")
            BL.hPut handle (Zlib.compress (BL.fromStrict encodedObject))
            hClose handle
            renameFile temporaryPath objectPath
            permissions <- getPermissions objectPath
            setPermissions objectPath (setOwnerWritable False (setOwnerReadable True permissions))
            pure objectId

readLooseObject :: Repository -> ObjectId -> IO GitObject
readLooseObject repository objectId = do
    let objectFormat = repositoryObjectFormat repository
        expectedLength = objectIdLengthFor objectFormat
    if BS.length (objectIdBytes objectId) /= expectedLength
        then
            ioError
                ( userError
                    ( "object id length does not match the repository object format: "
                        ++ objectIdToHex objectId
                    )
                )
        else do
            let objectPath = looseObjectPath repository objectId
            objectExists <- doesFileExist objectPath
            if not objectExists
                then ioError (userError ("Object not found: " ++ objectIdToHex objectId))
                else do
                    compressedObject <- BL.readFile objectPath
                    decompressedResult <-
                        try
                            (evaluate (BL.toStrict (Zlib.decompress compressedObject))) ::
                            IO (Either SomeException ByteString)
                    case decompressedResult of
                        Left decompressionError ->
                            ioError
                                (userError
                                    ("Failed to decompress loose object: " ++ displayException decompressionError))
                        Right rawObject ->
                            case decodeGitObject rawObject of
                                Left err -> ioError (userError err)
                                Right gitObjectValue -> pure gitObjectValue

-- | Every object id that has a loose file in the repository. Directories that
-- git reserves (@info@, @pack@) are skipped.
listLooseObjectIds :: Repository -> IO [ObjectId]
listLooseObjectIds repository = do
    let objectFormat = repositoryObjectFormat repository
        objectsDirectory = repositoryObjectsDir repository
    objectsDirectoryExists <- doesDirectoryExist objectsDirectory
    if not objectsDirectoryExists
        then pure []
        else do
            prefixes <- listDirectory objectsDirectory
            fmap concat $
                forM (filter isObjectPrefix prefixes) $ \prefix -> do
                    let prefixDirectory = objectsDirectory </> prefix
                    isDirectory <- doesDirectoryExist prefixDirectory
                    if not isDirectory
                        then pure []
                        else do
                            names <- listDirectory prefixDirectory
                            pure
                                ( catMaybes
                                    [ objectIdFromHexFor objectFormat (prefix ++ name)
                                    | name <- names
                                    ]
                                )
  where
    isObjectPrefix prefix = length prefix == 2 && all isHexCharacter prefix
    isHexCharacter character =
        (character >= '0' && character <= '9')
            || (character >= 'a' && character <= 'f')

looseObjectPath :: Repository -> ObjectId -> FilePath
looseObjectPath repository objectId =
    let (prefix, suffix) = objectIdPath objectId
     in repositoryObjectsDir repository </> prefix </> suffix
