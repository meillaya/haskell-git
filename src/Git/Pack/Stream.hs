module Git.Pack.Stream
    ( decompressMemberFromHandle
    , readPackHeaderFromFile
    , readPackObjectAtFile
    , readPackObjectAtHandle
    , validatePackFile
    ) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import qualified Codec.Compression.Zlib.Internal as ZlibInternal
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Directory (getFileSize)
import System.IO
    ( Handle
    , IOMode (ReadMode)
    , SeekMode (AbsoluteSeek, RelativeSeek)
    , hSeek
    , withBinaryFile
    )
import Git.Hash
    ( HashAlgorithm (..)
    , hashSize
    , objectIdFromBytesFor
    )
import Git.Pack
    ( PackHeader
    , PackObjectKind (..)
    , parsePackHeader
    )
import Git.Pack.Read (PackRawObject (..), decodeObjectHeader, decodeOffsetDeltaBase)

-- | Read and parse the 12-byte pack header.
readPackHeaderFromFile :: FilePath -> IO PackHeader
readPackHeaderFromFile path =
    withBinaryFile path ReadMode $ \handle -> do
        headerBytes <- BS.hGet handle packHeaderLength
        case parsePackHeader headerBytes of
            Left err -> ioError (userError (path ++ ": " ++ err))
            Right (header, _) -> pure header

-- | Read one pack entry. The file is opened for each call; callers that walk a
-- pack sequentially should use 'readPackObjectAtHandle' instead.
readPackObjectAtFile
    :: HashAlgorithm
    -> FilePath
    -> Int
    -> IO (Either String (PackRawObject, Int))
readPackObjectAtFile objectFormat path offset = do
    attempt <-
        try
            ( withBinaryFile path ReadMode $ \handle ->
                readPackObjectAtHandle objectFormat handle offset
            ) ::
            IO (Either IOException (Either String (PackRawObject, Int)))
    pure
        ( case attempt of
            Left err -> Left (show err)
            Right result -> result
        )

-- | Read one pack entry from an open handle. The returned count is the total
-- bytes the entry occupies, header and compressed stream included.
readPackObjectAtHandle
    :: HashAlgorithm
    -> Handle
    -> Int
    -> IO (Either String (PackRawObject, Int))
readPackObjectAtHandle objectFormat handle offset
    | offset < 0 = pure (Left "malformed pack: object offset is outside the file")
    | otherwise = do
        hSeek handle AbsoluteSeek (fromIntegral offset)
        prefix <- BS.hGet handle maximumEntryPrefix
        if BS.null prefix
            then pure (Left "malformed pack: object offset is outside the file")
            else case decodeObjectHeader prefix 0 of
                Left err -> pure (Left err)
                Right (objectKind, declaredSize, headerLength) -> do
                    let objectIdLength = hashSize objectFormat
                    case objectKind of
                        PackCommit -> finish (PackRawFull PackCommit) headerLength declaredSize
                        PackTree -> finish (PackRawFull PackTree) headerLength declaredSize
                        PackBlob -> finish (PackRawFull PackBlob) headerLength declaredSize
                        PackTag -> finish (PackRawFull PackTag) headerLength declaredSize
                        PackOffsetDelta ->
                            case decodeOffsetDeltaBase prefix headerLength of
                                Left err -> pure (Left err)
                                Right (baseOffset, bodyStart) ->
                                    finish (PackRawOffsetDelta baseOffset) bodyStart declaredSize
                        PackReferenceDelta ->
                            if BS.length prefix < headerLength + objectIdLength
                                then pure (Left "malformed pack: truncated reference delta base id")
                                else
                                    case
                                        objectIdFromBytesFor
                                            objectFormat
                                            (BS.take objectIdLength (BS.drop headerLength prefix)) of
                                        Nothing ->
                                            pure (Left "malformed pack: invalid reference delta base id")
                                        Just baseObjectId ->
                                            finish
                                                (PackRawRefDelta baseObjectId)
                                                (headerLength + objectIdLength)
                                                declaredSize
  where
    finish makeObject bodyStart declaredSize = do
        hSeek handle AbsoluteSeek (fromIntegral (offset + bodyStart))
        decompressed <- decompressMemberFromHandle handle
        case decompressed of
            Left err -> pure (Left err)
            Right (payload, compressedConsumed) ->
                if BS.length payload /= declaredSize
                    then pure (Left "malformed pack: object payload size does not match its header")
                    else
                        pure
                            ( Right
                                ( makeObject payload
                                , bodyStart + compressedConsumed
                                )
                            )

-- | Inflate one zlib member starting at the current handle position. The
-- second value is the number of compressed bytes consumed; any look-ahead is
-- pushed back so the handle points at the next member.
decompressMemberFromHandle :: Handle -> IO (Either String (ByteString, Int))
decompressMemberFromHandle handle =
    go
        ( ZlibInternal.decompressIO
            ZlibInternal.zlibFormat
            ZlibInternal.defaultDecompressParams
                { ZlibInternal.decompressAllMembers = False
                }
        )
        0
        []
  where
    go stream totalRead chunks =
        case stream of
            ZlibInternal.DecompressInputRequired supply -> do
                chunk <- BS.hGetSome handle decompressionChunkSize
                if BS.null chunk
                    then pure (Left "failed to decode pack member: truncated input")
                    else do
                        next <- supply chunk
                        go next (totalRead + BS.length chunk) chunks
            ZlibInternal.DecompressOutputAvailable output next -> do
                nextStream <- next
                go nextStream totalRead (output : chunks)
            ZlibInternal.DecompressStreamEnd unconsumed -> do
                let unconsumedLength = BS.length unconsumed
                    consumed = totalRead - unconsumedLength
                when (unconsumedLength > 0) $
                    hSeek handle RelativeSeek (negate (fromIntegral unconsumedLength))
                pure (Right (BS.concat (reverse chunks), consumed))
            ZlibInternal.DecompressStreamError err ->
                pure (Left ("failed to decode pack member: " ++ show err))

-- | Verify the pack trailer checksum without holding the pack in memory.
validatePackFile :: HashAlgorithm -> FilePath -> IO ()
validatePackFile objectFormat path = do
    size <- getFileSize path
    let hashLength = hashSize objectFormat
        minimumSize = fromIntegral (packHeaderLength + hashLength)
    if size < minimumSize
        then ioError (userError (path ++ ": malformed pack: file is too short"))
        else withBinaryFile path ReadMode $ \handle -> do
            headerBytes <- BS.hGet handle packHeaderLength
            case parsePackHeader headerBytes of
                Left err -> ioError (userError (path ++ ": " ++ err))
                Right _ -> pure ()
            hSeek handle AbsoluteSeek 0
            case objectFormat of
                Sha1 -> do
                    digest <-
                        hashChunks
                            SHA1.init
                            SHA1.update
                            SHA1.finalize
                            handle
                            (size - fromIntegral hashLength)
                    verifyTrailer handle hashLength digest
                Sha256 -> do
                    digest <-
                        hashChunks
                            SHA256.init
                            SHA256.update
                            SHA256.finalize
                            handle
                            (size - fromIntegral hashLength)
                    verifyTrailer handle hashLength digest

verifyTrailer :: Handle -> Int -> ByteString -> IO ()
verifyTrailer handle hashLength digest = do
    trailer <- BS.hGet handle hashLength
    if digest /= trailer
        then ioError (userError "malformed pack: checksum mismatch")
        else pure ()

hashChunks
    :: context
    -> (context -> ByteString -> context)
    -> (context -> ByteString)
    -> Handle
    -> Integer
    -> IO ByteString
hashChunks initialContext updateContext finalizeContext handle remaining
    | remaining <= 0 = pure (finalizeContext initialContext)
    | otherwise = do
        let chunkSize = fromIntegral (min remaining hashChunkSize)
        chunk <- BS.hGet handle chunkSize
        if BS.null chunk
            then ioError (userError "malformed pack: truncated checksum input")
            else
                hashChunks
                    (updateContext initialContext chunk)
                    updateContext
                    finalizeContext
                    handle
                    (remaining - fromIntegral (BS.length chunk))

packHeaderLength :: Int
packHeaderLength = 12

maximumEntryPrefix :: Int
maximumEntryPrefix = 64

decompressionChunkSize :: Int
decompressionChunkSize = 32 * 1024

hashChunkSize :: Integer
hashChunkSize = 64 * 1024
