module Git.Protocol.PacketLine
    ( PacketLine (..)
    , PacketLineV2 (..)
    , decodePacketLines
    , decodePacketLinesV2
    , encodeDelimPacketV2
    , encodePacketLine
    , encodePacketLineV2
    , encodeResponseEndPacketV2
    , flushPacket
    , flushPacketLength
    , packetLine
    , packetLinePayload
    , readPacketLineFromHandle
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Numeric (readHex, showHex)
import System.IO (Handle)

data PacketLine
    = DataPacket ByteString
    | FlushPacket
    deriving (Eq, Show)

flushPacketLength :: Int
flushPacketLength = 0

flushPacket :: PacketLine
flushPacket = FlushPacket

packetLine :: ByteString -> PacketLine
packetLine = DataPacket

packetLinePayload :: PacketLine -> Maybe ByteString
packetLinePayload (DataPacket payload) = Just payload
packetLinePayload FlushPacket = Nothing

encodePacketLine :: PacketLine -> ByteString
encodePacketLine FlushPacket = BS8.pack "0000"
encodePacketLine (DataPacket payload) =
    let totalLength = BS.length payload + 4
     in BS8.pack (padHex totalLength) <> payload

data PacketLineV2
    = DataPacketV2 ByteString
    | FlushPacketV2
    | DelimPacketV2
    | ResponseEndPacketV2
    deriving (Eq, Show)

encodePacketLineV2 :: PacketLineV2 -> ByteString
encodePacketLineV2 FlushPacketV2 = BS8.pack "0000"
encodePacketLineV2 DelimPacketV2 = BS8.pack "0001"
encodePacketLineV2 ResponseEndPacketV2 = BS8.pack "0002"
encodePacketLineV2 (DataPacketV2 payload) =
    let totalLength = BS.length payload + 4
     in BS8.pack (padHex totalLength) <> payload

encodeDelimPacketV2 :: ByteString
encodeDelimPacketV2 = encodePacketLineV2 DelimPacketV2

encodeResponseEndPacketV2 :: ByteString
encodeResponseEndPacketV2 = encodePacketLineV2 ResponseEndPacketV2

decodePacketLinesV2 :: ByteString -> Either String [PacketLineV2]
decodePacketLinesV2 rawBytes
    | BS.null rawBytes = Right []
    | BS.length rawBytes < 4 = Left "Malformed pkt-line stream: truncated length prefix"
    | otherwise = do
        let (lengthPrefix, remaining) = BS.splitAt 4 rawBytes
        packetLength <- parseLengthV2 lengthPrefix
        case packetLength of
            0 -> (FlushPacketV2 :) <$> decodePacketLinesV2 remaining
            1 -> (DelimPacketV2 :) <$> decodePacketLinesV2 remaining
            2 -> (ResponseEndPacketV2 :) <$> decodePacketLinesV2 remaining
            _ ->
                if packetLength <= specialPacketLimit
                    then Left "Malformed pkt-line stream: invalid packet length"
                    else do
                        let payloadLength = packetLength - 4
                        if BS.length remaining < payloadLength
                            then Left "Malformed pkt-line stream: truncated payload"
                            else do
                                let (payload, rest) = BS.splitAt payloadLength remaining
                                (DataPacketV2 payload :) <$> decodePacketLinesV2 rest
  where
    parseLengthV2 prefix =
        case readHex (BS8.unpack prefix) of
            [(parsedLength, "")] | parsedLength <= maximumPacketLength -> Right parsedLength
            [(_, "")] -> Left "Malformed pkt-line stream: length prefix overflows the maximum"
            _ -> Left "Malformed pkt-line stream: invalid length prefix"

decodePacketLines :: ByteString -> Either String [PacketLine]
decodePacketLines rawBytes
    | BS.null rawBytes = Right []
    | BS.length rawBytes < 4 = Left "Malformed pkt-line stream: truncated length prefix"
    | otherwise = do
        let (lengthPrefix, remaining) = BS.splitAt 4 rawBytes
        packetLength <- parseLength lengthPrefix
        if packetLength == flushPacketLength
            then (FlushPacket :) <$> decodePacketLines remaining
            else do
                -- Lengths 1 to 3 mark protocol v2 delimiters. A v0 stream never
                -- contains one, so name the problem instead of calling it a
                -- truncated payload.
                if packetLength <= specialPacketLimit
                    then Left "Malformed pkt-line stream: protocol v2 special packet in a v0 stream"
                    else do
                        let payloadLength = packetLength - 4
                        if BS.length remaining < payloadLength
                            then Left "Malformed pkt-line stream: truncated payload"
                            else do
                                let (payload, rest) = BS.splitAt payloadLength remaining
                                (DataPacket payload :) <$> decodePacketLines rest
  where
    parseLength prefix =
        case readHex (BS8.unpack prefix) of
            [(parsedLength, "")] | parsedLength <= maximumPacketLength -> Right parsedLength
            [(_, "")] -> Left "Malformed pkt-line stream: length prefix overflows the maximum"
            _ -> Left "Malformed pkt-line stream: invalid length prefix"

specialPacketLimit :: Int
specialPacketLimit = 3

readPacketLineFromHandle :: Handle -> IO (Either String PacketLine)
readPacketLineFromHandle handle = do
    prefix <- hGetExact 4
    case prefix of
        Left err -> pure (Left err)
        Right prefixBytes ->
            case readHex (BS8.unpack prefixBytes) of
                [(0, "")] -> pure (Right FlushPacket)
                [(parsedLength, "")]
                    | parsedLength <= specialPacketLimit ->
                        pure (Left "Malformed pkt-line stream: protocol v2 special packet in a v0 stream")
                    | parsedLength > maximumPacketLength ->
                        pure (Left "Malformed pkt-line stream: length prefix overflows the maximum")
                    | otherwise -> do
                        payload <- hGetExact (parsedLength - 4)
                        pure (DataPacket <$> payload)
                _ -> pure (Left "Malformed pkt-line stream: invalid length prefix")
  where
    hGetExact count
        | count <= 0 = pure (Right BS.empty)
        | otherwise = do
            bytes <- BS.hGet handle count
            if BS.length bytes == count
                then pure (Right bytes)
                else pure (Left "Malformed pkt-line stream: truncated packet")

maximumPacketLength :: Int
maximumPacketLength = 65520

padHex :: Int -> String
padHex value =
    let rendered = showHex value ""
     in replicate (4 - length rendered) '0' ++ rendered
