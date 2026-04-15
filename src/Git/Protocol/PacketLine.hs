module Git.Protocol.PacketLine
    ( PacketLine
    , decodePacketLines
    , encodePacketLine
    , flushPacket
    , flushPacketLength
    , packetLine
    , packetLinePayload
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Numeric (readHex, showHex)

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
                let payloadLength = packetLength - 4
                if payloadLength < 0 || BS.length remaining < payloadLength
                    then Left "Malformed pkt-line stream: truncated payload"
                    else do
                        let (payload, rest) = BS.splitAt payloadLength remaining
                        (DataPacket payload :) <$> decodePacketLines rest
  where
    parseLength prefix =
        case readHex (BS8.unpack prefix) of
            [(parsedLength, "")] -> Right parsedLength
            _ -> Left "Malformed pkt-line stream: invalid length prefix"

padHex :: Int -> String
padHex value =
    let rendered = showHex value ""
     in replicate (4 - length rendered) '0' ++ rendered
