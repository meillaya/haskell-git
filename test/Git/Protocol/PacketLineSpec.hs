module Git.Protocol.PacketLineSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Protocol.PacketLine (decodePacketLines, encodePacketLine, flushPacket, packetLine, packetLinePayload)
import TestSupport (assertEqual)

run :: IO ()
run = do
    let encoded =
            encodePacketLine (packetLine (BS8.pack "hello\n"))
                <> encodePacketLine flushPacket
    decoded <- either fail pure (decodePacketLines encoded)
    assertEqual
        [ Just (BS8.pack "hello\n")
        , Nothing
        ]
        (map packetLinePayload decoded)
        "pkt-line codec should preserve data and flush packets"
    assertEqual
        (Left "Malformed pkt-line stream: invalid length prefix")
        (decodePacketLines (BS8.pack "zzzz"))
        "pkt-line decoder should reject invalid hex length prefixes"
    assertEqual
        (Left "Malformed pkt-line stream: truncated payload")
        (decodePacketLines (BS8.pack "0008abc"))
        "pkt-line decoder should reject truncated payloads"
