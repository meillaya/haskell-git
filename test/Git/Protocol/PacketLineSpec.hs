module Git.Protocol.PacketLineSpec
    ( run
    ) where

import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as BS8
import Git.Protocol.PacketLine
    ( PacketLineV2 (..)
    , decodePacketLines
    , decodePacketLinesV2
    , encodeDelimPacketV2
    , encodePacketLine
    , encodePacketLineV2
    , encodeResponseEndPacketV2
    , flushPacket
    , packetLine
    , packetLinePayload
    )
import TestSupport (assert, assertContains, assertEqual)

-- | Check pkt-line framing: data and flush packets, empty data packets, the
-- length checks, and the protocol v2 special packets the v0 codec refuses.
run :: IO ()
run = do
    roundTrip
    emptyDataPacket
    rejectsMalformed
    specialPackets
    v2RoundTrip

-- | Round-trip a data packet and a flush packet.
roundTrip :: IO ()
roundTrip = do
    let encoded =
            encodePacketLine (packetLine (BS8.pack "hello\n"))
                <> encodePacketLine flushPacket
    decoded <- either fail pure (decodePacketLines encoded)
    assertEqual
        [Just (BS8.pack "hello\n"), Nothing]
        (map packetLinePayload decoded)
        "pkt-line codec should preserve data and flush packets"

-- | Decode a four-byte length as an empty data packet.
emptyDataPacket :: IO ()
emptyDataPacket = do
    decoded <- either fail pure (decodePacketLines (BS8.pack "0004"))
    assertEqual [Just BS8.empty] (map packetLinePayload decoded) "A length of four should be an empty data packet"

-- | Give the decoder bad prefixes, truncation, and overlong lengths.
rejectsMalformed :: IO ()
rejectsMalformed = do
    assertEqual
        (Left "Malformed pkt-line stream: invalid length prefix")
        (decodePacketLines (BS8.pack "zzzz"))
        "pkt-line decoder should reject invalid hex length prefixes"
    assertEqual
        (Left "Malformed pkt-line stream: truncated payload")
        (decodePacketLines (BS8.pack "0008abc"))
        "pkt-line decoder should reject truncated payloads"
    result <- pure (decodePacketLines (BS8.pack "ffff"))
    case result of
        Left message -> assertContains "overflows the maximum" message "An overlong length should be named"
        Right _ -> fail "A length above the protocol maximum should be rejected"
    case decodePacketLines (BS8.pack "00") of
        Left _ -> pure ()
        Right _ -> fail "A truncated length prefix should be rejected"

-- | Check that protocol v2 special packets are refused by name.
v2RoundTrip :: IO ()
v2RoundTrip = do
    let encoded =
            encodePacketLineV2 (DataPacketV2 (BS8.pack "command=ls-refs\n"))
                <> encodeDelimPacketV2
                <> encodePacketLineV2 FlushPacketV2
                <> encodeResponseEndPacketV2
    decoded <- either fail pure (decodePacketLinesV2 encoded)
    assertEqual
        [ DataPacketV2 (BS8.pack "command=ls-refs\n")
        , DelimPacketV2
        , FlushPacketV2
        , ResponseEndPacketV2
        ]
        decoded
        "The v2 pkt-line codec should preserve data, delimiter, flush, and response-end packets"

specialPackets :: IO ()
specialPackets = do
    case decodePacketLines (BS8.pack "0001") of
        Left message -> assert ("protocol v2 special packet" `isInfixOf` message) "A delimiter packet should be named"
        Right _ -> fail "A protocol v2 delimiter should be rejected by the v0 codec"
    case decodePacketLines (BS8.pack "0002") of
        Left _ -> pure ()
        Right _ -> fail "A response-end packet should be rejected by the v0 codec"
