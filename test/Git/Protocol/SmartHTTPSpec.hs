module Git.Protocol.SmartHTTPSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Protocol.PacketLine (encodePacketLine, flushPacket, packetLine)
import Git.Protocol.SmartHTTP
    ( defaultSmartHttpEndpoint,
      discoveryCapabilities,
      discoveryRefName,
      discoveryRefs,
      parseDiscoveryResponse,
      uploadPackPath )
import TestSupport (assertEqual)

run :: IO ()
run = do
    let endpoint = defaultSmartHttpEndpoint "https://example.com/repo.git"
    assertEqual
        "https://example.com/repo.git/git-upload-pack"
        (uploadPackPath endpoint)
        "Smart HTTP upload-pack path should be derived from the base URL"
    let response =
            mconcat
                [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
                , encodePacketLine flushPacket
                , encodePacketLine
                    (packetLine
                        (BS8.pack "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a HEAD\NULmulti_ack thin-pack\n"))
                , encodePacketLine
                    (packetLine
                        (BS8.pack "716f9b16f57f2fb17e7e1d1bda17f4cc1f2ac27a refs/heads/main\n"))
                ]
    advertisement <- either fail pure (parseDiscoveryResponse response)
    assertEqual
        [BS8.pack "multi_ack", BS8.pack "thin-pack"]
        (discoveryCapabilities advertisement)
        "Discovery capabilities should parse from the first advertised ref"
    assertEqual
        [BS8.pack "HEAD", BS8.pack "refs/heads/main"]
        (map discoveryRefName (discoveryRefs advertisement))
        "Discovery refs should include HEAD and the branch ref"
    assertEqual
        (Left "Unexpected Smart HTTP service line")
        (parseDiscoveryResponse (encodePacketLine (packetLine (BS8.pack "# service=git-receive-pack\n"))))
        "Discovery parser should reject unexpected service banners"
