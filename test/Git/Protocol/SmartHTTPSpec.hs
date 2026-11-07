module Git.Protocol.SmartHTTPSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Protocol.PacketLine (encodePacketLine, flushPacket, packetLine)
import Git.Protocol.SmartHTTP
    ( capabilityValue
    , defaultSmartHttpEndpoint
    , discoveryCapabilities
    , discoveryRefName
    , discoveryRefs
    , parseDiscoveryResponse
    , uploadPackPath
    )
import TestSupport (assert, assertContains, assertEqual)

-- | Check Smart HTTP discovery: endpoint derivation, capability parsing, the
-- empty repository advertisement, and remote error packets.
run :: IO ()
run = do
    endpointPaths
    advertisementParsing
    emptyAdvertisement
    errorPacket
    rejectsWrongService

-- | Derive both endpoint paths, with and without a trailing slash.
endpointPaths :: IO ()
endpointPaths = do
    let endpoint = defaultSmartHttpEndpoint "https://example.com/repo.git"
    assertEqual
        "https://example.com/repo.git/git-upload-pack"
        (uploadPackPath endpoint)
        "The upload-pack path should be derived from the base URL"
    let trailingSlashEndpoint = defaultSmartHttpEndpoint "https://example.com/repo.git/"
    assertEqual
        "https://example.com/repo.git/git-upload-pack"
        (uploadPackPath trailingSlashEndpoint)
        "A trailing slash should be dropped from the remote URL"

-- | Parse a v0 advertisement with HEAD, branches, and a peeled tag.
advertisementParsing :: IO ()
advertisementParsing = do
    let commitId = "8c2cc57cd2962d4c7476507f383f16907b9843af"
        tagId = "c4687b5295db17df345c68368c373175c886f743"
        response =
            BS8.concat
                [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
                , encodePacketLine flushPacket
                , encodePacketLine
                    ( packetLine
                        (BS8.pack
                            ( commitId
                                ++ " HEAD\NULmulti_ack ofs-delta include-tag symref=HEAD:refs/heads/main object-format=sha1\n"
                            )
                        )
                    )
                , encodePacketLine (packetLine (BS8.pack (commitId ++ " refs/heads/main\n")))
                , encodePacketLine (packetLine (BS8.pack (tagId ++ " refs/tags/v1\n")))
                , encodePacketLine (packetLine (BS8.pack (commitId ++ " refs/tags/v1^{}\n")))
                , encodePacketLine flushPacket
                ]
    advertisement <- either fail pure (parseDiscoveryResponse response)
    assertEqual
        ["HEAD", "refs/heads/main", "refs/tags/v1", "refs/tags/v1^{}"]
        (map (BS8.unpack . discoveryRefName) (discoveryRefs advertisement))
        "Discovery refs should include HEAD, branches, tags, and peeled tags"
    assertEqual
        (Just (BS8.pack "HEAD:refs/heads/main"))
        (capabilityValue (BS8.pack "symref") (discoveryCapabilities advertisement))
        "The symref capability should be readable"

-- | Parse the empty-repository pseudo ref.
emptyAdvertisement :: IO ()
emptyAdvertisement = do
    let zeroId = BS8.pack (replicate 40 '0')
        response =
            BS8.concat
                [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
                , encodePacketLine flushPacket
                , encodePacketLine
                    ( packetLine
                        (zeroId <> BS8.pack " capabilities^{}\0multi_ack ofs-delta include-tag\n")
                    )
                , encodePacketLine flushPacket
                ]
    advertisement <- either fail pure (parseDiscoveryResponse response)
    assertEqual [] (discoveryRefs advertisement) "An empty repository should advertise no refs"
    assert ("multi_ack" `elem` map BS8.unpack (discoveryCapabilities advertisement)) "An empty repository should still advertise capabilities"

-- | Parse an ERR packet as an error.
errorPacket :: IO ()
errorPacket = do
    let response =
            BS8.concat
                [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
                , encodePacketLine flushPacket
                , encodePacketLine (packetLine (BS8.pack "ERR repository not found\n"))
                ]
    case parseDiscoveryResponse response of
        Left message -> assertContains "repository not found" message "A remote error should be reported"
        Right _ -> fail "An ERR packet should not parse as an advertisement"

-- | Parse a receive-pack banner and expect a refusal.
rejectsWrongService :: IO ()
rejectsWrongService =
    case parseDiscoveryResponse (encodePacketLine (packetLine (BS8.pack "# service=git-receive-pack\n"))) of
        Left message -> assertContains "service line" message "An unexpected service line should be named"
        Right _ -> fail "A receive-pack banner should not parse as an upload-pack advertisement"
