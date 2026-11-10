module Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement (..)
    , DiscoveryRef (..)
    , SmartHttpEndpoint (..)
    , capabilityValue
    , defaultSmartHttpEndpoint
    , discoveryCapabilities
    , discoveryObjectFormat
    , discoveryProtocolVersion
    , discoveryRefName
    , discoveryRefObjectId
    , discoveryRefs
    , parseDiscoveryResponse
    , parseDiscoveryResponseFor
    , parseRefAdvertisement
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Hash
    ( HashAlgorithm (..)
    , ObjectId
    , defaultHashAlgorithm
    , hashAlgorithmFromName
    , hashSize
    , isZeroObjectId
    , objectIdBytes
    , objectIdFromHex
    )
import Git.Protocol.PacketLine (decodePacketLines, packetLinePayload)

data SmartHttpEndpoint = SmartHttpEndpoint
    { discoveryPath :: String
    , uploadPackPath :: String
    , receivePackPath :: String
    , receiveDiscoveryPath :: String
    }
    deriving (Eq, Show)

data DiscoveryRef = DiscoveryRef
    { discoveryRefName :: ByteString
    , discoveryRefObjectId :: ObjectId
    }
    deriving (Eq, Show)

data DiscoveryAdvertisement = DiscoveryAdvertisement
    { discoveryCapabilities :: [ByteString]
    , discoveryRefs :: [DiscoveryRef]
    , discoveryObjectFormat :: HashAlgorithm
    , discoveryProtocolVersion :: Int
    }
    deriving (Eq, Show)

-- | Derive the Smart HTTP endpoints. A trailing slash on the remote URL must
-- be dropped; git refuses to serve a double slash.
defaultSmartHttpEndpoint :: String -> SmartHttpEndpoint
defaultSmartHttpEndpoint baseUrl =
    SmartHttpEndpoint
        { discoveryPath = trimmedBaseUrl ++ "/info/refs?service=git-upload-pack"
        , uploadPackPath = trimmedBaseUrl ++ "/git-upload-pack"
        , receivePackPath = trimmedBaseUrl ++ "/git-receive-pack"
        , receiveDiscoveryPath = trimmedBaseUrl ++ "/info/refs?service=git-receive-pack"
        }
  where
    trimmedBaseUrl = reverse (dropWhile (== '/') (reverse baseUrl))

-- | Read a capability value, such as @symref@ or @object-format@, out of the
-- advertisement. A capability without a value yields Nothing.
capabilityValue :: ByteString -> [ByteString] -> Maybe ByteString
capabilityValue name capabilities =
    firstPresent (map parser capabilities)
  where
    prefix = name <> BS8.singleton '='
    parser capability
        | prefix `BS8.isPrefixOf` capability = Just (BS.drop (BS.length prefix) capability)
        | otherwise = Nothing

parseDiscoveryResponse :: ByteString -> Either String DiscoveryAdvertisement
parseDiscoveryResponse =
    parseDiscoveryResponseFor (BS8.pack "# service=git-upload-pack\n")

-- | Parse a discovery response for a named service. Upload-pack and
-- receive-pack use different service banners but share the ref format.
parseDiscoveryResponseFor :: ByteString -> ByteString -> Either String DiscoveryAdvertisement
parseDiscoveryResponseFor expectedServiceLine rawResponse = do
    packetLines <- decodePacketLines rawResponse
    let payloads = [payload | Just payload <- map packetLinePayload packetLines]
    case [payload | payload <- payloads, BS8.isPrefixOf (BS8.pack "ERR ") payload] of
        errorLine : _ ->
            Left ("remote error: " ++ trimTrailingNewlineString (BS8.unpack (BS.drop 4 errorLine)))
        [] -> pure ()
    case payloads of
        serviceLine : refLines
            | serviceLine == expectedServiceLine -> parseRefs refLines
            | otherwise -> Left "unexpected Smart HTTP service line"
        [] -> Left "empty Smart HTTP discovery response"

-- | Parse the ref lines of a Smart HTTP advertisement. The SSH and git://
-- transports have the same ref format but no service banner, so they use this
-- entry point directly.
parseRefAdvertisement :: [ByteString] -> Either String DiscoveryAdvertisement
parseRefAdvertisement = parseRefs

-- | The advertisement lists each ref once, plus a peeled line for annotated
-- tags and, for an empty repository, a pseudo line with a zero id. The object
-- format comes from the @object-format@ capability; old SHA-1 advertisements
-- omit it, in which case the id length decides.
parseRefs :: [ByteString] -> Either String DiscoveryAdvertisement
parseRefs refLines =
    case filter (not . BS.null) refLines of
        [] -> Right (DiscoveryAdvertisement [] [] defaultHashAlgorithm 0)
        firstRef : remainingRefs -> do
            (firstDiscoveryRef, capabilities) <- parseFirstRef firstRef
            objectFormat <- resolveObjectFormat capabilities firstDiscoveryRef
            validateRefObjectFormat objectFormat firstDiscoveryRef
            if isCapabilitiesPseudoRef firstDiscoveryRef
                then Right (DiscoveryAdvertisement capabilities [] objectFormat 0)
                else do
                    otherRefs <- traverse (parseSubsequentRef objectFormat) remainingRefs
                    Right
                        ( DiscoveryAdvertisement
                            capabilities
                            (firstDiscoveryRef : otherRefs)
                            objectFormat
                            0
                        )

resolveObjectFormat :: [ByteString] -> DiscoveryRef -> Either String HashAlgorithm
resolveObjectFormat capabilities firstRef =
    case capabilityValue (BS8.pack "object-format") capabilities of
        Just value ->
            case hashAlgorithmFromName value of
                Just objectFormat -> Right objectFormat
                Nothing ->
                    Left
                        ( "remote uses an unsupported object format: "
                            ++ BS8.unpack value
                        )
        Nothing -> inferObjectFormat (discoveryRefObjectId firstRef)

inferObjectFormat :: ObjectId -> Either String HashAlgorithm
inferObjectFormat objectId =
    case BS.length (objectIdBytes objectId) of
        20 -> Right Sha1
        32 -> Right Sha256
        _ -> Left "malformed Smart HTTP ref line: unsupported object id length"

validateRefObjectFormat :: HashAlgorithm -> DiscoveryRef -> Either String ()
validateRefObjectFormat objectFormat discoveryRef =
    if BS.length (objectIdBytes (discoveryRefObjectId discoveryRef)) == hashSize objectFormat
        then Right ()
        else
            Left
                ( "malformed Smart HTTP ref line: object id does not match object-format "
                    ++ BS8.unpack (hashAlgorithmText objectFormat)
                )

hashAlgorithmText :: HashAlgorithm -> ByteString
hashAlgorithmText Sha1 = BS8.pack "sha1"
hashAlgorithmText Sha256 = BS8.pack "sha256"

isCapabilitiesPseudoRef :: DiscoveryRef -> Bool
isCapabilitiesPseudoRef discoveryRef =
    discoveryRefName discoveryRef == BS8.pack "capabilities^{}"
        && isZeroObjectId (discoveryRefObjectId discoveryRef)

parseFirstRef :: ByteString -> Either String (DiscoveryRef, [ByteString])
parseFirstRef rawRefLine = do
    let (refPart, capabilitiesPartWithSeparator) = BS.break (== 0) rawRefLine
        capabilitiesPart =
            if BS.null capabilitiesPartWithSeparator
                then BS.empty
                else BS.tail capabilitiesPartWithSeparator
    discoveryRef <- parseRefLine refPart
    Right (discoveryRef, BS8.words capabilitiesPart)

parseSubsequentRef :: HashAlgorithm -> ByteString -> Either String DiscoveryRef
parseSubsequentRef objectFormat rawRefLine = do
    discoveryRef <- parseRefLine rawRefLine
    validateRefObjectFormat objectFormat discoveryRef
    pure discoveryRef

parseRefLine :: ByteString -> Either String DiscoveryRef
parseRefLine rawRefLine =
    let (objectIdText, refNameWithSpace) = BS8.break (== ' ') rawRefLine
        refName = trimTrailingNewline (BS.tail refNameWithSpace)
     in if BS.null refNameWithSpace
            then Left "malformed Smart HTTP ref line"
            else if BS.null refName
                then Left "malformed Smart HTTP ref line: empty reference name"
                else do
                    objectId <-
                        maybe
                            (Left "malformed Smart HTTP object id")
                            Right
                            (objectIdFromHex (BS8.unpack objectIdText))
                    Right (DiscoveryRef refName objectId)

firstPresent :: [Maybe a] -> Maybe a
firstPresent [] = Nothing
firstPresent (value:rest) =
    case value of
        Just present -> Just present
        Nothing -> firstPresent rest

trimTrailingNewline :: ByteString -> ByteString
trimTrailingNewline = BS8.reverse . BS8.dropWhile (`elem` "\r\n") . BS8.reverse

trimTrailingNewlineString :: String -> String
trimTrailingNewlineString = reverse . dropWhile (`elem` "\r\n") . reverse
