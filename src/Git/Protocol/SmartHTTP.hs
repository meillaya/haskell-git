module Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement
    , DiscoveryRef
    , SmartHttpEndpoint (..)
    , capabilityValue
    , defaultSmartHttpEndpoint
    , discoveryCapabilities
    , discoveryRefName
    , discoveryRefObjectId
    , discoveryRefs
    , parseDiscoveryResponse
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Hash (ObjectId, isZeroObjectId, objectIdFromHex)
import Git.Protocol.PacketLine (decodePacketLines, packetLinePayload)

data SmartHttpEndpoint = SmartHttpEndpoint
    { discoveryPath :: String
    , uploadPackPath :: String
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
    }
    deriving (Eq, Show)

-- | Derive the two Smart HTTP endpoints. A trailing slash on the remote URL
-- must be dropped; git refuses to serve a double slash.
defaultSmartHttpEndpoint :: String -> SmartHttpEndpoint
defaultSmartHttpEndpoint baseUrl =
    SmartHttpEndpoint
        { discoveryPath = trimmedBaseUrl ++ "/info/refs?service=git-upload-pack"
        , uploadPackPath = trimmedBaseUrl ++ "/git-upload-pack"
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
parseDiscoveryResponse rawResponse = do
    packetLines <- decodePacketLines rawResponse
    let payloads = [payload | Just payload <- map packetLinePayload packetLines]
    case [payload | payload <- payloads, BS8.isPrefixOf (BS8.pack "ERR ") payload] of
        errorLine : _ ->
            Left ("remote error: " ++ trimTrailingNewlineString (BS8.unpack (BS.drop 4 errorLine)))
        [] -> pure ()
    case payloads of
        serviceLine : refLines
            | serviceLine == BS8.pack "# service=git-upload-pack\n" -> parseRefs refLines
            | otherwise -> Left "unexpected Smart HTTP service line"
        [] -> Left "empty Smart HTTP discovery response"

-- | The advertisement lists each ref once, plus a peeled line for annotated
-- tags and, for an empty repository, a pseudo line with a zero id.
parseRefs :: [ByteString] -> Either String DiscoveryAdvertisement
parseRefs refLines =
    case filter (not . BS.null) refLines of
        [] -> Right (DiscoveryAdvertisement [] [])
        firstRef : remainingRefs -> do
            (firstDiscoveryRef, capabilities) <- parseFirstRef firstRef
            if isCapabilitiesPseudoRef firstDiscoveryRef
                then Right (DiscoveryAdvertisement capabilities [])
                else do
                    otherRefs <- traverse parseSubsequentRef remainingRefs
                    Right (DiscoveryAdvertisement capabilities (firstDiscoveryRef : otherRefs))

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

parseSubsequentRef :: ByteString -> Either String DiscoveryRef
parseSubsequentRef = parseRefLine

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
