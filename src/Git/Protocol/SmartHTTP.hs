module Git.Protocol.SmartHTTP
    ( DiscoveryAdvertisement
    , DiscoveryRef
    , SmartHttpEndpoint (..)
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
import Git.Hash (ObjectId, objectIdFromHex)
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

defaultSmartHttpEndpoint :: String -> SmartHttpEndpoint
defaultSmartHttpEndpoint baseUrl =
    SmartHttpEndpoint
        { discoveryPath = baseUrl ++ "/info/refs?service=git-upload-pack"
        , uploadPackPath = baseUrl ++ "/git-upload-pack"
        }

parseDiscoveryResponse :: ByteString -> Either String DiscoveryAdvertisement
parseDiscoveryResponse rawResponse = do
    packetLines <- decodePacketLines rawResponse
    payloads <- traverse requirePayload packetLines
    case payloads of
        serviceLine : refLines ->
            if serviceLine /= BS8.pack "# service=git-upload-pack\n"
                then Left "Unexpected Smart HTTP service line"
                else parseRefs refLines
        [] -> Left "Empty Smart HTTP discovery response"
  where
    requirePayload packet =
        case packetLinePayload packet of
            Just payload -> Right payload
            Nothing -> Right BS.empty

    parseRefs refLines =
        let nonEmptyRefLines = filter (not . BS.null) refLines
         in case nonEmptyRefLines of
                [] -> Right (DiscoveryAdvertisement [] [])
                firstRef : remainingRefs -> do
                    (firstDiscoveryRef, capabilities) <- parseFirstRef firstRef
                    otherRefs <- traverse parseSubsequentRef remainingRefs
                    Right (DiscoveryAdvertisement capabilities (firstDiscoveryRef : otherRefs))

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
     in if BS.null refNameWithSpace
            then Left "Malformed Smart HTTP ref line"
            else do
                objectId <-
                    maybe
                        (Left "Malformed Smart HTTP object id")
                        Right
                        (objectIdFromHex (BS8.unpack objectIdText))
                Right (DiscoveryRef (trimTrailingNewline (BS.tail refNameWithSpace)) objectId)

trimTrailingNewline :: ByteString -> ByteString
trimTrailingNewline = BS8.reverse . BS8.dropWhile (`elem` "\r\n") . BS8.reverse
