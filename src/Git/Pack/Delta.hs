module Git.Pack.Delta
    ( DeltaInstruction (..)
    , applyDelta
    , parseDelta
    ) where

import Data.Bits ((.&.), shiftL, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

data DeltaInstruction
    = CopyChunk Int Int
    | InsertChunk ByteString
    deriving (Eq, Show)

parseDelta :: ByteString -> Either String (Int, Int, [DeltaInstruction])
parseDelta rawDelta = do
    (baseSize, afterBaseSize) <- parseVarInt rawDelta
    (resultSize, instructionBytes) <- parseVarInt afterBaseSize
    instructions <- parseInstructions instructionBytes
    Right (baseSize, resultSize, instructions)

applyDelta :: Int -> Int -> ByteString -> [DeltaInstruction] -> Either String ByteString
applyDelta expectedBaseSize expectedResultSize baseBytes instructions
    | BS.length baseBytes /= expectedBaseSize = Left "Delta base size does not match the supplied base object"
    | otherwise = do
        result <- BS.concat <$> traverse applyInstruction instructions
        if BS.length result /= expectedResultSize
            then Left "Delta result size does not match the declared output size"
            else Right result
  where
    applyInstruction (InsertChunk payload) = Right payload
    applyInstruction (CopyChunk offset size)
        | offset < 0 || size < 0 = Left "Invalid delta copy instruction"
        | offset + size > BS.length baseBytes = Left "Delta copy instruction exceeds base object"
        | otherwise = Right (BS.take size (BS.drop offset baseBytes))

parseInstructions :: ByteString -> Either String [DeltaInstruction]
parseInstructions rawBytes
    | BS.null rawBytes = Right []
    | otherwise =
        case BS.uncons rawBytes of
            Nothing -> Right []
            Just (opcode, remaining)
                | opcode == 0 -> Left "Invalid delta opcode"
                | testBit opcode 7 -> do
                    (instruction, rest) <- parseCopyInstruction opcode remaining
                    (instruction :) <$> parseInstructions rest
                | otherwise ->
                    let insertLength = fromIntegral opcode
                     in if BS.length remaining < insertLength
                            then Left "Truncated delta insert instruction"
                            else
                                let (payload, rest) = BS.splitAt insertLength remaining
                                 in (InsertChunk payload :) <$> parseInstructions rest

parseCopyInstruction :: Word8 -> ByteString -> Either String (DeltaInstruction, ByteString)
parseCopyInstruction opcode rawBytes = do
    (offset, afterOffset) <- readSelectedBytes [0, 1, 2, 3] rawBytes
    (size, remaining) <- readSelectedBytes [4, 5, 6] afterOffset
    let normalizedSize = if size == 0 then 0x10000 else size
    Right (CopyChunk offset normalizedSize, remaining)
  where
    readSelectedBytes bitPositions bytes =
        go bitPositions bytes 0 0

    go [] bytes _ acc = Right (acc, bytes)
    go (bitPosition:rest) bytes shift acc
        | testBit opcode bitPosition =
            case BS.uncons bytes of
                Nothing -> Left "Truncated delta copy instruction"
                Just (nextByte, remaining) ->
                    go rest remaining (shift + 8) (acc + (fromIntegral nextByte `shiftL` shift))
        | otherwise = go rest bytes (shift + 8) acc

parseVarInt :: ByteString -> Either String (Int, ByteString)
parseVarInt = go 0 0
  where
    go acc shift rawBytes =
        case BS.uncons rawBytes of
            Nothing -> Left "Truncated delta header"
            Just (currentByte, remaining) ->
                let nextAcc = acc + (fromIntegral (currentByte .&. 0x7f) `shiftL` shift)
                 in if testBit currentByte 7
                        then go nextAcc (shift + 7) remaining
                        else Right (nextAcc, remaining)
