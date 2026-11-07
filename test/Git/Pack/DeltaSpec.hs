module Git.Pack.DeltaSpec
    ( run
    ) where

import Data.Bits ((.&.), (.|.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Pack.Delta (DeltaInstruction (..), applyDelta, parseDelta)
import TestSupport (assertEqual)

-- | Check delta parsing and application: the worked example, a multi-byte
-- copy offset, the 64 KiB copy-size rule, and malformed instructions.
run :: IO ()
run = do
    workedExample
    multiByteCopyOffset
    copySizeZeroMeans64K
    rejectsBadInstructions

-- | Parse and apply the documented delta example.
workedExample :: IO ()
workedExample = do
    let deltaBytes =
            BS.pack
                [ 0x0e
                , 0x0c
                , 0x90
                , 0x07
                , 0x03
                , 0x47
                , 0x69
                , 0x74
                , 0x91
                , 0x0c
                , 0x02
                ]
        baseBytes = BS8.pack "Hello, world!\n"
    (baseSize, resultSize, instructions) <- either fail pure (parseDelta deltaBytes)
    assertEqual 14 baseSize "Delta base size should decode"
    assertEqual 12 resultSize "Delta result size should decode"
    assertEqual
        [CopyChunk 0 7, InsertChunk (BS8.pack "Git"), CopyChunk 12 2]
        instructions
        "Delta instructions should decode"
    result <- either fail pure (applyDelta baseSize resultSize baseBytes instructions)
    assertEqual (BS8.pack "Hello, Git!\n") result "Applying the delta should reconstruct the target bytes"

-- | Decode a copy whose offset needs two bytes.
multiByteCopyOffset :: IO ()
multiByteCopyOffset = do
    let baseBytes = BS.replicate 300 120 <> BS8.pack "tail"
        deltaBytes = encodeVarInt (BS.length baseBytes) <> encodeVarInt 20 <> BS.pack [0x93, 0x04, 0x01, 20]
    (baseSize, resultSize, instructions) <- either fail pure (parseDelta deltaBytes)
    assertEqual (BS.length baseBytes) baseSize "The base size should decode"
    assertEqual 20 resultSize "The result size should decode"
    assertEqual [CopyChunk 260 20] instructions "A copy with a two-byte offset should decode"
    result <- either fail pure (applyDelta baseSize resultSize baseBytes instructions)
    assertEqual (BS.take 20 (BS.drop 260 baseBytes)) result "The copy should read from the decoded offset"

-- | Check that a stored copy size of zero means 65536.
copySizeZeroMeans64K :: IO ()
copySizeZeroMeans64K = do
    let baseBytes = BS.replicate 70000 97
        deltaBytes = encodeVarInt (BS.length baseBytes) <> encodeVarInt 65536 <> BS.singleton 0x80
    (baseSize, resultSize, instructions) <- either fail pure (parseDelta deltaBytes)
    assertEqual [CopyChunk 0 0x10000] instructions "A stored size of zero should mean 65536"
    result <- either fail pure (applyDelta baseSize resultSize baseBytes instructions)
    assertEqual (BS.take 65536 baseBytes) result "The 64 KiB copy should produce the first 65536 bytes"

-- | Feed the parser bad opcodes and mismatched sizes.
rejectsBadInstructions :: IO ()
rejectsBadInstructions = do
    case parseDelta (BS.pack [0x01, 0x01, 0x00]) of
        Left _ -> pure ()
        Right _ -> fail "A zero opcode should be rejected"
    case parseDelta (BS.pack [0x05, 0x02, 0x05, 0x01]) of
        Left _ -> pure ()
        Right _ -> fail "A truncated insert should be rejected"
    assertEqual
        (Left "Delta base size does not match the supplied base object")
        (applyDelta 4 2 BS.empty [InsertChunk (BS8.pack "ab")])
        "A base size mismatch should be rejected"
    assertEqual
        (Left "Delta result size does not match the declared output size")
        (applyDelta 4 3 (BS8.pack "abcd") [InsertChunk (BS8.pack "ab")])
        "A result size mismatch should be rejected"

encodeVarInt :: Int -> BS.ByteString
encodeVarInt value
    | value < 0x80 = BS.pack [fromIntegral value]
    | otherwise =
        BS.singleton (fromIntegral ((value .&. 0x7f) .|. 0x80))
            <> encodeVarInt (value `shiftR` 7)
