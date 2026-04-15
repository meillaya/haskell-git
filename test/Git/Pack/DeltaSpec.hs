module Git.Pack.DeltaSpec
    ( run
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Pack.Delta (DeltaInstruction (CopyChunk, InsertChunk), applyDelta, parseDelta)
import TestSupport (assertEqual)

run :: IO ()
run = do
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
    assertEqual (BS8.pack "Hello, Git!\n") result "Applying delta should reconstruct the target bytes"
    assertEqual
        (Left "Delta base size does not match the supplied base object")
        (applyDelta baseSize resultSize BS.empty instructions)
        "Delta application should reject mismatched base sizes"
    assertEqual
        (Left "Delta result size does not match the declared output size")
        (applyDelta baseSize (resultSize + 1) baseBytes instructions)
        "Delta application should reject mismatched result sizes"
