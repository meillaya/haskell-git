module Main where

import qualified Git.CloneSpec as CloneSpec
import qualified Git.InteropSpec as InteropSpec
import qualified Git.Object.BlobSpec as BlobSpec
import qualified Git.Object.CommitSpec as CommitSpec
import qualified Git.Object.TreeSpec as TreeSpec
import qualified Git.Pack.DeltaSpec as DeltaSpec
import qualified Git.PackSpec as PackSpec
import qualified Git.Protocol.PacketLineSpec as PacketLineSpec
import qualified Git.Protocol.SmartHTTPSpec as SmartHTTPSpec
import qualified Git.RefsSpec as RefsSpec
import qualified Git.RepoSpec as RepoSpec

main :: IO ()
main = do
    RepoSpec.run
    RefsSpec.run
    BlobSpec.run
    TreeSpec.run
    CommitSpec.run
    PacketLineSpec.run
    PackSpec.run
    DeltaSpec.run
    SmartHTTPSpec.run
    InteropSpec.run
    CloneSpec.run
    putStrLn "All scaffold through Step 7 tests passed."
