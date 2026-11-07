module Main where

import qualified Git.CloneSpec as CloneSpec
import qualified Git.IndexSpec as IndexSpec
import qualified Git.InteropSpec as InteropSpec
import qualified Git.Object.BlobSpec as BlobSpec
import qualified Git.Object.CommitSpec as CommitSpec
import qualified Git.Object.TagSpec as TagSpec
import qualified Git.Object.TreeSpec as TreeSpec
import qualified Git.Pack.DeltaSpec as DeltaSpec
import qualified Git.Pack.IndexSpec as PackIndexSpec
import qualified Git.Pack.ReadSpec as PackReadSpec
import qualified Git.PackSpec as PackSpec
import qualified Git.Protocol.PacketLineSpec as PacketLineSpec
import qualified Git.Protocol.SmartHTTPSpec as SmartHTTPSpec
import qualified Git.RefsSpec as RefsSpec
import qualified Git.RepoSpec as RepoSpec
import qualified Git.StoreSpec as StoreSpec
import qualified Git.VerifySpec as VerifySpec

main :: IO ()
main = do
    RepoSpec.run
    RefsSpec.run
    BlobSpec.run
    TreeSpec.run
    CommitSpec.run
    TagSpec.run
    PacketLineSpec.run
    PackSpec.run
    DeltaSpec.run
    PackIndexSpec.run
    PackReadSpec.run
    IndexSpec.run
    StoreSpec.run
    SmartHTTPSpec.run
    VerifySpec.run
    InteropSpec.run
    CloneSpec.run
    putStrLn "All tests passed."
