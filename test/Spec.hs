module Main where

import qualified Git.AuthSpec as AuthSpec
import qualified Git.CloneSpec as CloneSpec
import qualified Git.DiffSpec as DiffSpec
import qualified Git.FetchSpec as FetchSpec
import qualified Git.IndexSpec as IndexSpec
import qualified Git.InteropSpec as InteropSpec
import qualified Git.MergeSpec as MergeSpec
import qualified Git.Object.BlobSpec as BlobSpec
import qualified Git.Object.CommitSpec as CommitSpec
import qualified Git.Object.TagSpec as TagSpec
import qualified Git.Object.TreeSpec as TreeSpec
import qualified Git.Pack.DeltaSpec as DeltaSpec
import qualified Git.Pack.IndexSpec as PackIndexSpec
import qualified Git.Pack.ReadSpec as PackReadSpec
import qualified Git.Pack.StreamingSpec as PackStreamingSpec
import qualified Git.PackSpec as PackSpec
import qualified Git.PropertySpec as PropertySpec
import qualified Git.Protocol.PacketLineSpec as PacketLineSpec
import qualified Git.Protocol.SmartHTTPSpec as SmartHTTPSpec
import qualified Git.ProtocolVersionSpec as ProtocolVersionSpec
import qualified Git.PushSpec as PushSpec
import qualified Git.RebaseSpec as RebaseSpec
import qualified Git.RefsSpec as RefsSpec
import qualified Git.RepoSpec as RepoSpec
import qualified Git.Sha256Spec as Sha256Spec
import qualified Git.StoreSpec as StoreSpec
import qualified Git.SubmoduleSpec as SubmoduleSpec
import qualified Git.TransportSpec as TransportSpec
import qualified Git.VerifySpec as VerifySpec
import qualified Git.WorktreeSpec as WorktreeSpec

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
    PackStreamingSpec.run
    PropertySpec.run
    IndexSpec.run
    StoreSpec.run
    SmartHTTPSpec.run
    VerifySpec.run
    Sha256Spec.run
    InteropSpec.run
    CloneSpec.run
    FetchSpec.run
    AuthSpec.run
    TransportSpec.run
    ProtocolVersionSpec.run
    PushSpec.run
    WorktreeSpec.run
    MergeSpec.run
    RebaseSpec.run
    DiffSpec.run
    SubmoduleSpec.run
    putStrLn "All tests passed."
