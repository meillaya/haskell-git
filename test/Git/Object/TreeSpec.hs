module Git.Object.TreeSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Hash (objectIdFromHex)
import Git.Object.Tree
    ( decodeTree,
      encodeTree,
      treeEntry,
      TreeEntryMode (RegularFile),
      treeEntryName,
      treeEntryObjectId,
      treeFromObject,
      treeToObject )
import TestSupport (assert, assertEqual)

run :: IO ()
run = do
    let oid =
            maybe
                (error "invalid test object id")
                id
                (objectIdFromHex "ce013625030ba8dba906f756967f9e9ca394464a")
        treeObject = treeToObject [treeEntry RegularFile (BS8.pack "hello.txt") oid]
    decodedEntries <- either fail pure (treeFromObject treeObject)
    let roundTrippedBytes = encodeTree decodedEntries
    decodedAgain <- either fail pure (decodeTree roundTrippedBytes)
    assert (length decodedAgain == 1) "Tree codec should preserve one entry"
    assertEqual (BS8.pack "hello.txt") (treeEntryName (head decodedAgain)) "Tree entry name should round-trip"
    assertEqual oid (treeEntryObjectId (head decodedAgain)) "Tree entry object id should round-trip"
    assertEqual
        (Left "Unsupported tree entry mode")
        (decodeTree (BS8.pack "100755 hello.txt\0"))
        "Tree decoder should reject unsupported modes"
