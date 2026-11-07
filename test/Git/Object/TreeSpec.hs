module Git.Object.TreeSpec
    ( run
    ) where

import Data.List (sortOn)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Hash (ObjectId, objectIdFromHex)
import Git.Object.Tree
    ( TreeEntry (..)
    , TreeEntryMode (..)
    , decodeTree
    , encodeTree
    , renderTreeEntry
    , treeEntry
    , treeEntryName
    , treeFromObject
    , treeToObject
    )
import TestSupport (assertEqual)

-- | Check the tree codec: every git mode, git's sort order, the ls-tree
-- rendering, and the names and modes git rejects.
run :: IO ()
run = do
    allModesRoundTrip
    gitSortOrder
    renderingNamesTheType
    malformedTreesAreRejected

-- | Round-trip one entry of each mode through the codec.
allModesRoundTrip :: IO ()
allModesRoundTrip = do
    objectId <- requireObjectId "ce013625030ba8dba906f756967f9e9ca394464a"
    let entries =
            [ treeEntry RegularFile (BS8.pack "plain") objectId
            , treeEntry ExecutableFile (BS8.pack "run.sh") objectId
            , treeEntry Symlink (BS8.pack "link") objectId
            , treeEntry Directory (BS8.pack "docs") objectId
            , treeEntry Gitlink (BS8.pack "vendor") objectId
            ]
    decoded <- either fail pure (decodeTree (encodeTree entries))
    assertEqual (length entries) (length decoded) "Tree codec should keep every entry"
    assertEqual
        (map treeEntryName (sortOn treeEntryName entries))
        (map treeEntryName decoded)
        "Tree codec should round-trip entry names"
    decodedAgain <- either fail pure (treeFromObject (treeToObject decoded))
    assertEqual (length entries) (length decodedAgain) "Tree object wrapper should round-trip"

-- | Encode entries and read them back to check git's directory ordering.
gitSortOrder :: IO ()
gitSortOrder = do
    objectId <- requireObjectId "ce013625030ba8dba906f756967f9e9ca394464a"
    let entries =
            [ treeEntry Directory (BS8.pack "foo") objectId
            , treeEntry RegularFile (BS8.pack "foo.txt") objectId
            , treeEntry Directory (BS8.pack "foo0") objectId
            , treeEntry RegularFile (BS8.pack "foo!") objectId
            ]
    decoded <- either fail pure (decodeTree (encodeTree entries))
    assertEqual
        [BS8.pack "foo!", BS8.pack "foo.txt", BS8.pack "foo", BS8.pack "foo0"]
        (map treeEntryName decoded)
        "Tree order should compare a directory as its name plus a slash"

-- | Render one entry per mode in git ls-tree format.
renderingNamesTheType :: IO ()
renderingNamesTheType = do
    objectId <- requireObjectId "ce013625030ba8dba906f756967f9e9ca394464a"
    assertEqual
        "100755 blob ce013625030ba8dba906f756967f9e9ca394464a\trun.sh"
        (BS8.unpack (renderTreeEntry (treeEntry ExecutableFile (BS8.pack "run.sh") objectId)))
        "An executable should render as a blob with mode 100755"
    assertEqual
        "040000 tree ce013625030ba8dba906f756967f9e9ca394464a\tdocs"
        (BS8.unpack (renderTreeEntry (treeEntry Directory (BS8.pack "docs") objectId)))
        "A directory should render with a leading zero and type tree"
    assertEqual
        "160000 commit ce013625030ba8dba906f756967f9e9ca394464a\tvendor"
        (BS8.unpack (renderTreeEntry (treeEntry Gitlink (BS8.pack "vendor") objectId)))
        "A gitlink should render as a commit"

-- | Feed hand-built malformed trees to the decoder and expect a refusal.
malformedTreesAreRejected :: IO ()
malformedTreesAreRejected = do
    assertRejected (BS8.pack "100755 hello.txt\0") "an unknown tree mode should be rejected"
    assertRejected (BS8.pack "100644 \0") "an empty name should be rejected"
    assertRejected (BS8.pack "100644 .\0") "a dot name should be rejected"
    assertRejected (BS8.pack "100644 ..\0") "a double dot name should be rejected"
    assertRejected (BS8.pack "100644 a/b\0") "a slash in a name should be rejected"
    assertRejected (BS8.pack "100644 hello.txt\0") "a truncated object id should be rejected"
    assertRejected
        ( BS8.pack "100644 hello.txt\0"
            <> BS8.replicate 20 'x'
            <> BS8.pack "100644 hello.txt\0"
            <> BS8.replicate 20 'x'
        )
        "a duplicate name should be rejected"

assertRejected :: BS.ByteString -> String -> IO ()
assertRejected rawTree message =
    case decodeTree rawTree of
        Left _ -> pure ()
        Right _ -> fail ("Expected rejection: " ++ message)

requireObjectId :: String -> IO ObjectId
requireObjectId hexText =
    maybe (fail ("invalid test object id: " ++ hexText)) pure (objectIdFromHex hexText)
