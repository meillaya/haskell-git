module Git.Sha256Spec
    ( run
    ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort)
import Git.Clone (CloneResult (..), cloneRepository)
import Git.Fetch (defaultFetchOptions, fetchRepository)
import Git.Push (defaultPushOptions, pushRepository)
import Git.Hash
    ( HashAlgorithm (..)
    , ObjectId
    , hashBytesFor
    , objectIdFromHex
    , objectIdFromHexFor
    , objectIdToHex
    )
import Git.Index (IndexEntry (..), buildTreeFromIndex, parseIndexFor, writeIndexFor)
import Git.Object.Blob (blobFromBytes, blobToObject)
import Git.Object.Commit
    ( commit
    , commitFromObjectFor
    , commitToObject
    , commitTree
    , personIdent
    )
import Git.Object.Loose (writeLooseObject)
import Git.Object.Tree
    ( TreeEntry (..)
    , TreeEntryMode (..)
    , renderTreeEntry
    , treeEntryName
    )
import Git.Protocol.PacketLine (encodePacketLine, flushPacket, packetLine)
import Git.Protocol.SmartHTTP
    ( discoveryObjectFormat
    , discoveryRefName
    , discoveryRefObjectId
    , discoveryRefs
    , parseDiscoveryResponse
    )
import Git.Refs (updateHeadReference)
import Git.Repo
    ( Repository
    , findRepository
    , initRepositoryWithFormat
    , repositoryConfigPath
    , repositoryIndexPath
    , repositoryObjectFormat
    , repositoryRootDir
    )
import Git.Store (flattenTree, loadStore, readObjectOrFail)
import Git.Verify (VerifyReport (..), verifyRepository)
import System.Directory (createDirectoryIfMissing, getPermissions, getSymbolicLinkTarget, Permissions (..))
import System.FilePath ((</>))
import TestSupport
    ( assert
    , assertContains
    , assertEqual
    , runBash
    , runGit
    , withHttpBackendServer
    , withTempDirectory
    )

-- | SHA-256 coverage: hash vectors, repository initialization, the index and
-- pack codecs, Smart HTTP advertisement parsing, and read/write/clone paths.
run :: IO ()
run = do
    hashVectors
    initSha256Repository
    indexRoundTrip
    smartHttpSha256
    readsSha256PackedRepository
    writesSha256Repository
    clonesSha256Repository
    fetchesAndPushesSha256
    clonesEmptySha256Repository

hashVectors :: IO ()
hashVectors = do
    assertEqual
        "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        (objectIdToHex (hashBytesFor Sha1 BS.empty))
        "SHA-1 of the empty string"
    assertEqual
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        (objectIdToHex (hashBytesFor Sha256 BS.empty))
        "SHA-256 of the empty string"
    assertEqual
        "a9993e364706816aba3e25717850c26c9cd0d89d"
        (objectIdToHex (hashBytesFor Sha1 (BS8.pack "abc")))
        "SHA-1 of abc"
    assertEqual
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        (objectIdToHex (hashBytesFor Sha256 (BS8.pack "abc")))
        "SHA-256 of abc"
    let sha256Hex = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    assertEqual
        (Just sha256Hex)
        (fmap objectIdToHex (objectIdFromHexFor Sha256 sha256Hex))
        "A SHA-256 id should round-trip through hex"
    assertEqual
        (Just sha256Hex)
        (fmap objectIdToHex (objectIdFromHex sha256Hex))
        "The auto-detecting hex parser should accept 64 hex digits"
    assertEqual
        Nothing
        (objectIdFromHexFor Sha1 sha256Hex)
        "A 64-hex id must not parse as SHA-1"
    assertEqual Nothing (objectIdFromHex "abc") "A short hex string is not an object id"

initSha256Repository :: IO ()
initSha256Repository =
    withTempDirectory "haskell-git-sha256-init" $ \temporaryDir -> do
        repository <- initRepositoryWithFormat Sha256 "main" temporaryDir
        assertEqual Sha256 (repositoryObjectFormat repository) "init should record SHA-256"
        configText <- readFile (repositoryConfigPath repository)
        assertContains "repositoryformatversion = 1" configText "SHA-256 needs repository format 1"
        assertContains "objectformat = sha256" configText "The extension should be written"
        _ <- runGit ["-C", temporaryDir, "rev-parse", "--git-dir"]
        configured <- runGit ["-C", temporaryDir, "config", "--get", "extensions.objectformat"]
        assertEqual "sha256\n" configured "git should read the extension"
        found <- findRepository temporaryDir
        case found of
            Nothing -> fail "findRepository should see the new repository"
            Just discovered ->
                assertEqual Sha256 (repositoryObjectFormat discovered) "findRepository should read the format"
        reinitialized <-
            try (initRepositoryWithFormat Sha1 "main" temporaryDir) ::
                IO (Either IOException Repository)
        case reinitialized of
            Left _ -> pure ()
            Right _ -> fail "reinitializing SHA-256 as SHA-1 should fail"

indexRoundTrip :: IO ()
indexRoundTrip =
    withTempDirectory "haskell-git-sha256-index" $ \temporaryDir -> do
        repository <- initRepositoryWithFormat Sha256 "main" temporaryDir
        payload <- pure (BS8.pack "hello sha256\n")
        createDirectoryIfMissing True temporaryDir
        BS.writeFile (temporaryDir </> "hello.txt") payload
        blobId <- writeLooseObject repository (blobToObject (blobFromBytes payload))
        assertEqual 64 (length (objectIdToHex blobId)) "A SHA-256 blob id should be 64 hex digits"
        let entry =
                IndexEntry
                    { indexEntryPath = BS8.pack "hello.txt"
                    , indexEntryMode = RegularFile
                    , indexEntrySize = fromIntegral (BS.length payload)
                    , indexEntryObjectId = blobId
                    , indexEntryStage = 0
                    }
            encoded = writeIndexFor Sha256 [entry]
        BS.writeFile (repositoryIndexPath repository) encoded
        decoded <- either fail pure (parseIndexFor Sha256 encoded)
        assertEqual [entry] decoded "The SHA-256 index should round-trip"
        listing <- runGit ["-C", temporaryDir, "ls-files", "-s"]
        assertContains (objectIdToHex blobId) listing "git should read the SHA-256 index"

smartHttpSha256 :: IO ()
smartHttpSha256 = do
    let sha256Hex = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        response =
            BS8.concat
                [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
                , encodePacketLine flushPacket
                , encodePacketLine
                    ( packetLine
                        ( BS8.pack
                            ( sha256Hex
                                ++ " HEAD\0multi_ack ofs-delta symref=HEAD:refs/heads/main object-format=sha256\n"
                            )
                        )
                    )
                , encodePacketLine (packetLine (BS8.pack (sha256Hex ++ " refs/heads/main\n")))
                , encodePacketLine flushPacket
                ]
    advertisement <- either fail pure (parseDiscoveryResponse response)
    assertEqual Sha256 (discoveryObjectFormat advertisement) "The advertisement should be SHA-256"
    assertEqual
        ["HEAD", "refs/heads/main"]
        (map (BS8.unpack . discoveryRefName) (discoveryRefs advertisement))
        "Both advertised refs should parse"
    forM_ (discoveryRefs advertisement) $ \advertisedRef ->
        assertEqual
            64
            (length (objectIdToHex (discoveryRefObjectId advertisedRef)))
            "SHA-256 refs should keep all 64 hex digits"

    let emptyHex = BS8.pack (replicate 64 '0')
        emptyResponse =
            BS8.concat
                [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
                , encodePacketLine flushPacket
                , encodePacketLine
                    (packetLine (emptyHex <> BS8.pack " capabilities^{}\0object-format=sha256\n"))
                , encodePacketLine flushPacket
                ]
    emptyAdvertisement <- either fail pure (parseDiscoveryResponse emptyResponse)
    assertEqual [] (discoveryRefs emptyAdvertisement) "An empty repository advertises no refs"
    assertEqual Sha256 (discoveryObjectFormat emptyAdvertisement) "The empty advertisement keeps the format"

    let unsupportedResponse =
            BS8.concat
                [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
                , encodePacketLine flushPacket
                , encodePacketLine
                    (packetLine (BS8.pack (sha256Hex ++ " HEAD\0object-format=sha512\n")))
                , encodePacketLine flushPacket
                ]
    case parseDiscoveryResponse unsupportedResponse of
        Left message ->
            assertContains "unsupported object format" message "Unknown formats should be named"
        Right _ -> fail "An unsupported object format should not parse"

    let mixedResponse =
            BS8.concat
                [ encodePacketLine (packetLine (BS8.pack "# service=git-upload-pack\n"))
                , encodePacketLine flushPacket
                , encodePacketLine
                    (packetLine (BS8.pack (sha256Hex ++ " HEAD\0object-format=sha256\n")))
                , encodePacketLine
                    (packetLine (BS8.pack ("0123456789012345678901234567890123456789 refs/heads/main\n")))
                , encodePacketLine flushPacket
                ]
    case parseDiscoveryResponse mixedResponse of
        Left message ->
            assertContains "does not match object-format" message "Mixed id lengths should be refused"
        Right _ -> fail "Mixed object id lengths should not parse"

readsSha256PackedRepository :: IO ()
readsSha256PackedRepository =
    withTempDirectory "haskell-git-sha256-packed" $ \temporaryDir -> do
        headObjectId <- createSha256Repository temporaryDir
        _ <- runGit ["-C", temporaryDir, "gc", "-q"]
        repository <- requireRepositoryAt temporaryDir
        report <- verifyRepository repository
        assertEqual [] (verifyProblems report) "verify should accept a packed SHA-256 repository"
        store <- loadStore repository
        headObject <- readObjectOrFail store headObjectId
        currentCommit <- either fail pure (commitFromObjectFor Sha256 headObject)
        flattened <- flattenTree store (commitTree currentCommit)
        gitNames <- lines <$> runGit ["-C", temporaryDir, "ls-tree", "-r", "--name-only", "HEAD"]
        assertEqual
            (sort gitNames)
            (sort (map fst flattened))
            "The flattened SHA-256 tree should match git"
        indexBytes <- BS.readFile (repositoryIndexPath repository)
        entries <- either fail pure (parseIndexFor Sha256 indexBytes)
        assertEqual (length gitNames) (length entries) "The SHA-256 index should list one entry per path"

writesSha256Repository :: IO ()
writesSha256Repository =
    withTempDirectory "haskell-git-sha256-write" $ \temporaryDir -> do
        repository <- initRepositoryWithFormat Sha256 "main" temporaryDir
        let payload = BS8.pack "hello sha256 write\n"
        BS.writeFile (repositoryRootDir repository </> "hello.txt") payload
        blobId <- writeLooseObject repository (blobToObject (blobFromBytes payload))
        let entry =
                IndexEntry
                    { indexEntryPath = BS8.pack "hello.txt"
                    , indexEntryMode = RegularFile
                    , indexEntrySize = fromIntegral (BS.length payload)
                    , indexEntryObjectId = blobId
                    , indexEntryStage = 0
                    }
        BS.writeFile (repositoryIndexPath repository) (writeIndexFor Sha256 [entry])
        treeId <- buildTreeFromIndex repository [entry]
        let author =
                personIdent
                    (BS8.pack "Ada")
                    (BS8.pack "ada@example.com")
                    1713220000
                    (BS8.pack "+0000")
            currentCommit =
                commit treeId [] author author (BS8.pack "First commit\n")
        commitId <- writeLooseObject repository (commitToObject currentCommit)
        updateHeadReference repository commitId
        statusText <- runGit ["-C", temporaryDir, "status", "--porcelain"]
        assertEqual "" statusText "git status should be clean after our SHA-256 write"
        fsckText <- runGit ["-C", temporaryDir, "fsck", "--strict"]
        assertEqual "" fsckText "git fsck should accept our SHA-256 commit"
        blobText <- runGit ["-C", temporaryDir, "cat-file", "-p", objectIdToHex blobId]
        assertEqual "hello sha256 write\n" blobText "git should read our SHA-256 blob"
        logText <- runGit ["-C", temporaryDir, "log", "--format=%H"]
        assertEqual (objectIdToHex commitId) (trim logText) "git log should show our SHA-256 commit"
        report <- verifyRepository repository
        assertEqual [] (verifyProblems report) "verify should accept the repository we wrote"

clonesSha256Repository :: IO ()
clonesSha256Repository =
    withTempDirectory "haskell-git-sha256-clone" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            destination = temporaryDir </> "clone"
        headObjectId <- createSha256Repository sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, serveDirectory </> "repo.git"]
        withHttpBackendServer serveDirectory "repo.git" $ \remoteUrl -> do
            result <- cloneRepository remoteUrl destination
            assertEqual (Just headObjectId) (cloneHead result) "Clone should report the SHA-256 HEAD"
            assertEqual "refs/heads/main" (cloneLocalBranch result) "Clone should report the branch"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "git status should be clean after a SHA-256 clone"
            fsckText <- runGit ["-C", destination, "fsck", "--strict"]
            assertEqual "" fsckText "git fsck should accept the SHA-256 clone"
            branchList <- runGit ["-C", destination, "branch", "-a"]
            assertContains "origin/feature" branchList "Clone should fetch other SHA-256 branches"
            tagList <- runGit ["-C", destination, "tag"]
            assert ("v1" `elem` lines tagList) "Clone should fetch SHA-256 tags"
            contents <- readFile (destination </> "README.md")
            assertEqual "hello sha256\n" contents "Clone should write SHA-256 file contents"
            permissions <- getPermissions (destination </> "run.sh")
            assert (executable permissions) "Clone should restore the executable bit"
            symbolicTarget <- getSymbolicLinkTarget (destination </> "link.md")
            assertEqual "README.md" symbolicTarget "Clone should restore symlinks"
            gitListing <- runGit ["-C", destination, "ls-tree", "-r", "HEAD"]
            ourListing <- ourTreeListing destination
            assertEqual
                (sort (filter (not . null) (lines gitListing)))
                (sort ourListing)
                "Clone should produce the same tree as git ls-tree"

fetchesAndPushesSha256 :: IO ()
fetchesAndPushesSha256 =
    withTempDirectory "haskell-git-sha256-transport" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- createSha256Repository sourceDirectory
        _ <- runGit ["-C", sourceDirectory, "config", "user.email", "ada@example.com"]
        _ <- runGit ["-C", sourceDirectory, "config", "user.name", "Ada"]
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        _ <- runGit ["-C", bareRepository, "config", "http.receivepack", "true"]
        withHttpBackendServer serveDirectory "repo.git" $ \remoteUrl -> do
            _ <- cloneRepository remoteUrl destination
            _ <- runGit ["-C", destination, "config", "user.email", "ada@example.com"]
            _ <- runGit ["-C", destination, "config", "user.name", "Ada"]
            writeFile (destination </> "pushed.txt") "pushed sha256\n"
            _ <- runGit ["-C", destination, "add", "-A"]
            _ <- runGit ["-C", destination, "commit", "-qm", "push sha256"]
            pushedHead <- trim <$> runGit ["-C", destination, "rev-parse", "HEAD"]
            repository <- requireRepositoryAt destination
            _ <- pushRepository repository "origin" ["refs/heads/main:refs/heads/main"] defaultPushOptions
            remoteHead <- trim <$> runGit ["-C", bareRepository, "rev-parse", "refs/heads/main"]
            assertEqual pushedHead remoteHead "SHA-256 push should update the remote branch"
            writeFile (sourceDirectory </> "fetched.txt") "fetched sha256\n"
            _ <- runGit ["-C", sourceDirectory, "add", "-A"]
            _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "fetch sha256"]
            _ <- runGit ["-C", sourceDirectory, "push", "-q", "-f", bareRepository, "main"]
            _ <- fetchRepository repository "origin" [] defaultFetchOptions
            fetchedHead <- trim <$> runGit ["-C", destination, "rev-parse", "refs/remotes/origin/main"]
            sourceHead <- trim <$> runGit ["-C", sourceDirectory, "rev-parse", "HEAD"]
            assertEqual sourceHead fetchedHead "SHA-256 fetch should update the remote-tracking ref"
            fsckText <- runGit ["-C", destination, "fsck", "--strict"]
            assertEqual "" fsckText "SHA-256 fetch/push should leave a valid object store"

clonesEmptySha256Repository :: IO ()
clonesEmptySha256Repository =
    withTempDirectory "haskell-git-sha256-empty" $ \temporaryDir -> do
        let serveDirectory = temporaryDir </> "serve"
            destination = temporaryDir </> "clone"
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["init", "-q", "--bare", "--object-format=sha256", serveDirectory </> "empty.git"]
        withHttpBackendServer serveDirectory "empty.git" $ \remoteUrl -> do
            result <- cloneRepository remoteUrl destination
            assertEqual Nothing (cloneHead result) "An empty SHA-256 remote has no HEAD commit"
            headText <- runGit ["-C", destination, "symbolic-ref", "HEAD"]
            assertEqual "refs/heads/main\n" headText "An empty SHA-256 clone keeps a default branch"
            configured <- runGit ["-C", destination, "config", "--get", "extensions.objectformat"]
            assertEqual "sha256\n" configured "The empty clone should record SHA-256"
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "An empty SHA-256 clone should be clean"

-- | Create a SHA-256 source repository with modes, a symlink, a branch, and a
-- tag; return its HEAD id.
createSha256Repository :: FilePath -> IO ObjectId
createSha256Repository directory = do
    _ <- runGit ["init", "-q", "-b", "main", "--object-format=sha256", directory]
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    writeFile (directory </> "README.md") "hello sha256\n"
    createDirectoryIfMissing True (directory </> "docs")
    writeFile (directory </> "docs" </> "nested.txt") "nested sha256\n"
    writeFile (directory </> "run.sh") "#!/bin/sh\necho sha256\n"
    _ <- runBash ("chmod +x " ++ directory </> "run.sh")
    _ <- runBash ("ln -s README.md " ++ directory </> "link.md")
    _ <- runGit ["-C", directory, "add", "-A"]
    _ <- runGit ["-C", directory, "commit", "-qm", "initial"]
    _ <- runGit ["-C", directory, "branch", "feature"]
    _ <- runGit ["-C", directory, "tag", "-a", "v1", "-m", "release"]
    headObjectIdOf directory

requireRepositoryAt :: FilePath -> IO Repository
requireRepositoryAt directory = do
    found <- findRepository directory
    maybe (fail ("not a repository: " ++ directory)) pure found

headObjectIdOf :: FilePath -> IO ObjectId
headObjectIdOf directory = do
    headText <- trim <$> runGit ["-C", directory, "rev-parse", "HEAD"]
    maybe (fail "git returned an invalid SHA-256 head id") pure (objectIdFromHex headText)

ourTreeListing :: FilePath -> IO [String]
ourTreeListing destination = do
    repository <- requireRepositoryAt destination
    store <- loadStore repository
    headId <- headObjectIdOf destination
    headObject <- readObjectOrFail store headId
    currentCommit <- either fail pure (commitFromObjectFor Sha256 headObject)
    flattened <- flattenTree store (commitTree currentCommit)
    pure
        [ BS8.unpack (renderTreeEntry entry {treeEntryName = BS8.pack path})
        | (path, entry) <- flattened
        ]

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
