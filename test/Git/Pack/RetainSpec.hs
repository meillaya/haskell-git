module Git.Pack.RetainSpec
    ( run
    ) where

import Data.Char (isHexDigit)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf)
import Git.Clone (CloneResult (..), cloneRepository)
import Git.Fetch
    ( FetchResult (..)
    , defaultFetchOptions
    , fetchRepository
    )
import Git.Hash
    ( HashAlgorithm (..)
    , ObjectId
    , objectIdFromHexFor
    , objectIdToHex
    )
import Git.Object (GitObjectType (..), gitObjectType)
import Git.Pack (packHeaderObjectCount)
import Git.Pack.Read (PackRawObject (..))
import Git.Pack.Retain (retainPackDirectly)
import Git.Pack.Stream (readPackHeaderFromFile, readPackObjectAtHandle)
import Git.Repo (Repository, findRepository, repositoryObjectsDir)
import Git.Store (loadStore, readObjectOrFail)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , listDirectory
    )
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), withBinaryFile)
import TestSupport
    ( assert
    , assertEqual
    , runBash
    , runGit
    , withHttpBackendServerV2
    , withTempDirectory
    )

-- | Focused checks for direct pack retention, especially completing thin
-- packs by appending missing delta bases. Each scenario builds a real pack with
-- git and then retains it with the library:
--
--   * a self-contained pack is retained as-is,
--   * a hand-built thin pack (external delta base) is completed in place,
--   * a clone followed by a fetch whose v2 pack is thin succeeds through the
--     wired clone/fetch path with no loose objects left behind,
--   * repeated fetches whose thin packs each append several external bases
--     keep working (the appended-entry offsets stay correct),
--   * the thin completion works for SHA-256 repositories too.
run :: IO ()
run = do
    retainsSelfContainedPackDirectly
    completesThinPackDirectly
    completesThinPackDuringFetch
    completesMultiBaseThinPackAcrossFetches
    completesThinPackSha256

-- | A cloned repository whose HTTP server stays up while the scenario runs, so
-- a later fetch can see new commits pushed to the served bare repository.
data Fixture = Fixture
    { fixtureObjectFormat :: HashAlgorithm
    , fixtureSourceDir :: FilePath
    , fixtureBareDir :: FilePath
    , fixtureCloneDir :: FilePath
    , fixtureOldHead :: ObjectId
    }

withClonedFixture :: HashAlgorithm -> String -> (Fixture -> FilePath -> IO a) -> IO a
withClonedFixture objectFormat label action =
    withTempDirectory ("haskell-git-retain-" ++ label) $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
        _ <- runGit (initArguments objectFormat sourceDirectory)
        configureUser sourceDirectory
        writeFile (sourceDirectory </> bigFileName) (bigFileContents 0)
        _ <- runGit ["-C", sourceDirectory, "add", "-A"]
        _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "first"]
        oldHead <- requireHead objectFormat sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withHttpBackendServerV2 serveDirectory "repo.git" $ \remoteUrl -> do
            result <- cloneRepository remoteUrl destination
            assertEqual (Just oldHead) (cloneHead result) "clone should report the source HEAD"
            repository <- requireRepositoryAt destination
            assertNoLooseObjects repository "clone"
            action
                Fixture
                    { fixtureObjectFormat = objectFormat
                    , fixtureSourceDir = sourceDirectory
                    , fixtureBareDir = bareRepository
                    , fixtureCloneDir = destination
                    , fixtureOldHead = oldHead
                    }
                temporaryDir

retainsSelfContainedPackDirectly :: IO ()
retainsSelfContainedPackDirectly = do
    withTempDirectory "haskell-git-retain-thick" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            packPath = temporaryDir </> "full.pack"
            scratchDirectory = temporaryDir </> "scratch"
        _ <- runGit ["init", "-q", "-b", "main", sourceDirectory]
        configureUser sourceDirectory
        writeFile (sourceDirectory </> bigFileName) (bigFileContents 0)
        _ <- runGit ["-C", sourceDirectory, "add", "-A"]
        _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "first"]
        headObjectId <- requireHead Sha1 sourceDirectory
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        _ <- runBash (generateFullPackCommand bareRepository packPath)
        header <- readPackHeaderFromFile packPath
        let objectCount = packHeaderObjectCount header
        _ <- runGit ["init", "-q", "-b", "main", scratchDirectory]
        repository <- requireRepositoryAt scratchDirectory
        objectReports <- newIORef []
        deltaReports <- newIORef []
        retained <-
            retainPackDirectly
                Sha1
                repository
                packPath
                (recordProgress objectReports)
                (recordProgress deltaReports)
        assert retained "a self-contained pack should be retained directly"
        assertNoLooseObjects repository "self-contained retention"
        assertPackDirectoryIsClean repository "self-contained retention"
        names <- packDirectoryNames repository
        assertEqual
            1
            (length (filter (".pack" `isSuffixOf`) names))
            "self-contained retention should install exactly one pack"
        objectLog <- reverse <$> readIORef objectReports
        assertEqual
            (Just (objectCount, objectCount))
            (lastProgress objectLog)
            "object progress should finish at the pack's object count"
        deltaLog <- reverse <$> readIORef deltaReports
        assertEqual
            [(0, 0)]
            deltaLog
            "delta progress should be reported exactly once"
        store <- loadStore repository
        headObject <- readObjectOrFail store headObjectId
        assertEqual
            CommitObject
            (gitObjectType headObject)
            "the retained pack should contain the source HEAD commit"
        _ <- runGit ["-C", scratchDirectory, "fsck", "--strict", "--no-dangling"]
        pure ()

completesThinPackDirectly :: IO ()
completesThinPackDirectly = do
    withClonedFixture Sha1 "thin-direct" $ \fixture temporaryDir -> do
        let thinPackPath = temporaryDir </> "thin.pack"
            objectFormat = fixtureObjectFormat fixture
        firstNewHead <- advanceUpstream fixture 1
        secondNewHead <- advanceUpstream fixture 2
        generateThinPack fixture secondNewHead thinPackPath
        header <- readPackHeaderFromFile thinPackPath
        let originalCount = packHeaderObjectCount header
        (refDeltas, offsetDeltas) <- packDeltaCounts objectFormat thinPackPath
        assert
            (refDeltas >= 1)
            "the generated thin pack should delta against an object outside the pack"
        assert
            (offsetDeltas >= 1)
            "the generated thin pack should contain an in-pack delta entry"
        chained <- packChainsThroughDeferredBase objectFormat thinPackPath
        assert
            chained
            "the generated thin pack should have an in-pack delta whose base is a deferred external-base delta"
        repository <- requireRepositoryAt (fixtureCloneDir fixture)
        objectReports <- newIORef []
        deltaReports <- newIORef []
        retained <-
            retainPackDirectly
                objectFormat
                repository
                thinPackPath
                (recordProgress objectReports)
                (recordProgress deltaReports)
        assert retained "a thin pack should be completed and retained directly"
        assertNoLooseObjects repository "thin-pack completion"
        assertPackDirectoryIsClean repository "thin-pack completion"
        names <- packDirectoryNames repository
        assertEqual
            2
            (length (filter (".pack" `isSuffixOf`) names))
            "thin completion should add one completed pack to the clone's pack"
        objectLog <- reverse <$> readIORef objectReports
        case lastProgress objectLog of
            Nothing -> fail "thin completion should report object progress"
            Just (done, total) -> do
                assertEqual total done "object progress should finish at the final total"
                assert
                    (total > originalCount)
                    ( "completing a thin pack should grow the object total (got "
                        ++ show total
                        ++ " for "
                        ++ show originalCount
                        ++ " pack objects)"
                    )
        deltaLog <- reverse <$> readIORef deltaReports
        case deltaLog of
            [(done, total)] -> do
                assertEqual total done "delta progress should report a finished count"
                assert (done >= 1) "the thin pack contains at least one resolved delta"
            _ -> fail ("delta progress should be reported exactly once, got " ++ show deltaLog)
        store <- loadStore repository
        firstNewCommit <- readObjectOrFail store firstNewHead
        assertEqual
            CommitObject
            (gitObjectType firstNewCommit)
            "the first commit behind the thin pack should be readable"
        secondNewCommit <- readObjectOrFail store secondNewHead
        assertEqual
            CommitObject
            (gitObjectType secondNewCommit)
            "the object behind the thin pack's external base chain should be readable"
        objectTypeText <-
            trim <$> runGit ["-C", fixtureCloneDir fixture, "cat-file", "-t", objectIdToHex secondNewHead]
        assertEqual "commit" objectTypeText "git should read the new commit from the completed pack"
        _ <- runGit ["-C", fixtureCloneDir fixture, "fsck", "--strict", "--no-dangling"]
        pure ()

-- | End to end: a clone fetches an incremental commit whose pack the git
-- server makes thin against objects that only exist in the clone's retained
-- pack. The wired fetch path must complete it without falling back to loose
-- imports (which cannot resolve packed bases).
completesThinPackDuringFetch :: IO ()
completesThinPackDuringFetch = do
    withClonedFixture Sha1 "thin-fetch" $ \fixture _temporaryDir -> do
        newHead <- advanceUpstream fixture 1
        repository <- requireRepositoryAt (fixtureCloneDir fixture)
        result <- fetchRepository repository "origin" [] defaultFetchOptions
        assert
            (not (null (fetchUpdatedRefs result)))
            "fetch should report the updated remote-tracking ref"
        remoteHeadText <-
            trim <$> runGit ["-C", fixtureCloneDir fixture, "rev-parse", "refs/remotes/origin/main"]
        assertEqual
            (objectIdToHex newHead)
            remoteHeadText
            "fetch should move the remote-tracking ref to the new commit"
        assertNoLooseObjects repository "fetch of a thin pack"
        assertPackDirectoryIsClean repository "fetch of a thin pack"
        store <- loadStore repository
        newCommit <- readObjectOrFail store newHead
        assertEqual
            CommitObject
            (gitObjectType newCommit)
            "the fetched commit should be readable from the completed pack"
        objectTypeText <-
            trim <$> runGit ["-C", fixtureCloneDir fixture, "cat-file", "-t", objectIdToHex newHead]
        assertEqual "commit" objectTypeText "git should read the fetched commit"
        _ <- runGit ["-C", fixtureCloneDir fixture, "fsck", "--strict"]
        statusText <- runGit ["-C", fixtureCloneDir fixture, "status", "--porcelain"]
        assertEqual "" statusText "fetching a thin pack should leave the worktree clean"
        pure ()

completesThinPackSha256 :: IO ()
completesThinPackSha256 = do
    withClonedFixture Sha256 "thin-sha256" $ \fixture temporaryDir -> do
        let thinPackPath = temporaryDir </> "thin-sha256.pack"
            objectFormat = fixtureObjectFormat fixture
        _firstNewHead <- advanceUpstream fixture 1
        newHead <- advanceUpstream fixture 2
        generateThinPack fixture newHead thinPackPath
        (refDeltas, _offsetDeltas) <- packDeltaCounts objectFormat thinPackPath
        assert
            (refDeltas >= 1)
            "the generated SHA-256 thin pack should delta against an external object"
        repository <- requireRepositoryAt (fixtureCloneDir fixture)
        retained <-
            retainPackDirectly
                objectFormat
                repository
                thinPackPath
                (\_ _ -> pure ())
                (\_ _ -> pure ())
        assert retained "a SHA-256 thin pack should be completed and retained directly"
        assertNoLooseObjects repository "SHA-256 thin-pack completion"
        assertPackDirectoryIsClean repository "SHA-256 thin-pack completion"
        store <- loadStore repository
        newCommit <- readObjectOrFail store newHead
        assertEqual
            CommitObject
            (gitObjectType newCommit)
            "the SHA-256 commit behind the thin pack should be readable"
        _ <- runGit ["-C", fixtureCloneDir fixture, "fsck", "--strict", "--no-dangling"]
        pure ()

-- | Several external bases in one thin pack, fetched twice. The completed pack
-- writes the appended base entries after the original body, so their index
-- offsets must be the real entry offsets rather than consecutive numbers. A
-- single fetch can resolve the deltas from the in-memory cache and hide an
-- offset bug; the second fetch has to read the appended bases back from the
-- completed pack, which is what this scenario exercises.
completesMultiBaseThinPackAcrossFetches :: IO ()
completesMultiBaseThinPackAcrossFetches =
    withTempDirectory "haskell-git-retain-multibase" $ \temporaryDir -> do
        let sourceDirectory = temporaryDir </> "source"
            serveDirectory = temporaryDir </> "serve"
            bareRepository = serveDirectory </> "repo.git"
            destination = temporaryDir </> "clone"
            fileNames = ["file" ++ show index ++ ".txt" | index <- [1 .. 40 :: Int]]
        createDirectoryIfMissing True sourceDirectory
        _ <- runGit ["init", "-q", "-b", "main", sourceDirectory]
        configureUser sourceDirectory
        mapM_ (\name -> writeFile (sourceDirectory </> name) (multibaseContents 0 name)) fileNames
        _ <- runGit ["-C", sourceDirectory, "add", "-A"]
        _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "first"]
        createDirectoryIfMissing True serveDirectory
        _ <- runGit ["clone", "-q", "--bare", sourceDirectory, bareRepository]
        withHttpBackendServerV2 serveDirectory "repo.git" $ \remoteUrl -> do
            _ <- cloneRepository remoteUrl destination
            repository <- requireRepositoryAt destination
            assertNoLooseObjects repository "multi-base clone"
            let rounds = [[1 .. 20], [21 .. 40]]
            mapM_
                ( \indices -> do
                    mapM_
                        (\index ->
                            writeFile
                                (sourceDirectory </> ("file" ++ show index ++ ".txt"))
                                (multibaseContents 1 ("file" ++ show index ++ ".txt")))
                        indices
                    let roundLabel =
                            case indices of
                                firstIndex : _ -> "round " ++ show firstIndex
                                [] -> "round"
                    _ <- runGit ["-C", sourceDirectory, "add", "-A"]
                    _ <- runGit ["-C", sourceDirectory, "commit", "-qm", roundLabel]
                    _ <- runGit ["-C", sourceDirectory, "push", "-q", bareRepository, "main"]
                    result <- fetchRepository repository "origin" [] defaultFetchOptions
                    assert
                        (not (null (fetchUpdatedRefs result)))
                        "each fetch should update the remote-tracking ref"
                    assertNoLooseObjects repository "multi-base fetch"
                    assertPackDirectoryIsClean repository "multi-base fetch"
                    _ <- runGit ["-C", destination, "fsck", "--strict", "--no-dangling"]
                    pure ()
                )
                rounds
            _ <- runGit ["-C", destination, "fsck", "--strict", "--no-dangling"]
            statusText <- runGit ["-C", destination, "status", "--porcelain"]
            assertEqual "" statusText "repeated multi-base fetches should leave the worktree clean"

-- | Deterministic file contents: |seed| selects the mutation round, so a later
-- round produces a delta against the same file's previous content.
multibaseContents :: Int -> String -> String
multibaseContents seed name =
    concat
        [ lineText ++ " " ++ name ++ " seed=" ++ show seed ++ "\n"
        | lineText <- map show [1 .. 1500 :: Int]
        ]

-- | Add a commit upstream that modifies the existing large file, push it to
-- the served bare repository, and return the new head.
advanceUpstream :: Fixture -> Int -> IO ObjectId
advanceUpstream fixture version = do
    let sourceDirectory = fixtureSourceDir fixture
    writeFile (sourceDirectory </> bigFileName) (bigFileContents version)
    _ <- runGit ["-C", sourceDirectory, "add", "-A"]
    _ <- runGit ["-C", sourceDirectory, "commit", "-qm", "commit " ++ show version]
    newHead <- requireHead (fixtureObjectFormat fixture) sourceDirectory
    _ <- runGit ["-C", sourceDirectory, "push", "-q", fixtureBareDir fixture, "main"]
    pure newHead

-- | Build a thin pack containing the objects reachable from the new head but
-- not from the clone's head. Git writes reference deltas against the excluded
-- base objects, which is exactly what upload-pack does for a fetch.
generateThinPack :: Fixture -> ObjectId -> FilePath -> IO ()
generateThinPack fixture newHead destination = do
    let bareRepository = fixtureBareDir fixture
        command =
            "printf '%s\\n--not\\n%s\\n' "
                ++ shellQuote (objectIdToHex newHead)
                ++ " "
                ++ shellQuote (objectIdToHex (fixtureOldHead fixture))
                ++ " | git -C "
                ++ shellQuote bareRepository
                ++ " pack-objects --thin --delta-base-offset --stdout --revs > "
                ++ shellQuote destination
    _ <- runBash command
    pure ()

generateFullPackCommand :: FilePath -> FilePath -> String
generateFullPackCommand bareRepository destination =
    "git -C "
        ++ shellQuote bareRepository
        ++ " rev-list --objects --all | git -C "
        ++ shellQuote bareRepository
        ++ " pack-objects --stdout > "
        ++ shellQuote destination

-- | Count the delta entries in a pack without resolving them. The generated
-- packs use --delta-base-offset, so reference deltas are exactly the entries
-- whose base lives outside the pack.
packDeltaCounts :: HashAlgorithm -> FilePath -> IO (Int, Int)
packDeltaCounts objectFormat path = do
    header <- readPackHeaderFromFile path
    withBinaryFile path ReadMode $ \handle ->
        go handle (packHeaderObjectCount header) 12 0 0
  where
    go _ 0 _ refDeltas offsetDeltas = pure (refDeltas, offsetDeltas)
    go handle remaining offset refDeltas offsetDeltas = do
        (rawObject, consumed) <-
            either (fail . ("could not parse the generated pack: " ++)) pure
                =<< readPackObjectAtHandle objectFormat handle offset
        let (nextRefDeltas, nextOffsetDeltas) =
                case rawObject of
                    PackRawRefDelta _ _ -> (refDeltas + 1, offsetDeltas)
                    PackRawOffsetDelta _ _ -> (refDeltas, offsetDeltas + 1)
                    _ -> (refDeltas, offsetDeltas)
        go handle (remaining - 1) (offset + consumed) nextRefDeltas nextOffsetDeltas

-- | True when some in-pack offset delta uses an earlier reference delta as its
-- base. A thin delta chain rooted at an object outside the pack has exactly
-- this shape, so completion has to resolve an offset delta whose base is itself
-- deferred until the external base has been appended.
packChainsThroughDeferredBase :: HashAlgorithm -> FilePath -> IO Bool
packChainsThroughDeferredBase objectFormat path = do
    header <- readPackHeaderFromFile path
    withBinaryFile path ReadMode $ \handle ->
        go handle (packHeaderObjectCount header) 12 [] False
  where
    go _ 0 _ _ chained = pure chained
    go handle remaining offset referenceBaseOffsets chained = do
        (rawObject, consumed) <-
            either (fail . ("could not parse the generated pack: " ++)) pure
                =<< readPackObjectAtHandle objectFormat handle offset
        let thisIsReference = case rawObject of
                PackRawRefDelta _ _ -> True
                _ -> False
            baseOffset = case rawObject of
                PackRawOffsetDelta baseDistance _ -> Just (offset - baseDistance)
                _ -> Nothing
            baseIsDeferred = case baseOffset of
                Nothing -> False
                Just base -> base `elem` referenceBaseOffsets
            nextReferenceBaseOffsets =
                if thisIsReference then offset : referenceBaseOffsets else referenceBaseOffsets
        go
            handle
            (remaining - 1)
            (offset + consumed)
            nextReferenceBaseOffsets
            (chained || baseIsDeferred)

initArguments :: HashAlgorithm -> FilePath -> [String]
initArguments objectFormat directory =
    ["init", "-q", "-b", "main"]
        ++ (case objectFormat of
                Sha256 -> ["--object-format=sha256"]
                Sha1 -> []
           )
        ++ [directory]

configureUser :: FilePath -> IO ()
configureUser directory = do
    _ <- runGit ["-C", directory, "config", "user.email", "ada@example.com"]
    _ <- runGit ["-C", directory, "config", "user.name", "Ada"]
    pure ()

requireHead :: HashAlgorithm -> FilePath -> IO ObjectId
requireHead objectFormat directory = do
    headText <- trim <$> runGit ["-C", directory, "rev-parse", "HEAD"]
    maybe
        (fail ("git returned an invalid " ++ show objectFormat ++ " head id"))
        pure
        (objectIdFromHexFor objectFormat headText)

requireRepositoryAt :: FilePath -> IO Repository
requireRepositoryAt directory = do
    found <- findRepository directory
    maybe (fail ("not a repository: " ++ directory)) pure found

recordProgress :: IORef [(Int, Int)] -> Int -> Int -> IO ()
recordProgress ref done total = modifyIORef' ref ((done, total) :)

lastProgress :: [(Int, Int)] -> Maybe (Int, Int)
lastProgress reports =
    case reverse reports of
        [] -> Nothing
        latest : _ -> Just latest

assertNoLooseObjects :: Repository -> String -> IO ()
assertNoLooseObjects repository label = do
    count <- looseObjectCount repository
    assertEqual 0 count (label ++ " should leave no loose objects")

looseObjectCount :: Repository -> IO Int
looseObjectCount repository = do
    let objectsDirectory = repositoryObjectsDir repository
    present <- doesDirectoryExist objectsDirectory
    if not present
        then pure 0
        else do
            names <- listDirectory objectsDirectory
            counts <- mapM countPrefix [name | name <- names, isLooseObjectDirectory name]
            pure (sum counts)
  where
    isLooseObjectDirectory name = length name == 2 && all isHexDigit name
    countPrefix prefix = do
        let path = repositoryObjectsDir repository </> prefix
        isDirectory <- doesDirectoryExist path
        if not isDirectory then pure 0 else length <$> listDirectory path

packDirectoryNames :: Repository -> IO [String]
packDirectoryNames repository = do
    let packDirectory = repositoryObjectsDir repository </> "pack"
    present <- doesDirectoryExist packDirectory
    if present then listDirectory packDirectory else pure []

assertPackDirectoryIsClean :: Repository -> String -> IO ()
assertPackDirectoryIsClean repository label = do
    names <- packDirectoryNames repository
    let temporaryNames = filter ("tmp-haskell-git" `isPrefixOf`) names
    assertEqual
        []
        temporaryNames
        (label ++ " should clean up temporary files in the pack directory")
    let packFiles = filter (".pack" `isSuffixOf`) names
        indexFiles = filter (".idx" `isSuffixOf`) names
    assertEqual
        (length packFiles)
        (length indexFiles)
        (label ++ " should pair every pack with an index")

bigFileName :: FilePath
bigFileName = "big.txt"

-- | About 120 KiB of highly compressible, mostly duplicated text: a one-line
-- edit is enough for git to emit a thin reference delta for the new blob.
bigFileContents :: Int -> String
bigFileContents version =
    concat
        [ if index `elem` changedLines
            then "line " ++ show index ++ ": CHANGED the quick brown fox jumps over the lazy dog " ++ show index ++ "\n"
            else "line " ++ show index ++ ": the quick brown fox jumps over the lazy dog " ++ show index ++ "\n"
        | index <- [1 .. 2000 :: Int]
        ]
  where
    changedLines
        | version <= 0 = []
        | version == 1 = [500, 1500]
        | otherwise = [250, 500, 1500, 1750]

shellQuote :: FilePath -> String
shellQuote path = "'" ++ concatMap quoteCharacter path ++ "'"
  where
    quoteCharacter '\'' = "'\\''"
    quoteCharacter character = [character]

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse
