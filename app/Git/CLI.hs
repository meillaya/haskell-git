module Git.CLI
    ( runCLI
    ) where

import Control.Exception (IOException, catch)
import Control.Monad (foldM, forM, forM_, when)
import Data.Char (ord)
import Data.Bits ((.&.), (.|.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
    ( TimeZone (..)
    , getCurrentTimeZone
    , minutesToTimeZone
    , utcToZonedTime
    )
import Git.Clone (CloneResult (..), cloneRepository)
import Git.Hash (ObjectId, objectIdToHex)
import Git.Index (IndexEntry (..), buildTreeFromIndex, parseIndex, upsertIndexEntry, writeIndex)
import Git.Object
    ( GitObject
    , GitObjectType (..)
    , gitObjectPayload
    , gitObjectType
    , objectTypeName
    )
import Git.Object.Blob (blobFromBytes, blobToObject)
import Git.Object.Commit
    ( Commit
    , commit
    , commitAuthor
    , commitFromObject
    , commitMessage
    , commitToObject
    , commitTree
    , personIdent
    , personIdentEmail
    , personIdentName
    , personIdentTimestamp
    , personIdentTimezone
    )
import Git.Object.Loose (objectIdForObject, writeLooseObject)
import Git.Object.Person (PersonIdent)
import Git.Object.Tag (tagFromObject, tagObject)
import Git.Object.Tree
    ( TreeEntry (..)
    , TreeEntryMode (..)
    , renderTreeEntry
    , treeFromObject
    )
import Git.Refs (resolveRevision, writeRefValue)
import Git.Repo
    ( Repository
    , initRepositoryWithBranch
    , readConfigValue
    , repositoryIndexPath
    , repositoryRootDir
    , requireRepository
    )
import Git.Store (Store, flattenTree, loadStore, objectExists, readObjectOrFail, walkCommits)
import Git.Verify (VerifyReport (..), renderVerifyReport, verifyRepository)
import System.Directory
    ( Permissions (..)
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , getPermissions
    , getSymbolicLinkTarget
    , listDirectory
    , makeAbsolute
    , pathIsSymbolicLink
    )
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath ((</>), makeRelative)
import System.IO (hPutStrLn, stderr, stdout)
import System.IO.Error (ioeGetErrorString)
import Text.Read (readMaybe)

runCLI :: IO ()
runCLI = do
    arguments <- getArgs
    case parseCommand arguments of
        Left usageMessage -> do
            hPutStrLn stderr usageMessage
            exitWith (ExitFailure 2)
        Right command -> runCommand command `catch` reportFailure

reportFailure :: IOException -> IO ()
reportFailure err = do
    hPutStrLn stderr ("haskell-git: " ++ ioeGetErrorString err)
    exitWith (ExitFailure 1)

data Command
    = InitCommand String FilePath
    | AddCommand [FilePath]
    | HashObjectCommand HashObjectOptions
    | CatFileCommand CatFileMode String
    | LsTreeCommand LsTreeOptions String
    | LogCommand (Maybe Int) String
    | RevParseCommand String
    | WriteTreeCommand
    | CommitTreeCommand String [String] String
    | UpdateRefCommand String String
    | CloneCommand String (Maybe FilePath)
    | VerifyCommand FilePath
    | HelpCommand (Maybe String)
    | VersionCommand

data HashObjectOptions = HashObjectOptions
    { hashObjectWrite :: Bool
    , hashObjectReadStdin :: Bool
    , hashObjectPath :: FilePath
    }

data CatFileMode
    = CatFileType
    | CatFileSize
    | CatFilePretty

data LsTreeOptions = LsTreeOptions
    { lsTreeRecursive :: Bool
    , lsTreeNameOnly :: Bool
    }

parseCommand :: [String] -> Either String Command
parseCommand [] = Right (HelpCommand Nothing)
parseCommand ("-h":_) = Right (HelpCommand Nothing)
parseCommand ("--help":_) = Right (HelpCommand Nothing)
parseCommand ("--version":_) = Right VersionCommand
parseCommand ("help":rest) =
    Right (HelpCommand (case rest of name:_ -> Just name; [] -> Nothing))
parseCommand ("init":rest) = parseInit rest
parseCommand ("add":rest)
    | null rest = Left usageText
    | otherwise = Right (AddCommand rest)
parseCommand ("hash-object":rest) = parseHashObject rest
parseCommand ("cat-file":rest) = parseCatFile rest
parseCommand ("ls-tree":rest) = parseLsTree rest
parseCommand ("log":rest) = parseLog rest
parseCommand ("rev-parse":[revision]) = Right (RevParseCommand revision)
parseCommand ["write-tree"] = Right WriteTreeCommand
parseCommand ("commit-tree":rest) = parseCommitTree rest
parseCommand ("update-ref":[refPath, revision]) =
    Right (UpdateRefCommand refPath revision)
parseCommand ("clone":[remoteUrl]) = Right (CloneCommand remoteUrl Nothing)
parseCommand ("clone":[remoteUrl, destination]) =
    Right (CloneCommand remoteUrl (Just destination))
parseCommand ("verify":[]) = Right (VerifyCommand ".")
parseCommand ("verify":[path]) = Right (VerifyCommand path)
parseCommand (knownCommand:_)
    | knownCommand `elem` knownCommands =
        Left
            ( "haskell-git: "
                ++ knownCommand
                ++ " was called with arguments it does not understand\n\n"
                ++ fromMaybe usageText (commandUsage knownCommand)
            )
parseCommand (unknownCommand:_) =
    Left ("haskell-git: unknown command: " ++ unknownCommand ++ "\n\n" ++ usageText)

parseInit :: [String] -> Either String Command
parseInit = go defaultBranchName Nothing
  where
    go branchName maybePath [] = Right (InitCommand branchName (fromMaybe "." maybePath))
    go _ maybePath ("-b":rest) =
        case rest of
            branchArgument:remaining -> go branchArgument maybePath remaining
            [] -> Left "haskell-git: init -b needs a branch name"
    go branchName Nothing (path:rest) = go branchName (Just path) rest
    go _ (Just _) _ = Left "haskell-git: init accepts one path"

parseHashObject :: [String] -> Either String Command
parseHashObject = go False False Nothing
  where
    go shouldWrite shouldReadStdin maybePath [] =
        case (shouldReadStdin, maybePath) of
            (True, _) -> Right (HashObjectCommand (HashObjectOptions shouldWrite True ""))
            (False, Just path) -> Right (HashObjectCommand (HashObjectOptions shouldWrite False path))
            (False, Nothing) -> Left "haskell-git: hash-object needs a file, or --stdin"
    go shouldWrite shouldReadStdin maybePath (argument:rest)
        | argument == "-w" = go True shouldReadStdin maybePath rest
        | argument == "--stdin" = go shouldWrite True maybePath rest
        | otherwise =
            case maybePath of
                Nothing -> go shouldWrite shouldReadStdin (Just argument) rest
                Just _ -> Left "haskell-git: hash-object accepts one file"

parseCatFile :: [String] -> Either String Command
parseCatFile (modeFlag:objectText:[])
    | modeFlag == "-t" = Right (CatFileCommand CatFileType objectText)
    | modeFlag == "-s" = Right (CatFileCommand CatFileSize objectText)
    | modeFlag == "-p" = Right (CatFileCommand CatFilePretty objectText)
parseCatFile _ = Left "haskell-git: cat-file needs one of -t, -s, or -p, and an object"

parseLsTree :: [String] -> Either String Command
parseLsTree = go (LsTreeOptions False False) Nothing
  where
    go options maybeRevision [] =
        case maybeRevision of
            Just revision -> Right (LsTreeCommand options revision)
            Nothing -> Left "haskell-git: ls-tree needs a revision"
    go options maybeRevision ("-r":rest) =
        go (options {lsTreeRecursive = True}) maybeRevision rest
    go options maybeRevision ("--name-only":rest) =
        go (options {lsTreeNameOnly = True}) maybeRevision rest
    go options Nothing (argument:rest) = go options (Just argument) rest
    go _ (Just _) _ = Left "haskell-git: ls-tree accepts one revision"

parseLog :: [String] -> Either String Command
parseLog = go Nothing Nothing
  where
    go maybeLimit maybeRevision [] =
        Right (LogCommand maybeLimit (fromMaybe "HEAD" maybeRevision))
    go _ maybeRevision ("-n":rest) =
        case rest of
            countText:remaining ->
                case readMaybe countText of
                    Just count -> go (Just count) maybeRevision remaining
                    Nothing -> Left "haskell-git: log -n needs a number"
            [] -> Left "haskell-git: log -n needs a number"
    go maybeLimit Nothing (argument:rest) = go maybeLimit (Just argument) rest
    go _ (Just _) _ = Left "haskell-git: log accepts one revision"

parseCommitTree :: [String] -> Either String Command
parseCommitTree arguments =
    case arguments of
        treeText:rest -> go treeText [] Nothing rest
        [] -> Left "haskell-git: commit-tree needs a tree"
  where
    go treeText parents maybeMessage [] =
        case maybeMessage of
            Just messageText -> Right (CommitTreeCommand treeText parents messageText)
            Nothing -> Left "haskell-git: commit-tree needs -m MESSAGE"
    go treeText parents maybeMessage ("-m":messageText:remaining) =
        case maybeMessage of
            Just _ -> Left "haskell-git: commit-tree accepts one -m"
            Nothing -> go treeText parents (Just messageText) remaining
    go treeText parents maybeMessage ("-p":parentText:remaining) =
        go treeText (parents ++ [parentText]) maybeMessage remaining
    go _ _ _ _ = Left "haskell-git: commit-tree arguments are not valid"

knownCommands :: [String]
knownCommands =
    [ "init"
    , "add"
    , "hash-object"
    , "cat-file"
    , "ls-tree"
    , "log"
    , "rev-parse"
    , "write-tree"
    , "commit-tree"
    , "update-ref"
    , "clone"
    , "verify"
    ]

usageText :: String
usageText =
    unlines
        [ "usage: haskell-git <command> [<args>]"
        , ""
        , "commands:"
        , "  init [-b branch] [path]          create a repository"
        , "  add <path>...                    stage working-tree files"
        , "  hash-object [-w] [--stdin] file  hash a blob, optionally write it"
        , "  cat-file (-t | -s | -p) object   show an object"
        , "  ls-tree [-r] [--name-only] rev   list a tree"
        , "  log [-n count] [rev]             list commits"
        , "  rev-parse rev                    print an object id"
        , "  write-tree                       write trees from the index"
        , "  commit-tree tree [-p parent]... -m message"
        , "                                   write a commit object"
        , "  update-ref ref rev               point a reference at an object"
        , "  clone url [dir]                  clone over Smart HTTP"
        , "  verify [path]                    check objects and references"
        ]

commandUsage :: String -> Maybe String
commandUsage name =
    lookup
        name
        [ ("init", "usage: haskell-git init [-b branch] [path]\n")
        , ("add", "usage: haskell-git add <path>...\n")
        , ("hash-object", "usage: haskell-git hash-object [-w] [--stdin] <file>\n")
        , ("cat-file", "usage: haskell-git cat-file (-t | -s | -p) <object>\n")
        , ("ls-tree", "usage: haskell-git ls-tree [-r] [--name-only] <revision>\n")
        , ("log", "usage: haskell-git log [-n <count>] [<revision>]\n")
        , ("rev-parse", "usage: haskell-git rev-parse <revision>\n")
        , ("write-tree", "usage: haskell-git write-tree\n")
        , ("commit-tree", "usage: haskell-git commit-tree <tree> [-p <parent>]... -m <message>\n")
        , ("update-ref", "usage: haskell-git update-ref <ref> <revision>\n")
        , ("clone", "usage: haskell-git clone <url> [<directory>]\n")
        , ("verify", "usage: haskell-git verify [<path>]\n")
        ]

runCommand :: Command -> IO ()
runCommand (HelpCommand maybeCommand) =
    case maybeCommand of
        Nothing -> putStr usageText
        Just name ->
            case commandUsage name of
                Just usage -> putStr usage
                Nothing -> do
                    hPutStrLn stderr ("haskell-git: unknown command: " ++ name)
                    exitWith (ExitFailure 2)
runCommand VersionCommand = putStrLn ("haskell-git " ++ versionText)

runCommand (InitCommand branchName targetPath) = do
    existingRepository <- doesDirectoryExist (targetPath </> ".git")
    _ <- initRepositoryWithBranch branchName targetPath
    absolutePath <- makeAbsolute targetPath
    let action = if existingRepository then "Reinitialized existing" else "Initialized empty"
    putStrLn (action ++ " Git repository in " ++ absolutePath ++ "/.git/")

runCommand (AddCommand paths) = do
    repository <- requireRepository "."
    existingEntries <- readIndexFile (repositoryIndexPath repository)
    updatedEntries <- foldM (stagePath repository) existingEntries paths
    BS.writeFile (repositoryIndexPath repository) (writeIndex updatedEntries)

runCommand (HashObjectCommand options) = do
    payload <-
        if hashObjectReadStdin options
            then BS.getContents
            else BS.readFile (hashObjectPath options)
    let blobObject = blobToObject (blobFromBytes payload)
        objectId = objectIdForObject blobObject
    when (hashObjectWrite options) $ do
        repository <- requireRepository "."
        writtenObjectId <- writeLooseObject repository blobObject
        when (writtenObjectId /= objectId) $
            ioError (userError "blob content does not hash to its id")
    putStrLn (objectIdToHex objectId)

runCommand (CatFileCommand mode objectText) = do
    repository <- requireRepository "."
    store <- loadStore repository
    objectId <- resolveObjectId repository store objectText
    gitObjectValue <- readObjectOrFail store objectId
    case mode of
        CatFileType -> putStrLn (objectTypeName (gitObjectType gitObjectValue))
        CatFileSize -> putStrLn (show (BS.length (gitObjectPayload gitObjectValue)))
        CatFilePretty -> BS.hPut stdout =<< prettyObject gitObjectValue

runCommand (LsTreeCommand options objectText) = do
    repository <- requireRepository "."
    store <- loadStore repository
    objectId <- resolveObjectId repository store objectText
    treeId <- peelToTree store objectId
    if lsTreeRecursive options
        then do
            flattened <- flattenTree store treeId
            BS.hPut
                stdout
                ( BS.concat
                    [ renderTreeLine options (BS8.pack path) entry
                    | (path, entry) <- flattened
                    ]
                )
        else do
            treeObject <- readObjectOrFail store treeId
            entries <- either (ioError . userError) pure (treeFromObject treeObject)
            BS.hPut
                stdout
                ( BS.concat
                    [ renderTreeLine options (treeEntryName entry) entry
                    | entry <- entries
                    ]
                )

runCommand (LogCommand maybeLimit objectText) = do
    repository <- requireRepository "."
    store <- loadStore repository
    objectId <- resolveObjectId repository store objectText
    startCommitId <- peelToCommit store objectId
    commits <- walkCommits store startCommitId
    let selected = case maybeLimit of
            Nothing -> commits
            Just limit -> take limit commits
    mapM_ (putStr . renderLogEntry) selected

runCommand (RevParseCommand objectText) = do
    repository <- requireRepository "."
    store <- loadStore repository
    objectId <- resolveObjectId repository store objectText
    putStrLn (objectIdToHex objectId)

runCommand WriteTreeCommand = do
    repository <- requireRepository "."
    entries <- readIndexFile (repositoryIndexPath repository)
    treeId <- buildTreeFromIndex repository entries
    putStrLn (objectIdToHex treeId)

runCommand (CommitTreeCommand treeText parentTexts messageText) = do
    repository <- requireRepository "."
    store <- loadStore repository
    treeId <- resolveObjectId repository store treeText
    treeObject <- readObjectOrFail store treeId
    when (gitObjectType treeObject /= TreeObject) $
        ioError (userError "commit-tree: the first argument is not a tree")
    parentIds <- forM parentTexts $ \parentText -> do
        parentId <- resolveObjectId repository store parentText
        parentObject <- readObjectOrFail store parentId
        when (gitObjectType parentObject /= CommitObject) $
            ioError (userError ("commit-tree: parent is not a commit: " ++ parentText))
        pure parentId
    author <- resolvePersonIdent repository "GIT_AUTHOR"
    committer <- resolvePersonIdent repository "GIT_COMMITTER"
    let currentCommit =
            commit
                treeId
                parentIds
                author
                committer
                (normalizeMessage (stringToUtf8 messageText))
    commitId <- writeLooseObject repository (commitToObject currentCommit)
    putStrLn (objectIdToHex commitId)

runCommand (UpdateRefCommand refPath objectText) = do
    repository <- requireRepository "."
    store <- loadStore repository
    objectId <- resolveObjectId repository store objectText
    writeRefValue repository refPath objectId

runCommand (CloneCommand remoteUrl maybeDestination) = do
    let destination = fromMaybe (defaultCloneDestination remoteUrl) maybeDestination
    putStrLn ("Cloning into '" ++ destination ++ "'...")
    result <- cloneRepository remoteUrl destination
    case cloneHead result of
        Nothing -> putStrLn "warning: you appear to have cloned an empty repository"
        Just headObjectId ->
            putStrLn
                ( "HEAD is "
                    ++ objectIdToHex headObjectId
                    ++ " on "
                    ++ shortBranchName (cloneLocalBranch result)
                )

runCommand (VerifyCommand path) = do
    repository <- requireRepository path
    report <- verifyRepository repository
    forM_ (renderVerifyReport report) putStrLn
    when (not (null (verifyProblems report))) (exitWith (ExitFailure 1))

prettyObject :: GitObject -> IO ByteString
prettyObject gitObjectValue =
    case gitObjectType gitObjectValue of
        TreeObject ->
            case treeFromObject gitObjectValue of
                Left err -> ioError (userError err)
                Right entries ->
                    pure
                        ( BS.concat
                            [ renderTreeEntry entry <> BS8.singleton '\n'
                            | entry <- entries
                            ]
                        )
        _ -> pure (gitObjectPayload gitObjectValue)

renderTreeLine :: LsTreeOptions -> ByteString -> TreeEntry -> ByteString
renderTreeLine options displayName entry
    | lsTreeNameOnly options = displayName <> BS8.singleton '\n'
    | otherwise = renderTreeEntry (entry {treeEntryName = displayName}) <> BS8.singleton '\n'

peelToTree :: Store -> ObjectId -> IO ObjectId
peelToTree store objectId = do
    gitObjectValue <- readObjectOrFail store objectId
    case gitObjectType gitObjectValue of
        TreeObject -> pure objectId
        CommitObject ->
            case commitFromObject gitObjectValue of
                Left err -> ioError (userError err)
                Right currentCommit -> pure (commitTree currentCommit)
        TagObject ->
            case tagFromObject gitObjectValue of
                Left err -> ioError (userError err)
                Right currentTag -> peelToTree store (tagObject currentTag)
        BlobObject -> ioError (userError "object is a blob, not a tree")

peelToCommit :: Store -> ObjectId -> IO ObjectId
peelToCommit store objectId = do
    gitObjectValue <- readObjectOrFail store objectId
    case gitObjectType gitObjectValue of
        CommitObject -> pure objectId
        TagObject ->
            case tagFromObject gitObjectValue of
                Left err -> ioError (userError err)
                Right currentTag -> peelToCommit store (tagObject currentTag)
        _ -> ioError (userError "object does not peel to a commit")

resolveObjectId :: Repository -> Store -> String -> IO ObjectId
resolveObjectId repository store objectText = do
    maybeObjectId <- resolveRevision repository objectText
    case maybeObjectId of
        Nothing -> ioError (userError ("unknown revision: " ++ objectText))
        Just objectId -> do
            exists <- objectExists store objectId
            if exists
                then pure objectId
                else ioError (userError ("object not found: " ++ objectIdToHex objectId))

renderLogEntry :: (ObjectId, Commit) -> String
renderLogEntry (objectId, currentCommit) =
    unlines
        ( [ "commit " ++ objectIdToHex objectId
          , "Author: "
                ++ BS8.unpack (personIdentName (commitAuthor currentCommit))
                ++ " <"
                ++ BS8.unpack (personIdentEmail (commitAuthor currentCommit))
                ++ ">"
          , "Date:   " ++ formatPersonDate (commitAuthor currentCommit)
          , ""
          ]
            ++ map indentMessageLine (BS8.split '\n' (commitMessage currentCommit))
            ++ [""]
        )

indentMessageLine :: ByteString -> String
indentMessageLine line
    | BS.null line = ""
    | otherwise = "    " ++ BS8.unpack line

formatPersonDate :: PersonIdent -> String
formatPersonDate ident =
    formatTime
        defaultTimeLocale
        "%a %b %e %H:%M:%S %Y %z"
        ( utcToZonedTime
            (timezoneFromText (personIdentTimezone ident))
            (posixSecondsToUTCTime (fromIntegral (personIdentTimestamp ident)))
        )

timezoneFromText :: ByteString -> TimeZone
timezoneFromText text =
    let characters = BS8.unpack text
        sign = case characters of
            ('-':_) -> -1
            _ -> 1
        digits = drop 1 characters
        hours = fromMaybe 0 (readMaybe (take 2 digits))
        minutes = fromMaybe 0 (readMaybe (drop 2 digits))
     in minutesToTimeZone (sign * (hours * 60 + minutes))

readIndexFile :: FilePath -> IO [IndexEntry]
readIndexFile path = do
    indexExists <- doesFileExist path
    if not indexExists
        then pure []
        else do
            indexBytes <- BS.readFile path
            either
                (ioError . userError . (("index " ++ path ++ ": ") ++))
                pure
                (parseIndex indexBytes)

stagePath :: Repository -> [IndexEntry] -> FilePath -> IO [IndexEntry]
stagePath repository staged path = do
    absolutePath <- makeAbsolute path
    repositoryRoot <- makeAbsolute (repositoryRootDir repository)
    if not (isWithin repositoryRoot absolutePath)
        then ioError (userError ("path is outside the repository: " ++ path))
        else do
            let relativePath = makeRelative repositoryRoot absolutePath
                prefix = if relativePath == "." then "" else relativePath
                kept = filter (not . matchesIndexPath (BS8.pack prefix)) staged
            pathExists <- doesPathExist absolutePath
            if not pathExists
                then pure kept
                else do
                    isSymbolicLink <- pathIsSymbolicLink absolutePath
                    isDirectory <- doesDirectoryExist absolutePath
                    files <-
                        if isDirectory && not isSymbolicLink
                            then listFilesRecursively absolutePath
                            else pure [absolutePath]
                    foldM (stageFile repository repositoryRoot) kept files

stageFile :: Repository -> FilePath -> [IndexEntry] -> FilePath -> IO [IndexEntry]
stageFile repository repositoryRoot staged absolutePath = do
    isSymbolicLink <- pathIsSymbolicLink absolutePath
    (payload, mode) <-
        if isSymbolicLink
            then do
                target <- getSymbolicLinkTarget absolutePath
                pure (stringToUtf8 target, Symlink)
            else do
                filePayload <- BS.readFile absolutePath
                permissions <- getPermissions absolutePath
                let fileMode = if executable permissions then ExecutableFile else RegularFile
                pure (filePayload, fileMode)
    objectId <- writeLooseObject repository (blobToObject (blobFromBytes payload))
    let relativePath = makeRelative repositoryRoot absolutePath
        entry =
            IndexEntry
                { indexEntryPath = BS8.pack relativePath
                , indexEntryMode = mode
                , indexEntrySize = fromIntegral (BS.length payload)
                , indexEntryObjectId = objectId
                }
    pure (upsertIndexEntry entry staged)

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively directory = do
    names <- listDirectory directory
    fmap concat $
        forM names $ \name ->
            if name == ".git"
                then pure []
                else do
                    let path = directory </> name
                    isSymbolicLink <- pathIsSymbolicLink path
                    isDirectory <- doesDirectoryExist path
                    if isDirectory && not isSymbolicLink
                        then listFilesRecursively path
                        else pure [path]

matchesIndexPath :: ByteString -> IndexEntry -> Bool
matchesIndexPath prefix entry
    | BS.null prefix = True
    | entryPath == prefix = True
    | otherwise = BS8.isPrefixOf (prefix <> BS8.singleton '/') entryPath
  where
    entryPath = indexEntryPath entry

isWithin :: FilePath -> FilePath -> Bool
isWithin parent child = parent == child || (parent ++ "/") `isPrefixOf` child

resolvePersonIdent :: Repository -> String -> IO PersonIdent
resolvePersonIdent repository prefix = do
    nameValue <- identityValue prefix "NAME" "name"
    emailValue <- identityValue prefix "EMAIL" "email"
    timestamp <- round <$> getPOSIXTime
    timeZone <- getCurrentTimeZone
    pure
        ( personIdent
            (stringToUtf8 nameValue)
            (stringToUtf8 emailValue)
            timestamp
            (timezoneText timeZone)
        )
  where
    identityValue prefixText environmentSuffix configKey = do
        environmentValue <- lookupEnv (prefixText ++ "_" ++ environmentSuffix)
        case environmentValue of
            Just value -> pure value
            Nothing -> do
                configValue <- readConfigValue repository "user" configKey
                case configValue of
                    Just value -> pure value
                    Nothing ->
                        ioError
                            ( userError
                                ( "cannot determine identity: set "
                                    ++ prefixText
                                    ++ "_"
                                    ++ environmentSuffix
                                    ++ " or user."
                                    ++ configKey
                                )
                            )

timezoneText :: TimeZone -> ByteString
timezoneText timeZone =
    let offsetMinutes = timeZoneMinutes timeZone
        sign = if offsetMinutes < 0 then "-" else "+"
        absoluteMinutes = abs offsetMinutes
        hours = absoluteMinutes `div` 60
        minutes = absoluteMinutes `mod` 60
     in BS8.pack (sign ++ padTwo hours ++ padTwo minutes)

padTwo :: Int -> String
padTwo value
    | value < 10 = '0' : show value
    | otherwise = show value

normalizeMessage :: ByteString -> ByteString
normalizeMessage message
    | BS.null message = BS8.singleton '\n'
    | BS8.last message == '\n' = message
    | otherwise = message <> BS8.singleton '\n'

stringToUtf8 :: String -> ByteString
stringToUtf8 = BS.pack . map fromIntegral . concatMap encodeCharacter

encodeCharacter :: Char -> [Int]
encodeCharacter character =
    let code = fromIntegral (ord character) :: Int
     in if code <= 0x7f
            then [code]
            else
                if code <= 0x7ff
                    then
                        [ 0xc0 .|. (code `shiftR` 6)
                        , 0x80 .|. (code .&. 0x3f)
                        ]
                    else
                        if code <= 0xffff
                            then
                                [ 0xe0 .|. (code `shiftR` 12)
                                , 0x80 .|. ((code `shiftR` 6) .&. 0x3f)
                                , 0x80 .|. (code .&. 0x3f)
                                ]
                            else
                                [ 0xf0 .|. (code `shiftR` 18)
                                , 0x80 .|. ((code `shiftR` 12) .&. 0x3f)
                                , 0x80 .|. ((code `shiftR` 6) .&. 0x3f)
                                , 0x80 .|. (code .&. 0x3f)
                                ]

defaultCloneDestination :: String -> FilePath
defaultCloneDestination remoteUrl =
    let withoutTrailingSlashes = reverse (dropWhile (== '/') (reverse remoteUrl))
        lastSegment = reverse (takeWhile (/= '/') (reverse withoutTrailingSlashes))
        withoutGitSuffix =
            if ".git" `isSuffixOf` lastSegment
                then take (length lastSegment - length ".git") lastSegment
                else lastSegment
     in if null withoutGitSuffix then "repository" else withoutGitSuffix

shortBranchName :: FilePath -> FilePath
shortBranchName branchPath
    | "refs/heads/" `isPrefixOf` branchPath = drop (length "refs/heads/") branchPath
    | otherwise = branchPath

versionText :: String
versionText = "0.1.0"

defaultBranchName :: String
defaultBranchName = "main"
