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
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
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
import Git.Commit (commitCurrentIndex)
import Git.Config
    ( configEntries
    , configGet
    , configGetAll
    , ConfigEntry (..)
    , loadRepositoryConfig
    )
import Git.Fetch
    ( FetchOptions (..)
    , FetchResult (..)
    , defaultFetchOptions
    , fetchRepository
    )
import Git.Hash (HashAlgorithm (..), ObjectId, defaultHashAlgorithm, objectIdToHex)
import Git.Index
    ( IndexEntry (..)
    , buildTreeFromIndex
    , parseIndexFor
    , upsertIndexEntry
    , writeIndexFor
    )
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
    , commitFromObjectFor
    , commitMessage
    , commitToObject
    , commitTree
    , personIdent
    , personIdentEmail
    , personIdentName
    , personIdentTimestamp
    , personIdentTimezone
    )
import Git.Object.Loose (objectIdForObjectFor, writeLooseObject)
import Git.Object.Person (PersonIdent)
import Git.Object.Tag (tagFromObjectFor, tagObject)
import Git.Merge
    ( MergeResult (..)
    , mergeAbort
    , mergeContinue
    , mergeRepository
    )
import Git.Object.Tree
    ( TreeEntry (..)
    , TreeEntryMode (..)
    , renderTreeEntry
    , treeFromObjectFor
    )
import Git.Pull (pullRepository, pullRepositoryRebase)
import Git.Rebase
    ( RebaseResult (..)
    , rebaseAbort
    , rebaseContinue
    , rebaseRepository
    , rebaseSkip
    )
import Git.Push
    ( PushOptions (..)
    , PushResult (..)
    , defaultPushOptions
    , pushRepository
    )
import Git.Refs
    ( HeadReference (..)
    , listRefs
    , readRefValue
    , resolveRevision
    , symbolicHead
    , writeHeadReference
    , writeRefValue
    )
import Git.Worktree
    ( checkoutTree
    , createBranch
    , resetHard
    , statusRepository
    )
import Git.Repo
    ( Repository
    , findRepository
    , initRepositoryWithFormat
    , readConfigValue
    , repositoryConfigPath
    , repositoryIndexPath
    , repositoryObjectFormat
    , repositoryRootDir
    , requireRepository
    )
import Git.Store (Store, flattenTree, loadStore, objectExists, readObjectOrFail, walkCommits)
import Git.Submodule
    ( initialiseSubmodules
    , readSubmodules
    , submoduleStatus
    , updateSubmodules
    )
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
    = InitCommand InitOptions
    | AddCommand [FilePath]
    | HashObjectCommand HashObjectOptions
    | CatFileCommand CatFileMode String
    | LsTreeCommand LsTreeOptions String
    | LogCommand (Maybe Int) String
    | RevParseCommand String
    | WriteTreeCommand
    | CommitTreeCommand String [String] String
    | UpdateRefCommand String String
    | CloneCommand Bool String (Maybe FilePath)
    | FetchCommand String [String] FetchOptions
    | PushCommand String [String] PushOptions
    | CheckoutCommand (Maybe String) String
    | SwitchCommand String
    | BranchCommand (Maybe String)
    | StatusCommand
    | ResetCommand String
    | MergeCommand MergeAction
    | CommitCommand String
    | PullCommand String FetchOptions
    | PullRebaseCommand String
    | RebaseCommand RebaseAction
    | SubmoduleCommand SubmoduleAction
    | RemoteCommand RemoteAction
    | VerifyCommand FilePath
    | HelpCommand (Maybe String)
    | VersionCommand

data MergeAction
    = MergeRevision String
    | MergeAbort
    | MergeContinue

data RebaseAction
    = RebaseRevision String
    | RebaseAbort
    | RebaseContinue
    | RebaseSkip

data SubmoduleAction
    = SubmoduleStatus
    | SubmoduleUpdate Bool

data RemoteAction
    = RemoteAdd String String
    | RemoteSetUrl String String
    | RemoteGetUrl String
    | RemoteList

data HashObjectOptions = HashObjectOptions
    { hashObjectWrite :: Bool
    , hashObjectReadStdin :: Bool
    , hashObjectPath :: FilePath
    }

data InitOptions = InitOptions
    { initBranchName :: String
    , initObjectFormat :: HashAlgorithm
    , initPath :: FilePath
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
parseCommand ("clone":"--recurse-submodules":rest) = parseClone True rest
parseCommand ("clone":rest) = parseClone False rest
parseCommand ("fetch":rest) = parseFetch rest
parseCommand ("push":rest) = parsePush rest
parseCommand ("checkout":rest) = parseCheckout rest
parseCommand ("switch":[branchName]) = Right (SwitchCommand branchName)
parseCommand ("branch":rest) = parseBranch rest
parseCommand ["status"] = Right StatusCommand
parseCommand ["reset", "--hard", revision] = Right (ResetCommand revision)
parseCommand ("merge":rest) = parseMerge rest
parseCommand ("commit":rest) = parseCommit rest
parseCommand ("pull":rest) = parsePull rest
parseCommand ("rebase":rest) = parseRebase rest
parseCommand ("submodule":rest) = parseSubmodule rest
parseCommand ("reset":_) = Left "haskell-git: reset only supports --hard <revision>"
parseCommand ("remote":rest) = parseRemote rest
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
parseInit = go defaultBranchName defaultHashAlgorithm Nothing
  where
    go branchName objectFormat maybePath [] =
        Right (InitCommand (InitOptions branchName objectFormat (fromMaybe "." maybePath)))
    go _ objectFormat maybePath ("-b":rest) =
        case rest of
            branchArgument:remaining -> go branchArgument objectFormat maybePath remaining
            [] -> Left "haskell-git: init -b needs a branch name"
    go branchName _ maybePath ("--object-format":formatText:remaining) = do
        parsedFormat <- parseObjectFormat formatText
        go branchName parsedFormat maybePath remaining
    go branchName objectFormat maybePath (argument:rest)
        | Just formatText <- stripPrefix "--object-format=" argument = do
            parsedFormat <- parseObjectFormat formatText
            go branchName parsedFormat maybePath rest
        | otherwise =
            case maybePath of
                Nothing -> go branchName objectFormat (Just argument) rest
                Just _ -> Left "haskell-git: init accepts one path"

parseObjectFormat :: String -> Either String HashAlgorithm
parseObjectFormat "sha1" = Right Sha1
parseObjectFormat "sha256" = Right Sha256
parseObjectFormat other =
    Left ("haskell-git: unsupported object format: " ++ other)

parseFetch :: [String] -> Either String Command
parseFetch arguments = do
    (options, positionals) <- go defaultFetchOptions [] arguments
    case positionals of
        [] -> Right (FetchCommand "origin" [] options)
        remote : refspecs -> Right (FetchCommand remote refspecs options)
  where
    go options positionals [] = Right (options, reverse positionals)
    go options positionals ("--prune" : rest) =
        go (options {fetchPrune = True}) positionals rest
    go options positionals ("--tags" : rest) =
        go (options {fetchTags = True}) positionals rest
    go options positionals ("--no-tags" : rest) =
        go (options {fetchTags = False}) positionals rest
    go _ _ (argument : _)
        | "-" `isPrefixOf` argument =
            Left ("haskell-git: fetch does not understand " ++ argument)
    go options positionals (argument : rest) =
        go options (argument : positionals) rest

parsePush :: [String] -> Either String Command
parsePush arguments = do
    (options, positionals) <- go defaultPushOptions [] arguments
    case positionals of
        [] -> Right (PushCommand "origin" [] options)
        remote : refspecs -> Right (PushCommand remote refspecs options)
  where
    go options positionals [] = Right (options, reverse positionals)
    go options positionals ("--force" : rest) =
        go (options {pushForce = True}) positionals rest
    go options positionals ("-f" : rest) =
        go (options {pushForce = True}) positionals rest
    go options positionals ("--delete" : rest) =
        go (options {pushDelete = True}) positionals rest
    go _ _ (argument : _)
        | "-" `isPrefixOf` argument =
            Left ("haskell-git: push does not understand " ++ argument)
    go options positionals (argument : rest) =
        go options (argument : positionals) rest

parseCheckout :: [String] -> Either String Command
parseCheckout ["-b", branchName, start] =
    Right (CheckoutCommand (Just branchName) start)
parseCheckout ["-b", branchName] =
    Right (CheckoutCommand (Just branchName) "HEAD")
parseCheckout [revision] = Right (CheckoutCommand Nothing revision)
parseCheckout _ =
    Left "haskell-git: usage: checkout [-b <branch> [<start>]] <revision>"

parseBranch :: [String] -> Either String Command
parseBranch [] = Right (BranchCommand Nothing)
parseBranch [name] = Right (BranchCommand (Just name))
parseBranch _ = Left "haskell-git: usage: branch [<name>]"

parseMerge :: [String] -> Either String Command
parseMerge ["--abort"] = Right (MergeCommand MergeAbort)
parseMerge ["--continue"] = Right (MergeCommand MergeContinue)
parseMerge [revision] = Right (MergeCommand (MergeRevision revision))
parseMerge _ = Left "haskell-git: usage: merge [--abort|--continue] <revision>"

parseCommit :: [String] -> Either String Command
parseCommit ["-m", message] = Right (CommitCommand message)
parseCommit _ = Left "haskell-git: usage: commit -m <message>"

parsePull :: [String] -> Either String Command
parsePull [] = Right (PullCommand "origin" defaultFetchOptions)
parsePull ["--rebase"] = Right (PullRebaseCommand "origin")
parsePull [remote] = Right (PullCommand remote defaultFetchOptions)
parsePull _ = Left "haskell-git: usage: pull [<remote>]"

parseRebase :: [String] -> Either String Command
parseRebase ["--abort"] = Right (RebaseCommand RebaseAbort)
parseRebase ["--continue"] = Right (RebaseCommand RebaseContinue)
parseRebase ["--skip"] = Right (RebaseCommand RebaseSkip)
parseRebase [revision] = Right (RebaseCommand (RebaseRevision revision))
parseRebase _ = Left "haskell-git: usage: rebase [--abort|--continue|--skip] <upstream>"

parseClone :: Bool -> [String] -> Either String Command
parseClone recursive [remoteUrl] = Right (CloneCommand recursive remoteUrl Nothing)
parseClone recursive [remoteUrl, destination] =
    Right (CloneCommand recursive remoteUrl (Just destination))
parseClone _ _ = Left "haskell-git: usage: clone [--recurse-submodules] <url> [<directory>]"

parseSubmodule :: [String] -> Either String Command
parseSubmodule ["status"] = Right (SubmoduleCommand SubmoduleStatus)
parseSubmodule ("update":rest) = Right (SubmoduleCommand (SubmoduleUpdate ("--recursive" `elem` rest)))
parseSubmodule _ = Left "haskell-git: usage: submodule status | submodule update --init [--recursive]"

parseRemote :: [String] -> Either String Command
parseRemote [] = Right (RemoteCommand RemoteList)
parseRemote ["-v"] = Right (RemoteCommand RemoteList)
parseRemote ["add", name, url] = Right (RemoteCommand (RemoteAdd name url))
parseRemote ["set-url", name, url] = Right (RemoteCommand (RemoteSetUrl name url))
parseRemote ["get-url", name] = Right (RemoteCommand (RemoteGetUrl name))
parseRemote _ =
    Left "haskell-git: usage: remote [add <name> <url> | set-url <name> <url> | get-url <name> | -v]"

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
    , "fetch"
    , "push"
    , "checkout"
    , "switch"
    , "branch"
    , "status"
    , "reset"
    , "merge"
    , "commit"
    , "pull"
    , "rebase"
    , "submodule"
    , "remote"
    , "verify"
    ]

usageText :: String
usageText =
    unlines
        [ "usage: haskell-git <command> [<args>]"
        , ""
        , "commands:"
        , "  init [-b branch] [--object-format=sha1|sha256] [path]"
        , "                                   create a repository"
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
        , "  clone [--recurse-submodules] url [dir]"
        , "                                   clone over HTTP, SSH, or git://"
        , "  fetch [remote] [refspec...]      fetch branches and tags"
        , "  push [--force] [--delete] remote [refspec...]"
        , "                                   update remote branches"
        , "  checkout [-b branch [start]] rev switch branches or commits"
        , "  switch branch                    switch to a branch"
        , "  branch [name]                    create a branch at HEAD"
        , "  status                           show working-tree status"
        , "  reset --hard rev                 move HEAD and reset the worktree"
        , "  merge [--abort|--continue] rev   merge a commit or branch"
        , "  commit -m message                commit the index"
        , "  pull [remote]                    fetch and merge"
        , "  rebase [--abort|--continue|--skip] upstream"
        , "                                   replay the current branch"
        , "  submodule status|update --init   manage submodules"
        , "  remote add|set-url|get-url ...   manage remotes"
        , "  verify [path]                    check objects and references"
        ]

commandUsage :: String -> Maybe String
commandUsage name =
    lookup
        name
        [ ("init", "usage: haskell-git init [-b <branch>] [--object-format=sha1|sha256] [<path>]\n")
        , ("add", "usage: haskell-git add <path>...\n")
        , ("hash-object", "usage: haskell-git hash-object [-w] [--stdin] <file>\n")
        , ("cat-file", "usage: haskell-git cat-file (-t | -s | -p) <object>\n")
        , ("ls-tree", "usage: haskell-git ls-tree [-r] [--name-only] <revision>\n")
        , ("log", "usage: haskell-git log [-n <count>] [<revision>]\n")
        , ("rev-parse", "usage: haskell-git rev-parse <revision>\n")
        , ("write-tree", "usage: haskell-git write-tree\n")
        , ("commit-tree", "usage: haskell-git commit-tree <tree> [-p <parent>]... -m <message>\n")
        , ("update-ref", "usage: haskell-git update-ref <ref> <revision>\n")
        , ("clone", "usage: haskell-git clone [--recurse-submodules] <url> [<directory>]\n")
        , ("fetch", "usage: haskell-git fetch [<remote>] [<refspec>...]\n")
        , ("push", "usage: haskell-git push [--force] [--delete] <remote> [<refspec>...]\n")
        , ("checkout", "usage: haskell-git checkout [-b <branch> [<start>]] <revision>\n")
        , ("switch", "usage: haskell-git switch <branch>\n")
        , ("branch", "usage: haskell-git branch [<name>]\n")
        , ("status", "usage: haskell-git status\n")
        , ("reset", "usage: haskell-git reset --hard <revision>\n")
        , ("merge", "usage: haskell-git merge [--abort|--continue] <revision>\n")
        , ("commit", "usage: haskell-git commit -m <message>\n")
        , ("pull", "usage: haskell-git pull [<remote>]\n")
        , ("rebase", "usage: haskell-git rebase [--abort|--continue|--skip] <upstream>\n")
        , ("submodule", "usage: haskell-git submodule status | submodule update --init [--recursive]\n")
        , ("remote", "usage: haskell-git remote add|set-url|get-url <name> [<url>]\n")
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

runCommand (InitCommand options) = do
    let targetPath = initPath options
    existingRepository <- doesDirectoryExist (targetPath </> ".git")
    _ <-
        initRepositoryWithFormat
            (initObjectFormat options)
            (initBranchName options)
            targetPath
    absolutePath <- makeAbsolute targetPath
    let action = if existingRepository then "Reinitialized existing" else "Initialized empty"
    putStrLn (action ++ " Git repository in " ++ absolutePath ++ "/.git/")

runCommand (AddCommand paths) = do
    repository <- requireRepository "."
    let objectFormat = repositoryObjectFormat repository
    existingEntries <- readIndexFile objectFormat (repositoryIndexPath repository)
    updatedEntries <- foldM (stagePath repository) existingEntries paths
    BS.writeFile
        (repositoryIndexPath repository)
        (writeIndexFor objectFormat updatedEntries)

runCommand (HashObjectCommand options) = do
    repositoryMaybe <- findRepository "."
    let objectFormat =
            maybe defaultHashAlgorithm repositoryObjectFormat repositoryMaybe
    payload <-
        if hashObjectReadStdin options
            then BS.getContents
            else BS.readFile (hashObjectPath options)
    let blobObject = blobToObject (blobFromBytes payload)
        objectId = objectIdForObjectFor objectFormat blobObject
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
        CatFilePretty ->
            BS.hPut
                stdout
                =<< prettyObject (repositoryObjectFormat repository) gitObjectValue

runCommand (LsTreeCommand options objectText) = do
    repository <- requireRepository "."
    store <- loadStore repository
    objectId <- resolveObjectId repository store objectText
    treeId <- peelToTree (repositoryObjectFormat repository) store objectId
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
            entries <-
                either
                    (ioError . userError)
                    pure
                    (treeFromObjectFor (repositoryObjectFormat repository) treeObject)
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
    startCommitId <- peelToCommit (repositoryObjectFormat repository) store objectId
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
    entries <-
        readIndexFile
            (repositoryObjectFormat repository)
            (repositoryIndexPath repository)
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

runCommand (CloneCommand recursive remoteUrl maybeDestination) = do
    let destination = fromMaybe (defaultCloneDestination remoteUrl) maybeDestination
    putStrLn ("Cloning into '" ++ destination ++ "'...")
    result <- cloneRepository remoteUrl destination
    when recursive $ do
        found <- findRepository destination
        case found of
            Just repository -> do
                store <- loadStore repository
                initialiseSubmodules repository store True
            Nothing -> pure ()
    case cloneHead result of
        Nothing -> putStrLn "warning: you appear to have cloned an empty repository"
        Just headObjectId ->
            putStrLn
                ( "HEAD is "
                    ++ objectIdToHex headObjectId
                    ++ " on "
                    ++ shortBranchName (cloneLocalBranch result)
                )

runCommand (FetchCommand remote refspecs options) = do
    repository <- requireRepository "."
    result <- fetchRepository repository remote refspecs options
    forM_ (fetchUpdatedRefs result) $ \(refPath, objectId) ->
        putStrLn (" * " ++ refPath ++ " -> " ++ objectIdToHex objectId)
    case fetchHeadObject result of
        Nothing -> putStrLn "Already up to date."
        Just _ -> pure ()

runCommand (PushCommand remote refspecs options) = do
    repository <- requireRepository "."
    result <- pushRepository repository remote refspecs options
    if null (pushReportedRefs result)
        then putStrLn "Everything up-to-date."
        else forM_ (pushReportedRefs result) $ \(refPath, status) ->
            putStrLn (" * " ++ refPath ++ " " ++ status)

runCommand (CheckoutCommand maybeNewBranch revision) = do
    repository <- requireRepository "."
    store <- loadStore repository
    let objectFormat = repositoryObjectFormat repository
    targetObject <- resolveObjectId repository store revision
    targetId <- peelToCommit objectFormat store targetObject
    ensureCleanWorktree repository store
    case maybeNewBranch of
        Just branchName -> do
            writeRefValue repository ("refs/heads/" ++ branchName) targetId
            writeHeadReference repository (symbolicHead ("refs/heads/" ++ branchName))
            checkoutTree repository store targetId
        Nothing -> do
            let branchCandidate =
                    if "refs/" `isPrefixOf` revision
                        then revision
                        else "refs/heads/" ++ revision
            branchValue <- readRefValue repository branchCandidate
            case branchValue of
                Just _ -> writeHeadReference repository (symbolicHead branchCandidate)
                Nothing -> writeHeadReference repository (DetachedHead targetId)
            checkoutTree repository store targetId

runCommand (SwitchCommand branchName) = do
    repository <- requireRepository "."
    store <- loadStore repository
    let refPath = "refs/heads/" ++ branchName
    branchValue <- readRefValue repository refPath
    case branchValue of
        Nothing -> ioError (userError ("unknown branch: " ++ branchName))
        Just targetId -> do
            ensureCleanWorktree repository store
            writeHeadReference repository (symbolicHead refPath)
            checkoutTree repository store targetId

runCommand (BranchCommand maybeName) = do
    repository <- requireRepository "."
    case maybeName of
        Just name -> createBranch repository name
        Nothing -> do
            refs <- listRefs repository
            forM_ [refPath | (refPath, _) <- refs, "refs/heads/" `isPrefixOf` refPath] $ \refPath ->
                putStrLn (drop (length "refs/heads/") refPath)

runCommand StatusCommand = do
    repository <- requireRepository "."
    store <- loadStore repository
    statusLines <- statusRepository repository store
    mapM_ putStrLn statusLines

runCommand (ResetCommand revision) = do
    repository <- requireRepository "."
    store <- loadStore repository
    targetObject <- resolveObjectId repository store revision
    targetId <- peelToCommit (repositoryObjectFormat repository) store targetObject
    resetHard repository store targetId

runCommand (MergeCommand action) = do
    repository <- requireRepository "."
    store <- loadStore repository
    case action of
        MergeAbort -> mergeAbort repository store
        MergeContinue -> do
            _ <- mergeContinue repository store ""
            pure ()
        MergeRevision revision -> do
            targetObject <- resolveObjectId repository store revision
            targetId <- peelToCommit (repositoryObjectFormat repository) store targetObject
            result <- mergeRepository repository store targetId
            case result of
                MergeUpToDate -> putStrLn "Already up to date."
                MergeFastForward objectId ->
                    putStrLn ("Fast-forward to " ++ objectIdToHex objectId)
                MergeCreated objectId ->
                    putStrLn ("Merge commit " ++ objectIdToHex objectId)
                MergeConflicted paths -> do
                    mapM_ (putStrLn . ("CONFLICT " ++)) paths
                    exitWith (ExitFailure 1)

runCommand (CommitCommand message) = do
    repository <- requireRepository "."
    store <- loadStore repository
    commitId <- commitCurrentIndex repository store message
    putStrLn (objectIdToHex commitId)

runCommand (PullCommand remote options) = do
    repository <- requireRepository "."
    result <- pullRepository repository remote options
    case result of
        MergeUpToDate -> putStrLn "Already up to date."
        MergeFastForward objectId -> putStrLn ("Fast-forward to " ++ objectIdToHex objectId)
        MergeCreated objectId -> putStrLn ("Merge commit " ++ objectIdToHex objectId)
        MergeConflicted paths -> do
            mapM_ (putStrLn . ("CONFLICT " ++)) paths
            exitWith (ExitFailure 1)

runCommand (PullRebaseCommand remote) = do
    repository <- requireRepository "."
    result <- pullRepositoryRebase repository remote defaultFetchOptions
    reportRebaseResult result

runCommand (RebaseCommand action) = do
    repository <- requireRepository "."
    store <- loadStore repository
    case action of
        RebaseAbort -> rebaseAbort repository store
        RebaseContinue -> do
            result <- rebaseContinue repository store
            reportRebaseResult result
        RebaseSkip -> do
            result <- rebaseSkip repository store
            reportRebaseResult result
        RebaseRevision revision -> do
            targetObject <- resolveObjectId repository store revision
            targetId <- peelToCommit (repositoryObjectFormat repository) store targetObject
            result <- rebaseRepository repository store targetId
            reportRebaseResult result

runCommand (SubmoduleCommand action) = do
    repository <- requireRepository "."
    store <- loadStore repository
    case action of
        SubmoduleStatus -> mapM_ putStrLn =<< submoduleStatus repository store
        SubmoduleUpdate recursive -> updateSubmodules repository store recursive

runCommand (RemoteCommand action) = do
    repository <- requireRepository "."
    case action of
        RemoteAdd name url -> do
            if null name || any (`elem` "\"\n\r") name
                then ioError (userError "remote name is not valid")
                else
                    appendFile
                        (repositoryConfigPath repository)
                        ( unlines
                            [ ""
                            , "[remote \"" ++ name ++ "\"]"
                            , "\turl = " ++ url
                            , "\tfetch = +refs/heads/*:refs/remotes/" ++ name ++ "/*"
                            ]
                        )
        RemoteSetUrl name url -> do
            config <- loadRepositoryConfig repository
            case configGet config ("remote." ++ name ++ ".url") of
                Nothing -> ioError (userError ("unknown remote: " ++ name))
                Just _ ->
                    appendFile
                        (repositoryConfigPath repository)
                        ("\n[remote \"" ++ name ++ "\"]\n\turl = " ++ url ++ "\n")
        RemoteGetUrl name -> do
            config <- loadRepositoryConfig repository
            case configGet config ("remote." ++ name ++ ".url") of
                Nothing -> ioError (userError ("unknown remote: " ++ name))
                Just url -> putStrLn url
        RemoteList -> do
            config <- loadRepositoryConfig repository
            forM_ (configEntries config) $ \entry ->
                case (configEntrySection entry, configEntrySubsection entry, configEntryKey entry) of
                    ("remote", Just name, "url") ->
                        putStrLn (name ++ "\t" ++ configEntryValue entry ++ " (fetch)")
                    _ -> pure ()

runCommand (VerifyCommand path) = do
    repository <- requireRepository path
    report <- verifyRepository repository
    forM_ (renderVerifyReport report) putStrLn
    when (not (null (verifyProblems report))) (exitWith (ExitFailure 1))

reportRebaseResult :: RebaseResult -> IO ()
reportRebaseResult result =
    case result of
        RebaseUpToDate -> putStrLn "Current branch is up to date."
        RebaseComplete objectId -> putStrLn ("Rebased to " ++ objectIdToHex objectId)
        RebaseConflicted paths -> do
            mapM_ (putStrLn . ("CONFLICT " ++)) paths
            exitWith (ExitFailure 1)


prettyObject :: HashAlgorithm -> GitObject -> IO ByteString
prettyObject objectFormat gitObjectValue =
    case gitObjectType gitObjectValue of
        TreeObject ->
            case treeFromObjectFor objectFormat gitObjectValue of
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

peelToTree :: HashAlgorithm -> Store -> ObjectId -> IO ObjectId
peelToTree objectFormat store objectId = do
    gitObjectValue <- readObjectOrFail store objectId
    case gitObjectType gitObjectValue of
        TreeObject -> pure objectId
        CommitObject ->
            case commitFromObjectFor objectFormat gitObjectValue of
                Left err -> ioError (userError err)
                Right currentCommit -> pure (commitTree currentCommit)
        TagObject ->
            case tagFromObjectFor objectFormat gitObjectValue of
                Left err -> ioError (userError err)
                Right currentTag -> peelToTree objectFormat store (tagObject currentTag)
        BlobObject -> ioError (userError "object is a blob, not a tree")

peelToCommit :: HashAlgorithm -> Store -> ObjectId -> IO ObjectId
peelToCommit objectFormat store objectId = do
    gitObjectValue <- readObjectOrFail store objectId
    case gitObjectType gitObjectValue of
        CommitObject -> pure objectId
        TagObject ->
            case tagFromObjectFor objectFormat gitObjectValue of
                Left err -> ioError (userError err)
                Right currentTag -> peelToCommit objectFormat store (tagObject currentTag)
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

ensureCleanWorktree :: Repository -> Store -> IO ()
ensureCleanWorktree repository store = do
    statusLines <- statusRepository repository store
    let trackedChanges = [line | line <- statusLines, not ("?? " `isPrefixOf` line)]
    when (not (null trackedChanges)) $
        ioError (userError "local changes would be overwritten; commit or reset them first")

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

readIndexFile :: HashAlgorithm -> FilePath -> IO [IndexEntry]
readIndexFile objectFormat path = do
    indexExists <- doesFileExist path
    if not indexExists
        then pure []
        else do
            indexBytes <- BS.readFile path
            either
                (ioError . userError . (("index " ++ path ++ ": ") ++))
                pure
                (parseIndexFor objectFormat indexBytes)

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
                , indexEntryStage = 0
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
versionText = "0.2.0"

defaultBranchName :: String
defaultBranchName = "main"
