module Git.Ignore
    ( IgnoreRule (..)
    , isIgnored
    , loadIgnoreRules
    ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, sortOn)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import System.FilePath ((</>))
import Git.Repo (Repository (..), repositoryGitDir, repositoryRootDir)

data IgnoreRule = IgnoreRule
    { ignorePattern :: String
    , ignoreNegated :: Bool
    , ignoreDirectoryOnly :: Bool
    , ignoreAnchored :: Bool
    , ignoreBaseDir :: FilePath
    }
    deriving (Eq, Show)

-- | Load every .gitignore under the worktree plus .git/info/exclude. Deeper
-- rules are returned later so they take precedence, matching git.
loadIgnoreRules :: Repository -> IO [IgnoreRule]
loadIgnoreRules repository = do
    ignoreFiles <- findIgnoreFiles (repositoryRootDir repository) ""
    rules <- concat <$> traverse (uncurry readRules) ignoreFiles
    excludeRules <-
        readRules "" (repositoryGitDir repository </> "info" </> "exclude")
    pure (sortOn (length . splitComponents . ignoreBaseDir) (rules ++ excludeRules))

findIgnoreFiles :: FilePath -> FilePath -> IO [(FilePath, FilePath)]
findIgnoreFiles root relativeDirectory = do
    let absoluteDirectory =
            if null relativeDirectory
                then root
                else root </> relativeDirectory
    names <- listDirectory absoluteDirectory
    fmap concat $
        traverse
            (\name -> do
                let relativePath =
                        if null relativeDirectory
                            then name
                            else relativeDirectory </> name
                    absolutePath = root </> relativePath
                isDirectory <- doesDirectoryExist absolutePath
                if name == ".git"
                    then pure []
                    else
                        if isDirectory
                            then findIgnoreFiles root relativePath
                            else
                                if name == ".gitignore"
                                    then pure [(relativeDirectory, absolutePath)]
                                    else pure []
            )
            names

readRules :: FilePath -> FilePath -> IO [IgnoreRule]
readRules baseDirectory path = do
    exists <- doesFileExist path
    if not exists
        then pure []
        else concatMap (mapMaybeRule baseDirectory) . lines <$> readFile path

mapMaybeRule :: FilePath -> String -> [IgnoreRule]
mapMaybeRule baseDirectory line =
    case parseRule baseDirectory line of
        Nothing -> []
        Just rule -> [rule]

parseRule :: FilePath -> String -> Maybe IgnoreRule
parseRule baseDirectory raw
    | null stripped = Nothing
    | "#" `isPrefixOf` stripped = Nothing
    | otherwise =
        let (negated, withoutBang) =
                case stripped of
                    '!' : rest -> (True, rest)
                    _ -> (False, stripped)
            directoryOnly = "/" `isSuffixOfString` withoutBang
            withoutSlash = dropTrailingSlash withoutBang
            anchored = "/" `isPrefixOf` withoutSlash
            patternText = dropLeadingSlash withoutSlash
         in if null patternText
                then Nothing
                else
                    Just
                        IgnoreRule
                            { ignorePattern = patternText
                            , ignoreNegated = negated
                            , ignoreDirectoryOnly = directoryOnly
                            , ignoreAnchored = anchored
                            , ignoreBaseDir = baseDirectory
                            }
  where
    stripped = trim raw
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    dropTrailingSlash value
        | "/" `isSuffixOfString` value = init value
        | otherwise = value
    dropLeadingSlash value
        | "/" `isPrefixOf` value = tail value
        | otherwise = value

-- | Return True when the last matching rule ignores the path. Rules apply to
-- files and directories; a match on a parent directory ignores its contents.
isIgnored :: [IgnoreRule] -> FilePath -> Bool
isIgnored rules path =
    case foldl step Nothing rules of
        Just ignored -> ignored
        Nothing -> False
  where
    step current rule =
        if ruleMatches rule path
            then Just (not (ignoreNegated rule))
            else current

ruleMatches :: IgnoreRule -> FilePath -> Bool
ruleMatches rule path =
    case relativeToBase (ignoreBaseDir rule) path of
        Nothing -> False
        Just relativePath
            | ignoreDirectoryOnly rule ->
                any (matchPattern rule) (parentPrefixes relativePath)
            | ignoreAnchored rule ->
                matchPattern rule relativePath
                    || any (matchPattern rule) (parentPrefixes relativePath)
            | '/' `elem` ignorePattern rule ->
                matchPattern rule relativePath
                    || any (matchPattern rule) (parentPrefixes relativePath)
            | otherwise ->
                any (matchPattern rule) (componentCandidates relativePath)
  where
    componentCandidates value =
        splitComponents value ++ parentPrefixes value
    matchPattern rule candidate = globMatch (ignorePattern rule) candidate

relativeToBase :: FilePath -> FilePath -> Maybe FilePath
relativeToBase baseDirectory path
    | null baseDirectory = Just path
    | path == baseDirectory = Just ""
    | (baseDirectory ++ "/") `isPrefixOf` path =
        Just (drop (length baseDirectory + 1) path)
    | otherwise = Nothing

parentPrefixes :: FilePath -> [FilePath]
parentPrefixes path =
    case splitComponents path of
        [] -> []
        components -> init (scanl1 joinComponent components)
  where
    joinComponent left right =
        if null left then right else left ++ "/" ++ right

splitComponents :: FilePath -> [String]
splitComponents path =
    case break (== '/') path of
        (before, []) -> [before | not (null before)]
        (before, _ : after) ->
            [before | not (null before)] ++ splitComponents after

globMatch :: String -> String -> Bool
globMatch patternText text = go patternText text
  where
    go [] [] = True
    go [] _ = False
    go ('*' : '*' : rest) value =
        go rest value || case value of
            [] -> False
            (_ : tailValue) -> go ('*' : '*' : rest) tailValue
    go ('*' : rest) value =
        go rest value || case value of
            [] -> False
            (character : tailValue)
                | character /= '/' -> go ('*' : rest) tailValue
                | otherwise -> False
    go ('?' : rest) value =
        case value of
            (character : tailValue) -> character /= '/' && go rest tailValue
            [] -> False
    go (expected : rest) value =
        case value of
            (actual : tailValue) -> expected == actual && go rest tailValue
            [] -> False

isSuffixOfString :: String -> String -> Bool
isSuffixOfString suffix text = reverse suffix `isPrefixOf` reverse text
