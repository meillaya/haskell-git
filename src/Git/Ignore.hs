module Git.Ignore
    ( IgnoreRule (..)
    , isIgnored
    , loadIgnoreRules
    ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Git.Repo (Repository (..), repositoryGitDir, repositoryRootDir)

data IgnoreRule = IgnoreRule
    { ignorePattern :: String
    , ignoreNegated :: Bool
    , ignoreDirectoryOnly :: Bool
    , ignoreAnchored :: Bool
    }
    deriving (Eq, Show)

-- | Load the root .gitignore and .git/info/exclude. Nested .gitignore files
-- are not yet consulted.
loadIgnoreRules :: Repository -> IO [IgnoreRule]
loadIgnoreRules repository = do
    rootRules <- readRules (repositoryRootDir repository </> ".gitignore")
    excludeRules <-
        readRules (repositoryGitDir repository </> "info" </> "exclude")
    pure (rootRules ++ excludeRules)

readRules :: FilePath -> IO [IgnoreRule]
readRules path = do
    exists <- doesFileExist path
    if not exists
        then pure []
        else concatMap mapMaybeRule . lines <$> readFile path

mapMaybeRule :: String -> [IgnoreRule]
mapMaybeRule line =
    case parseRule line of
        Nothing -> []
        Just rule -> [rule]

parseRule :: String -> Maybe IgnoreRule
parseRule raw
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
ruleMatches rule path
    | ignoreDirectoryOnly rule =
        any (matchPattern rule) (parentPrefixes path)
    | ignoreAnchored rule =
        matchPattern rule path || any (matchPattern rule) (parentPrefixes path)
    | '/' `elem` ignorePattern rule =
        matchPattern rule path || any (matchPattern rule) (parentPrefixes path)
    | otherwise =
        any (matchPattern rule) (componentCandidates path)
  where
    componentCandidates value =
        splitComponents value ++ parentPrefixes value
    matchPattern rule candidate = globMatch (ignorePattern rule) candidate

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
