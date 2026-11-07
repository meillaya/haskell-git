module Git.RefSpec
    ( RefSpec (..)
    , applyRefSpec
    , parseRefSpec
    , refSpecMatches
    ) where

import Data.List (isPrefixOf, isSuffixOf)

data RefSpec = RefSpec
    { refSpecSource :: String
    , refSpecDestination :: Maybe String
    , refSpecForce :: Bool
    }
    deriving (Eq, Show)

-- | Parse a refspec such as @+refs/heads/*:refs/remotes/origin/*@ or
-- @main:refs/heads/main@. An empty destination means update FETCH_HEAD only.
parseRefSpec :: String -> Either String RefSpec
parseRefSpec rawSpec
    | null spec = Left "empty refspec"
    | otherwise = do
        let (force, withoutPlus) = case spec of
                '+' : rest -> (True, rest)
                _ -> (False, spec)
            (source, destinationWithColon) = break (== ':') withoutPlus
        if null source
            then Left ("refspec has an empty source: " ++ spec)
            else do
                destination <-
                    case destinationWithColon of
                        [] -> Right Nothing
                        ':' : destinationText ->
                            if null destinationText
                                then Left ("refspec has an empty destination: " ++ spec)
                                else Right (Just destinationText)
                        _ -> Left ("malformed refspec: " ++ spec)
                validateWildcards source destination
                Right
                    RefSpec
                        { refSpecSource = source
                        , refSpecDestination = destination
                        , refSpecForce = force
                        }
  where
    spec = trim rawSpec

validateWildcards :: String -> Maybe String -> Either String ()
validateWildcards source destination
    | countWildcards source > 1 = Left "refspec source has more than one wildcard"
    | Just target <- destination
    , countWildcards target > 1 =
        Left "refspec destination has more than one wildcard"
    | hasWildcard source
    , Just target <- destination
    , not (hasWildcard target) =
        Left "refspec source has a wildcard but the destination does not"
    | otherwise = Right ()

-- | Match a remote ref name against a refspec source and produce the local
-- destination name.
applyRefSpec :: RefSpec -> String -> Maybe String
applyRefSpec refSpec remoteRef
    | not (refSpecMatches refSpec remoteRef) = Nothing
    | otherwise =
        case refSpecDestination refSpec of
            Nothing -> Nothing
            Just destination ->
                case break (== '*') (refSpecSource refSpec) of
                    (prefix, '*' : suffix) ->
                        let matched =
                                take (length remoteRef - length prefix - length suffix)
                                    (drop (length prefix) remoteRef)
                         in Just (replaceWildcard destination matched)
                    _ -> Just destination

refSpecMatches :: RefSpec -> String -> Bool
refSpecMatches refSpec refName =
    case break (== '*') (refSpecSource refSpec) of
        (prefix, '*' : suffix) ->
            prefix `isPrefixOf` refName
                && suffix `isSuffixOf` refName
                && length refName >= length prefix + length suffix
        (exact, _) -> exact == refName

hasWildcard :: String -> Bool
hasWildcard = elem '*'

countWildcards :: String -> Int
countWildcards = length . filter (== '*')

replaceWildcard :: String -> String -> String
replaceWildcard patternText replacement =
    case break (== '*') patternText of
        (prefix, '*' : suffix) -> prefix ++ replacement ++ suffix
        _ -> patternText

trim :: String -> String
trim = reverse . dropWhile isSpaceCharacter . reverse . dropWhile isSpaceCharacter

isSpaceCharacter :: Char -> Bool
isSpaceCharacter character = character == ' ' || character == '\t'
