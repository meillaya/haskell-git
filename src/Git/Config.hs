module Git.Config
    ( Config
    , ConfigEntry (..)
    , configEntries
    , configGet
    , configGetAll
    , configGetIn
    , configGetAllIn
    , loadGlobalConfig
    , loadRepositoryConfig
    , parseConfig
    , readConfigFile
    ) where

import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Git.Repo (Repository, repositoryConfigPath)

newtype Config = Config [ConfigEntry]
    deriving (Eq, Show)

data ConfigEntry = ConfigEntry
    { configEntrySection :: String
    , configEntrySubsection :: Maybe String
    , configEntryKey :: String
    , configEntryValue :: String
    }
    deriving (Eq, Show)

configEntries :: Config -> [ConfigEntry]
configEntries (Config entries) = entries

-- | Read a config file. A missing file is an empty config.
readConfigFile :: FilePath -> IO Config
readConfigFile path = do
    exists <- doesFileExist path
    if exists
        then parseConfig <$> readFile path
        else pure (Config [])

loadRepositoryConfig :: Repository -> IO Config
loadRepositoryConfig repository = readConfigFile (repositoryConfigPath repository)

loadGlobalConfig :: IO Config
loadGlobalConfig = do
    home <- getHomeDirectory
    readConfigFile (home </> ".gitconfig")

-- | Look up the last value for a key such as @remote.origin.url@,
-- @credential.helper@, or @http.extraHeader@. The section and key are
-- case-insensitive; a subsection is kept as written.
configGet :: Config -> String -> Maybe String
configGet config key =
    case reverse (configGetAll config key) of
        value : _ -> Just value
        [] -> Nothing

configGetAll :: Config -> String -> [String]
configGetAll (Config entries) key =
    [ configEntryValue entry
    | entry <- entries
    , matchesKey key entry
    ]

configGetIn :: Config -> String -> Maybe String -> String -> Maybe String
configGetIn (Config entries) section subsection key =
    case
        [ configEntryValue entry
        | entry <- entries
        , configEntrySection entry == normalize section
        , normalizeMaybe (configEntrySubsection entry) == normalizeMaybe subsection
        , configEntryKey entry == normalize key
        ] of
        value : _ -> Just value
        [] -> Nothing

configGetAllIn :: Config -> String -> Maybe String -> String -> [String]
configGetAllIn (Config entries) section subsection key =
    [ configEntryValue entry
    | entry <- entries
    , configEntrySection entry == normalize section
    , normalizeMaybe (configEntrySubsection entry) == normalizeMaybe subsection
    , configEntryKey entry == normalize key
    ]

matchesKey :: String -> ConfigEntry -> Bool
matchesKey key entry =
    case splitKey key of
        (section, Nothing, name) ->
            normalize section == configEntrySection entry
                && configEntrySubsection entry == Nothing
                && normalize name == configEntryKey entry
        (section, Just subsection, name) ->
            normalize section == configEntrySection entry
                && configEntrySubsection entry == Just subsection
                && normalize name == configEntryKey entry

-- | Split @section[.subsection].key@ at the first and last dots. A
-- subsection may itself contain dots.
splitKey :: String -> (String, Maybe String, String)
splitKey key =
    case break (== '.') key of
        (section, '.' : rest) ->
            case breakLastDot rest of
                (subsection, name) | null subsection -> (section, Nothing, name)
                (subsection, name) -> (section, Just subsection, name)
        _ -> (key, Nothing, key)

breakLastDot :: String -> (String, String)
breakLastDot text =
    case break (== '.') (reverse text) of
        (nameReversed, '.' : rest) -> (reverse rest, reverse nameReversed)
        _ -> ("", text)

parseConfig :: String -> Config
parseConfig contents = Config (go "" (lines contents))
  where
    go _ [] = []
    go currentSection (line : rest) =
        let stripped = trim line
         in if null stripped || isComment stripped
                then go currentSection rest
                else
                    if isPrefixOf "[" stripped
                        then
                            case parseSection stripped of
                                Just section -> go section rest
                                Nothing -> go currentSection rest
                        else case break (== '=') stripped of
                            (keyPart, '=' : valuePart)
                                | not (null (trim keyPart)) ->
                                    let (section, subsection) = splitSection currentSection
                                        entry =
                                            ConfigEntry
                                                { configEntrySection = normalize section
                                                , configEntrySubsection = subsection
                                                , configEntryKey = normalize (trim keyPart)
                                                , configEntryValue = unquote (trim valuePart)
                                                }
                                     in entry : go currentSection rest
                            _ -> go currentSection rest

    isComment text = case text of
        firstCharacter : _ -> firstCharacter `elem` "#;"
        [] -> False

parseSection :: String -> Maybe String
parseSection text =
    case stripBrackets text of
        Nothing -> Nothing
        Just inside ->
            case breakOnQuote inside of
                (section, Nothing) ->
                    if null (trim section) then Nothing else Just (trim section)
                (section, Just (subsection, remainder)) ->
                    if not (null (trim remainder)) || null (trim section)
                        then Nothing
                        else Just (trim section ++ ".\"" ++ subsection ++ "\"")

stripBrackets :: String -> Maybe String
stripBrackets text =
    case dropWhile isSpace text of
        '[' : rest ->
            case reverse rest of
                ']' : reversedInside -> Just (reverse reversedInside)
                _ -> Nothing
        _ -> Nothing

-- | Parse a quoted subsection after the section name. Returns the section
-- text before the quote, the unescaped subsection, and any text after the
-- closing quote.
breakOnQuote :: String -> (String, Maybe (String, String))
breakOnQuote text =
    case break (== '"') text of
        (before, '"' : rest) ->
            let (subsection, remainder) = consumeQuoted rest []
             in (before, Just (subsection, remainder))
        _ -> (text, Nothing)

consumeQuoted :: String -> String -> (String, String)
consumeQuoted [] accumulated = (reverse accumulated, "")
consumeQuoted ('"' : rest) accumulated = (reverse accumulated, rest)
consumeQuoted ('\\' : character : rest) accumulated =
    consumeQuoted rest (unescape character : accumulated)
consumeQuoted (character : rest) accumulated = consumeQuoted rest (character : accumulated)

unquote :: String -> String
unquote text =
    case text of
        '"' : rest ->
            let (value, remainder) = consumeQuoted rest []
             in if null (trim remainder) then value else text
        _ -> text

unescape :: Char -> Char
unescape 'n' = '\n'
unescape 't' = '\t'
unescape '\\' = '\\'
unescape '"' = '"'
unescape character = character

splitSection :: String -> (String, Maybe String)
splitSection section =
    case break (== '.') section of
        (name, '.' : rest) ->
            case rest of
                '"' : restOfSubsection ->
                    case reverse restOfSubsection of
                        '"' : reversedSubsection -> (name, Just (reverse reversedSubsection))
                        _ -> (name, Nothing)
                _ -> (name, Just rest)
        _ -> (section, Nothing)

normalize :: String -> String
normalize = map toLower

normalizeMaybe :: Maybe String -> Maybe String
normalizeMaybe = fmap normalize

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
