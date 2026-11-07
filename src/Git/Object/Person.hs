module Git.Object.Person
    ( PersonIdent (..)
    , parsePersonIdent
    , personIdent
    , renderPersonIdent
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- | An author, committer, or tagger line: name, email, unix time, and the
-- timezone offset as text.
data PersonIdent = PersonIdent
    { personIdentName :: ByteString
    , personIdentEmail :: ByteString
    , personIdentTimestamp :: Integer
    , personIdentTimezone :: ByteString
    }
    deriving (Eq, Show)

personIdent :: ByteString -> ByteString -> Integer -> ByteString -> PersonIdent
personIdent = PersonIdent

renderPersonIdent :: PersonIdent -> ByteString
renderPersonIdent ident =
    personIdentName ident
        <> BS8.pack " <"
        <> personIdentEmail ident
        <> BS8.pack "> "
        <> BS8.pack (show (personIdentTimestamp ident))
        <> BS8.singleton ' '
        <> personIdentTimezone ident

-- | Parse everything after the label of an author, committer, or tagger line.
-- The timezone must be a sign and four digits, and the timestamp plain digits;
-- git writes both in that form and rejects anything else.
parsePersonIdent :: ByteString -> Either String PersonIdent
parsePersonIdent payload = do
    let (namePart, afterName) = BS8.breakSubstring (BS8.pack " <") payload
    if BS.null afterName
        then Left "identity is missing its email address"
        else do
            let emailAndRest = BS.drop 2 afterName
                (emailPart, afterEmail) = BS8.break (== '>') emailAndRest
            if BS.null afterEmail
                then Left "identity is missing its closing angle bracket"
                else parseTimestamp namePart emailPart (BS.drop 1 afterEmail)
  where
    parseTimestamp namePart emailPart rest =
        case BS8.words rest of
            [timestampText, timezoneText] -> do
                timestamp <- parseTimestampText timestampText
                validateTimezone timezoneText
                Right (PersonIdent namePart emailPart timestamp timezoneText)
            _ -> Left "identity is missing its timestamp or timezone"

    parseTimestampText text
        | BS.null text = Left "identity has an empty timestamp"
        | not (BS8.all isDigit text) = Left "identity has a non-numeric timestamp"
        | otherwise =
            maybe
                (Left "identity timestamp does not fit an integer")
                Right
                (readMaybe (BS8.unpack text))

    validateTimezone timezone
        | BS.length timezone /= timezoneLength = Left "identity has a malformed timezone"
        | BS8.head timezone `notElem` ['+', '-'] = Left "identity timezone is missing its sign"
        | not (BS8.all isDigit (BS.drop 1 timezone)) = Left "identity timezone is not numeric"
        | otherwise = Right ()

timezoneLength :: Int
timezoneLength = 5
