module Git.Object.Commit
    ( Commit
    , PersonIdent
    , commit
    , commitAuthor
    , commitCommitter
    , commitFromObject
    , commitMessage
    , commitParents
    , commitToObject
    , commitTree
    , decodeCommit
    , encodeCommit
    , personIdent
    , personIdentEmail
    , personIdentName
    , personIdentTimestamp
    , personIdentTimezone
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Hash (ObjectId, objectIdFromHex, objectIdToHex)
import Git.Object (GitObject, GitObjectType (CommitObject), gitObject, gitObjectPayload, gitObjectType)
import Text.Read (readMaybe)

data PersonIdent = PersonIdent
    { personIdentName :: ByteString
    , personIdentEmail :: ByteString
    , personIdentTimestamp :: Integer
    , personIdentTimezone :: ByteString
    }
    deriving (Eq, Show)

data Commit = Commit
    { commitTree :: ObjectId
    , commitParents :: [ObjectId]
    , commitAuthor :: PersonIdent
    , commitCommitter :: PersonIdent
    , commitMessage :: ByteString
    }
    deriving (Eq, Show)

personIdent :: ByteString -> ByteString -> Integer -> ByteString -> PersonIdent
personIdent = PersonIdent

commit :: ObjectId -> [ObjectId] -> PersonIdent -> PersonIdent -> ByteString -> Commit
commit = Commit

encodeCommit :: Commit -> ByteString
encodeCommit currentCommit =
    BS.intercalate
        (BS8.pack "\n")
        ( [ BS8.pack ("tree " ++ objectIdToHex (commitTree currentCommit))
          ]
            ++ map (\parentId -> BS8.pack ("parent " ++ objectIdToHex parentId)) (commitParents currentCommit)
            ++ [ BS8.pack "author " <> renderPersonIdent (commitAuthor currentCommit)
               , BS8.pack "committer " <> renderPersonIdent (commitCommitter currentCommit)
               , BS.empty
               , commitMessage currentCommit
               ]
        )

decodeCommit :: ByteString -> Either String Commit
decodeCommit rawCommit = do
    let (headerPart, messagePartWithSeparator) = BS8.breakSubstring (BS8.pack "\n\n") rawCommit
    if BS.null messagePartWithSeparator
        then Left "Malformed commit: missing header/message separator"
        else do
            let messagePart = BS.drop 2 messagePartWithSeparator
                headerLines = BS8.split '\n' headerPart
            treeId <- parseRequiredObjectId "tree" headerLines
            parentIds <- traverse parseParentLine (filter (hasPrefix "parent") headerLines)
            authorIdent <- parseRequiredPerson "author" headerLines
            committerIdent <- parseRequiredPerson "committer" headerLines
            Right (commit treeId parentIds authorIdent committerIdent messagePart)
  where
    hasPrefix prefix = BS8.isPrefixOf (BS8.pack (prefix ++ " "))

    parseRequiredObjectId prefix headerLines =
        case filter (hasPrefix prefix) headerLines of
            [line] -> parseObjectIdLine prefix line
            [] -> Left ("Malformed commit: missing " ++ prefix ++ " line")
            _ -> Left ("Malformed commit: duplicate " ++ prefix ++ " line")

    parseRequiredPerson prefix headerLines =
        case filter (hasPrefix prefix) headerLines of
            [line] -> parsePersonLine prefix line
            [] -> Left ("Malformed commit: missing " ++ prefix ++ " line")
            _ -> Left ("Malformed commit: duplicate " ++ prefix ++ " line")

    parseParentLine = parseObjectIdLine "parent"

parseObjectIdLine :: String -> ByteString -> Either String ObjectId
parseObjectIdLine prefix line =
    case objectIdFromHex (BS8.unpack (BS.drop (length prefix + 1) line)) of
        Nothing -> Left ("Malformed commit: invalid " ++ prefix ++ " object id")
        Just objectId -> Right objectId

parsePersonLine :: String -> ByteString -> Either String PersonIdent
parsePersonLine prefix line = do
    let payload = BS.drop (length prefix + 1) line
        (namePart, afterName) = BS8.breakSubstring (BS8.pack " <") payload
    if BS.null afterName
        then Left ("Malformed commit: invalid " ++ prefix ++ " identity")
        else do
            let emailAndRest = BS.drop 2 afterName
                (emailPart, afterEmail) = BS8.break (== '>') emailAndRest
            if BS.null afterEmail
                then Left ("Malformed commit: invalid " ++ prefix ++ " email")
                else do
                    let restWords = BS8.words (BS.drop 1 afterEmail)
                    case restWords of
                        [timestampText, timezoneText] ->
                            case readMaybe (BS8.unpack timestampText) of
                                Nothing -> Left ("Malformed commit: invalid " ++ prefix ++ " timestamp")
                                Just timestamp ->
                                    Right (personIdent namePart emailPart timestamp timezoneText)
                        _ -> Left ("Malformed commit: invalid " ++ prefix ++ " metadata")

commitToObject :: Commit -> GitObject
commitToObject = gitObject CommitObject . encodeCommit

commitFromObject :: GitObject -> Either String Commit
commitFromObject currentObject =
    if gitObjectType currentObject == CommitObject
        then decodeCommit (gitObjectPayload currentObject)
        else Left "Expected a commit object"

renderPersonIdent :: PersonIdent -> ByteString
renderPersonIdent ident =
    personIdentName ident
        <> BS8.pack " <"
        <> personIdentEmail ident
        <> BS8.pack "> "
        <> BS8.pack (show (personIdentTimestamp ident))
        <> BS8.pack " "
        <> personIdentTimezone ident
