module Git.Object.Commit
    ( Commit
    , commit
    , commitAuthor
    , commitCommitter
    , commitFromObject
    , commitFromObjectFor
    , commitMessage
    , commitParents
    , commitToObject
    , commitTree
    , decodeCommit
    , decodeCommitFor
    , encodeCommit
    , module Git.Object.Person
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Git.Hash (HashAlgorithm (..), ObjectId, objectIdFromHexFor, objectIdToHex)
import Git.Object (GitObject, GitObjectType (CommitObject), gitObject, gitObjectPayload, gitObjectType)
import Git.Object.Person

data Commit = Commit
    { commitTree :: ObjectId
    , commitParents :: [ObjectId]
    , commitAuthor :: PersonIdent
    , commitCommitter :: PersonIdent
    , commitMessage :: ByteString
    }
    deriving (Eq, Show)

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

-- | Decode the headers this library reads. Signature, encoding, and merge tag
-- headers are skipped here on purpose: callers that need a byte-exact object
-- read the raw payload instead of round tripping through this type.
decodeCommit :: ByteString -> Either String Commit
decodeCommit = decodeCommitFor Sha1

decodeCommitFor :: HashAlgorithm -> ByteString -> Either String Commit
decodeCommitFor hashAlgorithm rawCommit = do
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
            [line] -> parseObjectIdLine hashAlgorithm prefix line
            [] -> Left ("Malformed commit: missing " ++ prefix ++ " line")
            _ -> Left ("Malformed commit: duplicate " ++ prefix ++ " line")

    parseRequiredPerson prefix headerLines =
        case filter (hasPrefix prefix) headerLines of
            [line] -> parsePersonLine prefix line
            [] -> Left ("Malformed commit: missing " ++ prefix ++ " line")
            _ -> Left ("Malformed commit: duplicate " ++ prefix ++ " line")

    parseParentLine = parseObjectIdLine hashAlgorithm "parent"

parseObjectIdLine :: HashAlgorithm -> String -> ByteString -> Either String ObjectId
parseObjectIdLine hashAlgorithm prefix line =
    case objectIdFromHexFor hashAlgorithm (BS8.unpack (BS.drop (length prefix + 1) line)) of
        Nothing -> Left ("Malformed commit: invalid " ++ prefix ++ " object id")
        Just objectId -> Right objectId

parsePersonLine :: String -> ByteString -> Either String PersonIdent
parsePersonLine prefix line =
    case parsePersonIdent (BS.drop (length prefix + 1) line) of
        Left err -> Left ("Malformed commit: " ++ err)
        Right ident -> Right ident

commitToObject :: Commit -> GitObject
commitToObject = gitObject CommitObject . encodeCommit

commitFromObject :: GitObject -> Either String Commit
commitFromObject = commitFromObjectFor Sha1

commitFromObjectFor :: HashAlgorithm -> GitObject -> Either String Commit
commitFromObjectFor hashAlgorithm currentObject =
    if gitObjectType currentObject == CommitObject
        then decodeCommitFor hashAlgorithm (gitObjectPayload currentObject)
        else Left "Expected a commit object"
