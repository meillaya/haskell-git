module Git.DiffSpec
    ( run
    ) where

import qualified Data.ByteString.Char8 as BS8
import Git.Diff (mergeTextContent)
import TestSupport (assertEqual)

run :: IO ()
run = do
    combinesDifferentLines
    rejectsOverlappingLines
    preservesMissingFinalNewline

combinesDifferentLines :: IO ()
combinesDifferentLines =
    assertEqual
        (Right (BS8.pack "one changed\ntwo\nthree changed\n"))
        ( mergeTextContent
            (BS8.pack "one\ntwo\nthree\n")
            (BS8.pack "one changed\ntwo\nthree\n")
            (BS8.pack "one\ntwo\nthree changed\n")
        )
        "Non-overlapping line edits should merge cleanly"

rejectsOverlappingLines :: IO ()
rejectsOverlappingLines =
    case
        mergeTextContent
            (BS8.pack "one\ntwo\nthree\n")
            (BS8.pack "one ours\nthree\n")
            (BS8.pack "one theirs\nthree\n") of
        Left () -> pure ()
        Right merged -> fail ("overlapping edits should conflict, got " ++ show merged)

preservesMissingFinalNewline :: IO ()
preservesMissingFinalNewline =
    assertEqual
        (Right (BS8.pack "one\ntwo changed"))
        ( mergeTextContent
            (BS8.pack "one\ntwo")
            (BS8.pack "one\ntwo changed")
            (BS8.pack "one\ntwo")
        )
        "A file without a final newline should stay that way"
