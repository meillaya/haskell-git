module TestSupport
    ( assert
    , assertContains
    , assertEqual
    , withTempDirectory
    ) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory
    ( createDirectory,
      doesPathExist,
      getTemporaryDirectory,
      removePathForcibly,
      removeFile )
import System.Exit (die)
import System.IO (hClose, openTempFile)

assert :: Bool -> String -> IO ()
assert condition message =
    unless condition (die ("Test failure: " ++ message))

assertContains :: String -> String -> String -> IO ()
assertContains needle haystack message =
    unless (needle `isInfixOf` haystack) $
        die
            ( "Test failure: "
                ++ message
                ++ "\n  expected substring: "
                ++ show needle
                ++ "\n  actual: "
                ++ show haystack
            )

assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual expected actual message =
    unless (expected == actual) $
        die
            ( "Test failure: "
                ++ message
                ++ "\n  expected: "
                ++ show expected
                ++ "\n  actual: "
                ++ show actual
            )

withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory prefix action = do
    temporaryRoot <- getTemporaryDirectory
    (placeholderPath, handle) <- openTempFile temporaryRoot (prefix ++ "-placeholder")
    hClose handle
    removeFile placeholderPath
    createDirectory placeholderPath
    bracket
        (pure placeholderPath)
        cleanupTempDirectory
        action

cleanupTempDirectory :: FilePath -> IO ()
cleanupTempDirectory directoryPath = do
    exists <- doesPathExist directoryPath
    if exists
        then removePathForcibly directoryPath
        else pure ()
