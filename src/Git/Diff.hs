{-# LANGUAGE ScopedTypeVariables #-}

module Git.Diff
    ( Hunk (..)
    , contentSimilarity
    , diffHunks
    , mergeTextContent
    ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, listArray, (!))
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

-- | One replacement in a line diff. The base range is half-open, and the
-- replacement is the list of target lines that takes its place.
data Hunk = Hunk
    { hunkBaseStart :: Int
    , hunkBaseEnd :: Int
    , hunkReplacement :: [ByteString]
    }
    deriving (Eq, Show)

-- | A rough text similarity in [0,1], used for rename detection. It combines
-- unchanged-line ratio with common byte prefix/suffix, which handles both
-- large files and small one-line edits.
contentSimilarity :: ByteString -> ByteString -> Double
contentSimilarity left right
    | BS.null left && BS.null right = 1
    | BS.elem 0 (BS.take 8000 left) || BS.elem 0 (BS.take 8000 right) = 0
    | otherwise = max lineRatio byteRatio
  where
    maxLength = max (BS.length left) (BS.length right)
    commonPrefix = length (takeWhile id (BS.zipWith (==) left right))
    commonSuffix =
        length
            ( takeWhile id
                ( BS.zipWith
                    (==)
                    (BS.reverse left)
                    (BS.reverse right)
                )
            )
    byteRatio =
        if maxLength == 0
            then 1
            else
                fromIntegral (min maxLength (commonPrefix + commonSuffix))
                    / fromIntegral maxLength
    (leftLines, _) = splitLines left
    (rightLines, _) = splitLines right
    lineRatio =
        case diffHunks leftLines rightLines of
            Nothing -> 0
            Just hunks ->
                let baseLength = length leftLines
                    targetLength = length rightLines
                    denominator = max baseLength targetLength
                    changed = sum [hunkBaseEnd hunk - hunkBaseStart hunk | hunk <- hunks]
                    kept = max 0 (baseLength - changed)
                 in if denominator == 0
                        then 1
                        else fromIntegral kept / fromIntegral denominator

-- | Line-level three-way merge. Two changed regions may be combined when they
-- touch different base lines; overlapping edits conflict.
mergeTextContent :: ByteString -> ByteString -> ByteString -> Either () ByteString
mergeTextContent base ours theirs
    | ours == base = Right theirs
    | theirs == base = Right ours
    | ours == theirs = Right ours
    | isBinary base || isBinary ours || isBinary theirs = Left ()
    | otherwise = do
        let (baseLines, baseNewline) = splitLines base
            (ourLines, ourNewline) = splitLines ours
            (theirLines, theirNewline) = splitLines theirs
        ourHunks <- maybe (Left ()) Right (diffHunks baseLines ourLines)
        theirHunks <- maybe (Left ()) Right (diffHunks baseLines theirLines)
        mergedHunks <- mergeHunks (deduplicate (ourHunks ++ theirHunks))
        let mergedLines = applyHunks baseLines mergedHunks
            newline = ourNewline || theirNewline || baseNewline
        Right (joinLines mergedLines newline)
  where
    isBinary bytes = BS.elem 0 (BS.take 8000 bytes)

-- | The shortest edit script as replacement hunks. 'Nothing' means the inputs
-- are too large for the dynamic-programming table; callers treat that as a
-- conflict rather than allocating unbounded memory.
diffHunks :: [ByteString] -> [ByteString] -> Maybe [Hunk]
diffHunks baseLines targetLines
    | n == 0 && m == 0 = Just []
    | n == 0 = Just [Hunk 0 0 targetLines]
    | m == 0 = Just [Hunk 0 n []]
    | n * m > maximumDiffCells = Nothing
    | otherwise = Just (runST (buildDiff n m))
  where
    n = length baseLines
    m = length targetLines
    baseArray = listArray (0, n - 1) baseLines :: Array Int ByteString
    targetArray = listArray (0, m - 1) targetLines :: Array Int ByteString

    buildDiff :: forall s. Int -> Int -> ST s [Hunk]
    buildDiff n m = do
        lengths <- newArray ((0, 0), (n, m)) 0 :: ST s (STUArray s (Int, Int) Int)
        directions <- newArray ((0, 0), (n, m)) 0 :: ST s (STUArray s (Int, Int) Word8)
        forM_ [n, n - 1 .. 0] $ \i ->
            forM_ [m, m - 1 .. 0] $ \j ->
                if i == n || j == m
                    then do
                        writeArray lengths (i, j) 0
                        writeArray directions (i, j) 0
                    else
                        if baseArray ! i == targetArray ! j
                            then do
                                next <- readArray lengths (i + 1, j + 1)
                                writeArray lengths (i, j) (next + 1)
                                writeArray directions (i, j) 0
                            else do
                                down <- readArray lengths (i + 1, j)
                                right <- readArray lengths (i, j + 1)
                                if down >= right
                                    then do
                                        writeArray lengths (i, j) down
                                        writeArray directions (i, j) 1
                                    else do
                                        writeArray lengths (i, j) right
                                        writeArray directions (i, j) 2
        backtrack n m directions

    backtrack n m directions = go 0 0 []
      where
        go i j hunks
            | i == n && j == m = pure (reverse hunks)
            | i == n = pure (reverse (Hunk i i (drop j targetLines) : hunks))
            | j == m = pure (reverse (Hunk i n [] : hunks))
            | otherwise = do
                direction <- readArray directions (i, j)
                if direction == 0
                    then go (i + 1) (j + 1) hunks
                    else do
                        (i', j') <- consume i j
                        go i' j' (Hunk i i' (slice j j') : hunks)

        consume i j
            | i == n && j == m = pure (i, j)
            | i == n = consume i (j + 1)
            | j == m = consume (i + 1) j
            | otherwise = do
                direction <- readArray directions (i, j)
                if direction == 0
                    then pure (i, j)
                    else
                        if direction == 1
                            then consume (i + 1) j
                            else consume i (j + 1)

        slice start end = take (end - start) (drop start targetLines)

-- | Combine hunks from both sides, rejecting edits that overlap.
mergeHunks :: [Hunk] -> Either () [Hunk]
mergeHunks hunks = go Nothing (sortHunks hunks)
  where
    go _ [] = Right []
    go Nothing (hunk : rest) = (hunk :) <$> go (Just hunk) rest
    go (Just previous) (hunk : rest)
        | sameReplacement previous hunk = go (Just previous) rest
        | hunksOverlap previous hunk = Left ()
        | otherwise = (hunk :) <$> go (Just hunk) rest

deduplicate :: [Hunk] -> [Hunk]
deduplicate = foldr add []
  where
    add hunk hunks
        | any (sameReplacement hunk) hunks = hunks
        | otherwise = hunk : hunks

sortHunks :: [Hunk] -> [Hunk]
sortHunks =
    foldr insert []
  where
    insert hunk [] = [hunk]
    insert hunk (other : rest)
        | hunkBaseStart hunk < hunkBaseStart other = hunk : other : rest
        | otherwise = other : insert hunk rest

sameReplacement :: Hunk -> Hunk -> Bool
sameReplacement left right =
    hunkBaseStart left == hunkBaseStart right
        && hunkBaseEnd left == hunkBaseEnd right
        && hunkReplacement left == hunkReplacement right

hunksOverlap :: Hunk -> Hunk -> Bool
hunksOverlap left right =
    let leftStart = hunkBaseStart left
        leftEnd = hunkBaseEnd left
        rightStart = hunkBaseStart right
        rightEnd = hunkBaseEnd right
     in if leftStart == leftEnd
            then rightStart <= leftStart && leftStart <= rightEnd
            else
                if rightStart == rightEnd
                    then leftStart <= rightStart && rightStart <= leftEnd
                    else leftStart < rightEnd && rightStart < leftEnd

applyHunks :: [ByteString] -> [Hunk] -> [ByteString]
applyHunks baseLines hunks = go 0 hunks
  where
    go position [] = drop position baseLines
    go position (hunk : rest) =
        take (hunkBaseStart hunk - position) (drop position baseLines)
            ++ hunkReplacement hunk
            ++ go (hunkBaseEnd hunk) rest

splitLines :: ByteString -> ([ByteString], Bool)
splitLines bytes
    | BS.null bytes = ([], False)
    | BS8.last bytes == '\n' = (init (BS8.split '\n' bytes), True)
    | otherwise = (BS8.split '\n' bytes, False)

joinLines :: [ByteString] -> Bool -> ByteString
joinLines linesToJoin newline
    | null linesToJoin = if newline then BS8.singleton '\n' else BS.empty
    | newline = BS8.intercalate (BS8.singleton '\n') linesToJoin <> BS8.singleton '\n'
    | otherwise = BS8.intercalate (BS8.singleton '\n') linesToJoin

maximumDiffCells :: Int
maximumDiffCells = 4000000
