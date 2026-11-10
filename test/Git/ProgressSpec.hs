module Git.ProgressSpec
    ( run
    ) where

-- | Deterministic checks for the progress renderer: the pure formatting and
-- line-rendering helpers, the plain (non-TTY) fallback, and the no-op
-- behaviour of the disabled renderer. Nothing here depends on timers, sleeps,
-- terminal capabilities, or the real clock (elapsed values are fixed inputs
-- for the pure renderers; the IO capture only asserts stable structure).
import Control.Exception (SomeException, evaluate, throwIO, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString8
import Data.List (isInfixOf)
import Git.Progress
    ( LineState (..)
    , Phase (..)
    , Progress
    , averageRate
    , consumeRemotePayload
    , formatByteCount
    , formatElapsed
    , formatRate
    , formatSeconds
    , newProgress
    , noProgress
    , progressAbort
    , progressBegin
    , progressEnd
    , progressFinish
    , progressNetwork
    , progressObjects
    , progressRemote
    , remoteMessageLines
    , renderFinishLine
    , renderLine
    , renderRemoteLine
    )
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO
    ( IOMode (ReadMode, WriteMode)
    , hClose
    , hFlush
    , hGetContents
    , hSetEncoding
    , openFile
    , stderr
    , utf8
    )
import TestSupport (assert, assertContains, assertEqual, withTempDirectory)

-- | Walk a progress renderer through every callback once.
run :: IO ()
run = do
    byteCounts
    rates
    averageRates
    elapsedTimes
    secondFormats
    liveLines
    finalLines
    coloredLines
    remoteMessages
    remoteRendering
    remoteBuffering
    finishSummary
    noProgressCallbacks
    disabledRendererIsSilent
    enabledPlainFallback
    putStrLn "Git.ProgressSpec: formatting helpers, plain fallback, and no-op renderer"

-- Formatting helpers --------------------------------------------------------

byteCounts :: IO ()
byteCounts =
    mapM_
        (\(bytes, expected) -> assertEqual expected (formatByteCount bytes) ("formatByteCount " ++ show bytes))
        [ (0, "0 B")
        , (1, "1 B")
        , (1023, "1023 B")
        , (1024, "1.00 KiB")
        , (1536, "1.50 KiB")
        , (1048576, "1.00 MiB")
        , (15802040, "15.07 MiB")
        , (45246054, "43.15 MiB")
        , (1073741824, "1.00 GiB")
        , (2147483648, "2.00 GiB")
        , (-1, "0 B")
        ]

rates :: IO ()
rates = do
    mapM_
        (\(bytesPerSecond, expected) -> assertEqual expected (formatRate bytesPerSecond) ("formatRate " ++ show bytesPerSecond))
        [ (0, "0 B/s")
        , (-5, "0 B/s")
        , (512, "512 B/s")
        , (1572864, "1.50 MiB/s")
        , (26320077, "25.10 MiB/s")
        ]
    assertEqual "0 B/s" (formatRate (0 / 0 :: Double)) "NaN rates render as 0 B/s"
    assertEqual "0 B/s" (formatRate (1 / 0 :: Double)) "infinite rates render as 0 B/s"

-- | The rate shown for a phase is the average over the network window; a final
-- small packet must not drag it down.
averageRates :: IO ()
averageRates = do
    assertEqual 0 (averageRate 0 4000) "no bytes means no rate"
    assertEqual 0 (averageRate 44459622 0) "a zero window means no rate"
    assertEqual 0 (averageRate 44459622 199) "windows under 200 ms are too noisy for a rate"
    assertEqual 1000 (averageRate 1000 1000) "one KiB over one second is 1000 B/s"
    assertEqual
        "10.60 MiB/s"
        (formatRate (averageRate 44459622 4000))
        "42.4 MiB over 4.0 s averages about 10.6 MiB/s"
    assert
        (averageRate 44459622 4000 > 5 * 1024 * 1024)
        "the average rate uses the whole download window, not the last packet gap"

elapsedTimes :: IO ()
elapsedTimes =
    mapM_
        (\(milliseconds, expected) -> assertEqual expected (formatElapsed milliseconds) ("formatElapsed " ++ show milliseconds))
        [ (0, "00:00.0")
        , (-1, "00:00.0")
        , (1, "00:00.0")
        , (3700, "00:03.7")
        , (5850, "00:05.9")
        , (59995, "01:00.0")
        , (61500, "01:01.5")
        , (3723000, "62:03.0")
        ]

secondFormats :: IO ()
secondFormats =
    mapM_
        (\(milliseconds, expected) -> assertEqual expected (formatSeconds milliseconds) ("formatSeconds " ++ show milliseconds))
        [ (0, "0.00")
        , (-4, "0.00")
        , (10, "0.01")
        , (500, "0.50")
        , (999, "1.00")
        , (1000, "1.00")
        , (1250, "1.25")
        , (5820, "5.82")
        ]

-- Pure line rendering -------------------------------------------------------

linesWithPhase :: Phase -> LineState
linesWithPhase phase =
    LineState
        { linePhase = phase
        , lineSpinnerFrame = 0
        , lineObjectsDone = 0
        , lineObjectsTotal = -1
        , lineReceivedBytes = 0
        , lineRateBytesPerSecond = 0
        , lineElapsedMs = 0
        , lineDone = False
        }

liveReceiving :: LineState
liveReceiving =
    (linesWithPhase PhaseReceiving)
        { lineObjectsDone = 16000
        , lineObjectsTotal = 28023
        , lineReceivedBytes = 45246054
        , lineRateBytesPerSecond = 25200000
        , lineElapsedMs = 3700
        }

downloadingReceiving :: LineState
downloadingReceiving =
    (linesWithPhase PhaseReceiving)
        { lineSpinnerFrame = 2
        , lineReceivedBytes = 15802040
        , lineRateBytesPerSecond = 15728640
        , lineElapsedMs = 3200
        }

downloadingWithoutRate :: LineState
downloadingWithoutRate =
    (linesWithPhase PhaseReceiving)
        { lineSpinnerFrame = 1
        , lineReceivedBytes = 15802040
        , lineElapsedMs = 900
        }

-- | A tiny transfer that finished within a few milliseconds: showing a rate
-- would be noise, so only the byte count is rendered.
tinyInstantTransfer :: LineState
tinyInstantTransfer =
    (linesWithPhase PhaseReceiving)
        { lineReceivedBytes = 4700
        , lineRateBytesPerSecond = 3300000
        , lineElapsedMs = 30
        }

-- | A tiny transfer that took long enough for the rate to be meaningful.
tinyMeasurableTransfer :: LineState
tinyMeasurableTransfer =
    tinyInstantTransfer
        { lineElapsedMs = 120
        , lineRateBytesPerSecond = 100000
        }

-- | A large transfer always shows the rate, even for a very fast local copy.
largeInstantTransfer :: LineState
largeInstantTransfer =
    (linesWithPhase PhaseReceiving)
        { lineReceivedBytes = 15802040
        , lineRateBytesPerSecond = 15728640
        , lineElapsedMs = 30
        }

finalReceiving :: LineState
finalReceiving =
    (linesWithPhase PhaseReceiving)
        { lineObjectsDone = 28023
        , lineObjectsTotal = 28023
        , lineReceivedBytes = 45246054
        , lineRateBytesPerSecond = 26320077
        , lineElapsedMs = 1700
        , lineDone = True
        }

-- | A slower clone whose displayed rate is the download-window average, not the
-- last small packet's instantaneous rate.
finalAverageReceiving :: LineState
finalAverageReceiving =
    (linesWithPhase PhaseReceiving)
        { lineObjectsDone = 28023
        , lineObjectsTotal = 28023
        , lineReceivedBytes = 44459622
        , lineRateBytesPerSecond = averageRate 44459622 4000
        , lineElapsedMs = 4000
        , lineDone = True
        }

liveLines :: IO ()
liveLines = do
    assertEqual
        "  ⠋ Receiving objects  ▕███████████░░░░░░░░░▏  57%  43.15 MiB | 24.03 MiB/s  00:03.7"
        (renderLine False liveReceiving)
        "live Receiving line shows spinner, bar, percent, bytes/rate, and timer"
    assertEqual
        "  ⠹ Receiving objects  15.07 MiB | 15.00 MiB/s  00:03.2"
        (renderLine False downloadingReceiving)
        "unknown object total shows bytes and rate instead of a percentage"
    assertEqual
        "  ⠙ Receiving objects  15.07 MiB  00:00.9"
        (renderLine False downloadingWithoutRate)
        "no rate yet shows bytes only"
    assertEqual
        "  ⠹ Resolving deltas  00:00.3"
        (renderLine False ((linesWithPhase PhaseResolvingDeltas) { lineSpinnerFrame = 2, lineElapsedMs = 250 }))
        "Resolving deltas without counts shows a spinner and timer"
    assertEqual
        "  ⠼ Updating files  ▕█████░░░░░░░░░░░░░░░▏  24%  00:00.7"
        ( renderLine
            False
            ( (linesWithPhase PhaseUpdatingFiles)
                { lineSpinnerFrame = 4
                , lineObjectsDone = 12
                , lineObjectsTotal = 50
                , lineElapsedMs = 700
                }
            )
        )
        "Updating files shows a bar and percent"
    assertEqual
        "  ⠋ Receiving objects  ▕████████████████████▏  100%  00:00.0"
        (renderLine False ((linesWithPhase PhaseReceiving) { lineObjectsTotal = 0 }))
        "a zero object total renders 100% without dividing by zero"
    assertEqual
        "  ⠋ Receiving objects  4.59 KiB  00:00.0"
        (renderLine False tinyInstantTransfer)
        "a tiny instant transfer shows the size without a meaningless rate"
    assertEqual
        "  ⠋ Receiving objects  4.59 KiB | 97.66 KiB/s  00:00.1"
        (renderLine False tinyMeasurableTransfer)
        "a transfer with a measurable duration shows its rate"
    assertEqual
        "  ⠋ Receiving objects  15.07 MiB | 15.00 MiB/s  00:00.0"
        (renderLine False largeInstantTransfer)
        "a large transfer shows its rate even when very fast"

finalLines :: IO ()
finalLines = do
    assertEqual
        "  ✓ Receiving objects  100% (28023/28023)  43.15 MiB | 25.10 MiB/s  00:01.7"
        (renderLine False finalReceiving)
        "final Receiving line keeps counts, last bytes and rate, and the phase timer"
    assertEqual
        "  ✓ Receiving objects  100% (28023/28023)  42.40 MiB | 10.60 MiB/s  00:04.0"
        (renderLine False finalAverageReceiving)
        "the final Receiving rate is the download-window average"
    assertEqual
        "  ✓ Resolving deltas  100% (1234/1234)  00:00.4"
        ( renderLine
            False
            ( (linesWithPhase PhaseResolvingDeltas)
                { lineObjectsDone = 1234
                , lineObjectsTotal = 1234
                , lineElapsedMs = 400
                , lineDone = True
                }
            )
        )
        "final Resolving deltas line reports done counts"
    assertEqual
        "  ✓ Updating files  100% (50/50)  00:01.3"
        ( renderLine
            False
            ( (linesWithPhase PhaseUpdatingFiles)
                { lineObjectsDone = 50
                , lineObjectsTotal = 50
                , lineElapsedMs = 1250
                , lineDone = True
                }
            )
        )
        "final Updating files line reports done counts"
    assertEqual
        "  ✓ Receiving objects  100% (7/7)  4.59 KiB  00:00.0"
        ( renderLine
            False
            tinyInstantTransfer
                { lineObjectsDone = 7
                , lineObjectsTotal = 7
                , lineDone = True
                }
        )
        "a tiny final Receiving line omits the meaningless rate"

coloredLines :: IO ()
coloredLines = do
    assertContains "\ESC[1;36m" (renderLine True liveReceiving) "colored live line styles the phase label"
    assertContains "\ESC[1;33m" (renderLine True liveReceiving) "colored live line styles the spinner"
    assertContains "\ESC[32m" (renderLine True liveReceiving) "colored live line styles the filled bar green"
    assertContains "\ESC[1;32m" (renderLine True finalReceiving) "colored final line styles the check green"
    assert (not (elem '\ESC' (renderLine False liveReceiving))) "plain line must not contain ANSI escapes"
    assert (not (elem '\ESC' (renderLine False finalReceiving))) "plain final line must not contain ANSI escapes"
    assertEqual
        (renderLine False liveReceiving)
        (stripAnsi (renderLine True liveReceiving))
        "colors must not change the visible live line"
    assertEqual
        (renderLine False finalReceiving)
        (stripAnsi (renderLine True finalReceiving))
        "colors must not change the visible final line"

-- Remote sideband messages --------------------------------------------------

remoteMessages :: IO ()
remoteMessages = do
    assertEqual
        ([], "")
        (consumeRemotePayload "" ByteString8.empty)
        "an empty payload leaves the buffer empty"
    assertEqual
        (["Counting objects: 100% (5/5), done."], "")
        (consumeRemotePayload "" (pack "Counting objects: 100% (5/5), done.\n"))
        "a single complete sideband message"
    assertEqual
        ( ["Enumerating objects: 5, done.", "Counting objects: 100% (2/2), done."]
        , ""
        )
        ( consumeRemotePayload
            ""
            (pack "Enumerating objects: 5, done.\nCounting objects:  50% (1/2)\rCounting objects: 100% (2/2), done.\n")
        )
        "only the final redraw segment of each logical line is printed"
    assertEqual
        ([], "Counting objects: 100%")
        (consumeRemotePayload "" (pack "Counting objects: 100%"))
        "an incomplete line is buffered, not printed"
    assertEqual
        (["Counting objects: 100% (5/5), done."], "")
        (consumeRemotePayload "Counting objects: 100% (5/5), done." (pack "\n"))
        "a later chunk completes the buffered line"
    assertEqual
        ([], "Compress")
        (consumeRemotePayload "" (pack "Compressing objects:  50% (1/2)\rCompress"))
        "a carriage-return redraw resets the buffered segment"
    assertEqual
        (["Compressing objects: 100% (2/2), done."], "")
        (consumeRemotePayload "Compress" (pack "ing objects: 100% (2/2), done.\n"))
        "a logical line split across chunks is reassembled"
    assertEqual
        (["Total 5 (delta 1), reused 0 (delta 0), pack-reused 0 (from 0)"], "")
        ( consumeRemotePayload
            ""
            (pack "Total 5 (delta 1), reused 0 (delta 0), pack-reused 0 (from 0)        \n")
        )
        "trailing progress padding is trimmed"
    assertEqual
        ([], "")
        (consumeRemotePayload "" (pack "\r\r\n"))
        "empty redraws are skipped"
    assertEqual
        ["Counting objects: 100% (5/5), done."]
        (remoteMessageLines (pack "Counting objects:  20% (1/5)\rCounting objects: 100% (5/5), done.\n"))
        "remoteMessageLines is the one-shot form of the same parser"
    assertEqual
        ["one", "two"]
        (remoteMessageLines (pack "one\ntwo\n"))
        "several complete lines are reported in order"

remoteRendering :: IO ()
remoteRendering = do
    let message = "Counting objects: 1/1, done."
    assertEqual
        "remote: Counting objects: 1/1, done."
        (renderRemoteLine False message)
        "plain remote line has the git-style prefix"
    assertContains "\ESC[2;36mremote:" (renderRemoteLine True message) "colored remote prefix is dim cyan"
    assertEqual
        (renderRemoteLine False message)
        (stripAnsi (renderRemoteLine True message))
        "colors must not change the visible remote line"

-- | A remote line split across two 'progressRemote' calls, with an in-place CR
-- redraw, must be reassembled and printed exactly once (at finish, when the
-- server never completes it with a newline).
remoteBuffering :: IO ()
remoteBuffering = do
    captured <- captureStderr $ do
        progress <- newProgress True
        progressRemote progress (pack "Counting objects:  50% (1/2)\rCounting")
        progressRemote progress (pack " objects: 100% (2/2), done.")
        progressFinish progress
    let outputLines = lines captured
    case outputLines of
        [] -> die "the buffered remote tail was not flushed by progressFinish"
        firstLine : _ ->
            assertEqual
                "remote: Counting objects: 100% (2/2), done."
                firstLine
                "the split remote line is reassembled and its redraw collapsed"
    assert (not (isInfixOf "50%" captured)) "intermediate carriage-return redraws are not printed"
    assert (not (elem '\r' captured)) "plain fallback must not move the cursor"
    assertEqual 2 (length outputLines) "flushed remote tail plus the finish summary"

finishSummary :: IO ()
finishSummary = do
    assertEqual "  ✓ done in 5.82s" (renderFinishLine False 5820) "finish summary with two-decimal total elapsed"
    assertEqual "  ✓ done in 0.00s" (renderFinishLine False 0) "finish summary for an instant operation"
    assertContains "\ESC[1;32m" (renderFinishLine True 5820) "finish summary styles the check green"
    assertContains "\ESC[2m" (renderFinishLine True 5820) "finish summary dims the elapsed text"
    assertEqual
        (renderFinishLine False 5820)
        (stripAnsi (renderFinishLine True 5820))
        "colors must not change the visible finish summary"

-- Renderer behaviour --------------------------------------------------------

-- | Drive every callback in a plausible clone order. The remote chunks mimic
-- what git servers actually send: in-place CR progress redraws, and packets
-- that split a progress line mid-way.
exercise :: Progress -> IO ()
exercise progress = do
    progressRemote progress (pack "Enumerating objects: 5, done.\n")
    progressRemote
        progress
        (pack "Counting objects:  50% (1/2)\rCounting objects: 100% (2/2), done.\n")
    progressRemote progress (pack "Compressing objects:  50% (1/2)\rCompress")
    progressRemote progress (pack "ing objects: 100% (2/2), done.\n")
    progressRemote progress (pack "Total 5 (delta 1), reused 0 (delta 0), pack-reused 0 (from 0)\n")
    progressBegin progress PhaseReceiving
    progressNetwork progress 15802040
    progressObjects progress 0 28023
    progressObjects progress 28023 28023
    progressEnd progress PhaseReceiving
    progressBegin progress PhaseResolvingDeltas
    progressObjects progress 1234 1234
    progressEnd progress PhaseResolvingDeltas
    progressBegin progress PhaseUpdatingFiles
    progressObjects progress 1 1
    progressEnd progress PhaseUpdatingFiles
    progressFinish progress

noProgressCallbacks :: IO ()
noProgressCallbacks = do
    outcome <- try (exercise noProgress) :: IO (Either SomeException ())
    case outcome of
        Left exception -> die ("noProgress callbacks threw: " ++ show exception)
        Right () -> pure ()
    -- finish/abort are idempotent-safe: a second call must be a no-op too.
    outcomeAfterFinish <- try (progressFinish noProgress >> progressAbort noProgress) :: IO (Either SomeException ())
    case outcomeAfterFinish of
        Left exception -> die ("repeated finish/abort on noProgress threw: " ++ show exception)
        Right () -> pure ()

disabledRendererIsSilent :: IO ()
disabledRendererIsSilent = do
    progress <- newProgress False
    captured <- captureStderr (exercise progress)
    assertEqual "" captured "newProgress False must produce no output"

enabledPlainFallback :: IO ()
enabledPlainFallback = do
    captured <- captureStderr $ do
        progress <- newProgress True
        exercise progress
    let outputLines = lines captured
    -- With stderr redirected to a file there is no terminal, so the renderer
    -- must fall back to plain final lines: four collapsed remote lines, one
    -- line per finished phase, and the summary.
    assertEqual 8 (length outputLines) "plain fallback prints 4 remote lines + 3 phase lines + summary"
    case outputLines of
        [] -> die "plain fallback produced no output"
        firstLine : _ ->
            assertEqual
                "remote: Enumerating objects: 5, done."
                firstLine
                "plain fallback prints remote messages without a prefix beyond 'remote: '"
    assert (not (elem '\ESC' captured)) "plain fallback must not emit ANSI escapes"
    assert (not (elem '\r' captured)) "plain fallback must not move the cursor"
    assertContains "remote: Counting objects: 100% (2/2), done." captured "remote CR redraws collapse to the final segment"
    assertContains "remote: Compressing objects: 100% (2/2), done." captured "a remote line split across chunks is reassembled"
    assertContains "remote: Total 5 (delta 1), reused 0 (delta 0), pack-reused 0 (from 0)" captured "the remote total line is printed once"
    assert (not (isInfixOf " 50% " captured)) "intermediate remote redraws are not printed"
    assertContains "✓ Receiving objects  100% (28023/28023)  15.07 MiB" captured "plain fallback prints the Receiving final line with network bytes"
    assertContains "✓ Resolving deltas  100% (1234/1234)" captured "plain fallback prints the Resolving deltas final line"
    assertContains "✓ Updating files  100% (1/1)" captured "plain fallback prints the Updating files final line"
    assertContains "✓ done in " captured "plain fallback prints the total elapsed summary"
    assertEqual
        4
        (length (filter (isInfixOf "remote: ") outputLines))
        "plain fallback prints each completed remote line exactly once"

-- Helpers -------------------------------------------------------------------

pack :: String -> ByteString
pack = ByteString8.pack

-- | Remove ANSI SGR sequences so colored and plain renderings can be compared.
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC' : '[' : rest) = stripAnsi (drop 1 (dropWhile (/= 'm') rest))
stripAnsi (character : rest) = character : stripAnsi rest

-- | Run an action with stderr redirected into a temporary file and return what
-- was written. The real stderr is restored before any assertion can fail, so
-- test failures still reach the outside world.
captureStderr :: IO () -> IO String
captureStderr action =
    withTempDirectory "haskell-git-progress-capture" $ \directory -> do
        let outputPath = directory </> "stderr.txt"
        hFlush stderr
        savedStderr <- hDuplicate stderr
        outputHandle <- openFile outputPath WriteMode
        hDuplicateTo outputHandle stderr
        outcome <- try action :: IO (Either SomeException ())
        hFlush stderr
        hDuplicateTo savedStderr stderr
        hFlush stderr
        hClose outputHandle
        hClose savedStderr
        case outcome of
            Left exception -> throwIO exception
            Right () -> pure ()
        readUtf8 outputPath

readUtf8 :: FilePath -> IO String
readUtf8 path = do
    handle <- openFile path ReadMode
    hSetEncoding handle utf8
    contents <- hGetContents handle
    _ <- evaluate (length contents)
    hClose handle
    pure contents
