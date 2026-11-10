-- | A small, git-flavoured progress renderer for clone and fetch.
--
-- The renderer draws a single live line on stderr:
--
-- >   ⠹ Receiving objects  ▕███████████░░░░░░░░░▏  57%  43.15 MiB | 24.03 MiB/s  00:03.7
--
-- and finishes every phase with a compact summary line:
--
-- >   ✓ Receiving objects  100% (28023/28023)  43.15 MiB | 24.03 MiB/s  00:01.7
--
-- Server-side sideband messages are printed as @remote: ...@ lines above the
-- live line. When colors are unavailable the output falls back to clean,
-- plain final lines (no animation, no carriage returns, no escapes), so logs
-- and piped output stay readable.
module Git.Progress
    ( Phase (..)
    , Progress
    , newProgress
    , noProgress
    , progressRemote
    , progressBegin
    , progressObjects
    , progressNetwork
    , progressEnd
    , progressFinish
    , progressAbort
      -- * Pure rendering helpers (exported for deterministic tests)
    , LineState (..)
    , renderLine
    , renderRemoteLine
    , renderFinishLine
    , formatByteCount
    , formatRate
    , formatElapsed
    , formatSeconds
    , averageRate
    , consumeRemotePayload
    , remoteMessageLines
    ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString8
import Data.Char (isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Environment (lookupEnv)
import System.IO
    ( hFlush
    , hIsTerminalDevice
    , hPutStr
    , hSetEncoding
    , stderr
    , utf8
    )
import Text.Printf (printf)

-- | Progress phases reported by clone and fetch. Each phase draws one live
-- line and ends with a final "100%, done" line.
data Phase
    = PhaseReceiving
    | PhaseResolvingDeltas
    | PhaseUpdatingFiles
    deriving (Eq, Show)

-- | A progress renderer on stderr. A disabled renderer produces no output at
-- all. The callback functions are safe to call from the command's main thread:
--
--   * 'progressRemote' prints server sideband progress messages above the
--     live line. The payload is raw sideband text and may be split anywhere,
--     even mid-line; a partial line is buffered until it completes. Git's
--     servers redraw progress in place with @\\r@, so only the final redraw
--     segment of each completed line is printed, and blank lines are skipped.
--   * 'progressBegin' starts a phase and its timer and draws the live line.
--   * 'progressObjects' reports object/file progress within the current phase.
--   * 'progressNetwork' reports the cumulative number of pack bytes received;
--     used while the object total is not known yet.
--   * 'progressEnd' finishes the current phase's line.
--   * 'progressFinish' clears the live line and prints the total elapsed time.
--   * 'progressAbort' clears the live line without a success summary.
data Progress = Progress
    { progressRemote :: ByteString -> IO ()
    , progressBegin :: Phase -> IO ()
    , progressObjects :: Int -> Int -> IO ()
    , progressNetwork :: Integer -> IO ()
    , progressEnd :: Phase -> IO ()
    , progressFinish :: IO ()
    , progressAbort :: IO ()
    }

-- | The renderer used when progress output is disabled.
noProgress :: Progress
noProgress =
    Progress
        { progressRemote = const (pure ())
        , progressBegin = const (pure ())
        , progressObjects = const (const (pure ()))
        , progressNetwork = const (pure ())
        , progressEnd = const (pure ())
        , progressFinish = pure ()
        , progressAbort = pure ()
        }

-- | Create a renderer. The flag comes from the command line (and defaults to
-- true only when stderr is a terminal); a disabled renderer is 'noProgress'.
--
-- When enabled, the renderer animates a live line only when stderr is a
-- terminal and @TERM@ is not @dumb@. ANSI colors additionally require that
-- @NO_COLOR@ is absent from the environment. Otherwise the renderer emits
-- plain, newline-terminated final lines only.
newProgress :: Bool -> IO Progress
newProgress False = pure noProgress
newProgress True = do
    terminalResult <- try (hIsTerminalDevice stderr) :: IO (Either IOException Bool)
    let isTerminal = either (const False) id terminalResult
    dumbTerminal <- maybe False (== "dumb") <$> lookupEnv "TERM"
    noColor <- isJust <$> lookupEnv "NO_COLOR"
    -- The line uses braille and block characters; pin down a UTF-8 encoding so
    -- they render even from a C/POSIX locale. Failure is harmless.
    _ <- try (hSetEncoding stderr utf8) :: IO (Either IOException ())
    startTime <- getCurrentTime
    currentRef <- newIORef (Nothing :: Maybe PhaseState)
    lastDrawRef <- newIORef startTime
    spinnerRef <- newIORef 0
    lineOpenRef <- newIORef False
    remoteBufferRef <- newIORef ""
    closedRef <- newIORef False
    let animated = isTerminal && not dumbTerminal
        useColor = animated && not noColor
        renderer =
            Renderer
                { rendererAnimated = animated
                , rendererColor = useColor
                , rendererTotalStarted = startTime
                , rendererCurrent = currentRef
                , rendererLastDraw = lastDrawRef
                , rendererSpinner = spinnerRef
                , rendererLineOpen = lineOpenRef
                , rendererRemoteBuffer = remoteBufferRef
                , rendererClosed = closedRef
                }
    pure (progressFor renderer)

-- Pure formatting -----------------------------------------------------------

-- | Human readable byte count: plain bytes below one KiB, two decimals (like
-- git) for KiB, MiB, and GiB. Negative counts render as @0 B@.
formatByteCount :: Integer -> String
formatByteCount bytes
    | bytes < 0 = "0 B"
    | bytes < 1024 = show bytes ++ " B"
    | bytes < mebibyte = twoDecimals (fromIntegral bytes / 1024) ++ " KiB"
    | bytes < gibibyte =
        twoDecimals (fromIntegral bytes / fromIntegral mebibyte) ++ " MiB"
    | otherwise =
        twoDecimals (fromIntegral bytes / fromIntegral gibibyte) ++ " GiB"
  where
    mebibyte :: Integer
    mebibyte = 1024 * 1024
    gibibyte :: Integer
    gibibyte = 1024 * 1024 * 1024

-- | Render a transfer rate as a byte count per second, e.g. @24.03 MiB/s@.
-- Non-positive or non-finite rates render as @0 B\/s@.
formatRate :: Double -> String
formatRate bytesPerSecond
    | isNaN bytesPerSecond = "0 B/s"
    | isInfinite bytesPerSecond = "0 B/s"
    | bytesPerSecond <= 0 = "0 B/s"
    | otherwise = formatByteCount (round bytesPerSecond) ++ "/s"

-- | Average network rate in bytes per second over a measured window. This is
-- the rate shown for a phase: the bytes received divided by the time between
-- the phase start and the last network sample, so it stays stable and is not
-- dragged down by a final small packet. Windows shorter than 200 ms are too
-- noisy to be meaningful and report 0 (the renderer then omits the rate).
averageRate :: Integer -> Int -> Double
averageRate bytes elapsedMs
    | bytes <= 0 = 0
    | elapsedMs < minimumMeaningfulWindowMs = 0
    | otherwise = fromIntegral bytes * 1000 / fromIntegral elapsedMs
  where
    minimumMeaningfulWindowMs = 200

-- | Render an elapsed time in milliseconds as @mm:ss.d@, e.g. @00:03.7@.
formatElapsed :: Int -> String
formatElapsed milliseconds
    | milliseconds <= 0 = "00:00.0"
    | otherwise =
        let totalTenths = (milliseconds + 50) `div` 100
            totalSeconds = totalTenths `div` 10
            tenth = totalTenths `mod` 10
            minutes = totalSeconds `div` 60
            seconds = totalSeconds `mod` 60
         in printf "%02d:%02d.%d" minutes seconds tenth

-- | Render an elapsed time in milliseconds as seconds with two decimals, e.g.
-- @5.82@. Used by the final @done in 5.82s@ summary.
formatSeconds :: Int -> String
formatSeconds milliseconds
    | milliseconds <= 0 = "0.00"
    | otherwise = printf "%.2f" (fromIntegral milliseconds / 1000 :: Double)

-- | Incremental parser for server sideband progress text.
--
-- Git's servers redraw progress in place with @\\r@ and can also split a
-- progress line across sideband packets. Feed this function the buffered tail
-- from the previous call and the next raw sideband chunk; it returns the
-- completed, printable messages and the new buffered tail. A newline completes
-- a logical line; carriage returns discard the text they redraw over, so only
-- the final redraw segment of a logical line is reported (matching what git
-- itself ends up showing on one terminal line). Trailing whitespace is
-- dropped and empty messages are skipped.
consumeRemotePayload :: String -> ByteString -> ([String], String)
consumeRemotePayload buffered payload = go [] buffered (ByteString8.unpack payload)
  where
    go messages current [] = (reverse messages, current)
    go messages current (character : rest) =
        case character of
            '\r' -> go messages "" rest
            '\n' ->
                let message = dropWhileEnd isSpace current
                 in go
                        (if null message then messages else message : messages)
                        ""
                        rest
            _ ->
                let (chunk, remaining) = break isLineBreak (character : rest)
                 in go messages (current ++ chunk) remaining
    isLineBreak character = character == '\n' || character == '\r'

-- | Convenience wrapper around 'consumeRemotePayload' for a complete chunk:
-- the printable messages, ignoring any trailing partial line.
remoteMessageLines :: ByteString -> [String]
remoteMessageLines payload = fst (consumeRemotePayload "" payload)

-- | One snapshot of a progress line, sufficient to render it purely.
data LineState = LineState
    { linePhase :: Phase
    , lineSpinnerFrame :: Int
    -- ^ Index into the braille spinner frames; incremented on each redraw.
    , lineObjectsDone :: Int
    , lineObjectsTotal :: Int
    -- ^ Negative when the total is not known yet.
    , lineReceivedBytes :: Integer
    , lineRateBytesPerSecond :: Double
    , lineElapsedMs :: Int
    , lineDone :: Bool
    -- ^ Render the final @✓ ... done@ form instead of the live bar form.
    }
    deriving (Eq, Show)

-- | Render one progress line. The first argument selects ANSI colors; the
-- rendered text (ignoring escapes) is identical either way. A live line looks
-- like
--
-- >   ⠹ Receiving objects  ▕███████████░░░░░░░░░▏  57%  43.15 MiB | 24.03 MiB/s  00:03.7
--
-- and a final line looks like
--
-- >   ✓ Receiving objects  100% (28023/28023)  43.15 MiB | 24.03 MiB/s  00:01.7
renderLine :: Bool -> LineState -> String
renderLine useColor state =
    "  " ++ glyph ++ " " ++ label ++ "  " ++ intercalate "  " parts
  where
    label = paint useColor labelCode (phaseLabel (linePhase state))
    glyph
        | lineDone state = paint useColor checkCode "\10003"
        | otherwise = paint useColor spinnerCode (spinnerFrame (lineSpinnerFrame state))
    knownTotal = lineObjectsTotal state >= 0
    percent = percentOf (lineObjectsDone state) (lineObjectsTotal state)
    bar
        | knownTotal && not (lineDone state) = renderBar useColor percent
        | otherwise = ""
    counts
        | not knownTotal = ""
        | lineDone state =
            paint useColor boldCode "100%"
                ++ " "
                ++ paint useColor dimCode (countsText total)
        | otherwise = paint useColor boldCode (show percent ++ "%")
    total = max 0 (lineObjectsTotal state)
    countsText value = "(" ++ show value ++ "/" ++ show value ++ ")"
    network
        | linePhase state /= PhaseReceiving = ""
        | lineReceivedBytes state <= 0 = ""
        | otherwise = paint useColor dimCode (networkText)
    -- A rate is only meaningful once either enough data or enough time has
    -- passed; for a tiny instant clone "4.59 KiB | 2.31 GiB/s" is noise, so
    -- show just the size until one of the thresholds is crossed.
    showRate =
        lineRateBytesPerSecond state > 0
            && (lineElapsedMs state >= 100 || lineReceivedBytes state >= 1024 * 1024)
    networkText
        | showRate =
            formatByteCount (lineReceivedBytes state)
                ++ " | "
                ++ formatRate (lineRateBytesPerSecond state)
        | otherwise = formatByteCount (lineReceivedBytes state)
    timer = paint useColor dimCode (formatElapsed (lineElapsedMs state))
    parts = filter (not . null) [bar, counts, network, timer]

-- | Render one server sideband line: @remote: <message>@ with a dim cyan
-- @remote:@ prefix when colors are enabled.
renderRemoteLine :: Bool -> String -> String
renderRemoteLine useColor message =
    paint useColor remoteCode "remote:" ++ " " ++ message

-- | Render the total-elapsed summary printed by 'progressFinish', e.g.
-- @  ✓ done in 5.82s@.
renderFinishLine :: Bool -> Int -> String
renderFinishLine useColor elapsedMs =
    "  "
        ++ paint useColor checkCode "\10003"
        ++ " "
        ++ paint useColor dimCode ("done in " ++ formatSeconds elapsedMs ++ "s")

-- Internals ----------------------------------------------------------------

-- | How the renderer talks to the terminal.
data RenderMode
    = RenderPlain
    | RenderAnimated Bool
    -- ^ Whether ANSI colors are enabled.

data Renderer = Renderer
    { rendererAnimated :: Bool
    , rendererColor :: Bool
    , rendererTotalStarted :: UTCTime
    , rendererCurrent :: IORef (Maybe PhaseState)
    , rendererLastDraw :: IORef UTCTime
    , rendererSpinner :: IORef Int
    , rendererLineOpen :: IORef Bool
    , rendererRemoteBuffer :: IORef String
    , rendererClosed :: IORef Bool
    }

-- | Live state for the phase currently being rendered.
data PhaseState = PhaseState
    { phaseKind :: Phase
    , phaseStarted :: UTCTime
    , phaseObjectsDone :: Int
    , phaseObjectsTotal :: Int
    , phaseReceivedBytes :: Integer
    , phaseLastNetworkTime :: Maybe UTCTime
    -- ^ Timestamp of the last 'progressNetwork' sample. Absent until the first
    -- byte arrives; frozen afterwards, which keeps the final rate on the
    -- network window instead of including retention work.
    }

redrawIntervalSeconds :: Double
redrawIntervalSeconds = 0.075

clearLineCode :: String
clearLineCode = "\ESC[K"

labelCode, checkCode, spinnerCode, dimCode, barCode, boldCode, remoteCode :: String
labelCode = "\ESC[1;36m" -- bold cyan
checkCode = "\ESC[1;32m" -- bold green
spinnerCode = "\ESC[1;33m" -- bold yellow
dimCode = "\ESC[2m" -- dim
barCode = "\ESC[32m" -- green
boldCode = "\ESC[1m" -- bold
remoteCode = "\ESC[2;36m" -- dim cyan

resetCode :: String
resetCode = "\ESC[0m"

paint :: Bool -> String -> String -> String
paint useColor code text
    | null text = ""
    | useColor = code ++ text ++ resetCode
    | otherwise = text

renderBar :: Bool -> Int -> String
renderBar useColor percent =
    paint useColor dimCode "\9621" -- ▕
        ++ paint useColor barCode (replicate filled '█')
        ++ paint useColor dimCode (replicate empty '░')
        ++ paint useColor dimCode "\9615" -- ▏
  where
    cells = 20
    clamped = max 0 (min 100 percent)
    filled = (clamped * cells + 50) `div` 100
    empty = cells - filled

spinnerFrames :: [String]
spinnerFrames =
    [ "⠋"
    , "⠙"
    , "⠹"
    , "⠸"
    , "⠼"
    , "⠴"
    , "⠦"
    , "⠧"
    , "⠇"
    , "⠏"
    ]

spinnerFrame :: Int -> String
spinnerFrame index = spinnerFrames !! (index `mod` length spinnerFrames)

phaseLabel :: Phase -> String
phaseLabel PhaseReceiving = "Receiving objects"
phaseLabel PhaseResolvingDeltas = "Resolving deltas"
phaseLabel PhaseUpdatingFiles = "Updating files"

percentOf :: Int -> Int -> Int
percentOf done total
    | total <= 0 = 100
    | otherwise = max 0 (min 100 ((max 0 done * 100) `div` total))

twoDecimals :: Double -> String
twoDecimals value = printf "%.2f" value

secondsBetween :: UTCTime -> UTCTime -> Double
secondsBetween start end = realToFrac (diffUTCTime end start)

millisecondsBetween :: UTCTime -> UTCTime -> Int
millisecondsBetween start end =
    max 0 (round (secondsBetween start end * 1000 :: Double))

-- IO plumbing --------------------------------------------------------------

progressFor :: Renderer -> Progress
progressFor renderer =
    Progress
        { progressRemote = \payload -> unlessClosed renderer (handleRemote renderer payload)
        , progressBegin = \phase -> unlessClosed renderer (handleBegin renderer phase)
        , progressObjects = \done total ->
            unlessClosed renderer (handleObjects renderer done total)
        , progressNetwork = \bytes ->
            unlessClosed renderer (handleNetwork renderer bytes)
        , progressEnd = \phase -> unlessClosed renderer (handleEnd renderer phase)
        , progressFinish = closeOnce renderer (handleFinish renderer)
        , progressAbort = closeOnce renderer (handleAbort renderer)
        }

unlessClosed :: Renderer -> IO () -> IO ()
unlessClosed renderer action = do
    closed <- readIORef (rendererClosed renderer)
    unless closed action

closeOnce :: Renderer -> IO () -> IO ()
closeOnce renderer action = do
    closed <- readIORef (rendererClosed renderer)
    unless closed $ do
        action
        writeIORef (rendererClosed renderer) True

handleBegin :: Renderer -> Phase -> IO ()
handleBegin renderer phase = do
    now <- getCurrentTime
    discardLiveLine renderer
    writeIORef (rendererCurrent renderer) (Just (newPhaseState phase now))
    drawLive renderer True

newPhaseState :: Phase -> UTCTime -> PhaseState
newPhaseState phase started =
    PhaseState
        { phaseKind = phase
        , phaseStarted = started
        , phaseObjectsDone = 0
        , phaseObjectsTotal = -1
        , phaseReceivedBytes = 0
        , phaseLastNetworkTime = Nothing
        }

handleObjects :: Renderer -> Int -> Int -> IO ()
handleObjects renderer done total = do
    active <- readIORef (rendererCurrent renderer)
    case active of
        Nothing -> pure ()
        Just state -> do
            writeIORef (rendererCurrent renderer) $
                Just
                    state
                        { phaseObjectsDone = max 0 done
                        , phaseObjectsTotal = total
                        }
            drawLive renderer False

handleNetwork :: Renderer -> Integer -> IO ()
handleNetwork renderer cumulativeBytes = do
    now <- getCurrentTime
    active <- readIORef (rendererCurrent renderer)
    case active of
        Nothing -> pure ()
        Just state -> do
            writeIORef (rendererCurrent renderer) $
                Just
                    state
                        { phaseReceivedBytes = max 0 cumulativeBytes
                        , phaseLastNetworkTime = Just now
                        }
            drawLive renderer False

-- | Rate for the phase's network window: bytes received divided by the time
-- from the phase start to the last network sample. A running average is used
-- on every live redraw (so the number is smooth), and because the timestamp is
-- frozen after the last sample the final line reports the download average
-- rather than the last chunk's instantaneous rate.
networkRate :: PhaseState -> Double
networkRate state =
    case phaseLastNetworkTime state of
        Nothing -> 0
        Just lastSampleTime ->
            averageRate
                (phaseReceivedBytes state)
                (millisecondsBetween (phaseStarted state) lastSampleTime)

handleEnd :: Renderer -> Phase -> IO ()
handleEnd renderer phase = do
    active <- readIORef (rendererCurrent renderer)
    case active of
        Just state
            | phaseKind state == phase -> do
                now <- getCurrentTime
                let total = phaseObjectsTotal state
                    finalDone = if total >= 0 then total else phaseObjectsDone state
                    lineState =
                        (stateToLineState state 0 now)
                            { lineObjectsDone = finalDone
                            , lineDone = True
                            }
                    rendered = renderLine (rendererColor renderer) lineState
                case renderMode renderer of
                    RenderPlain -> do
                        hPutStr stderr (rendered ++ "\n")
                        hFlush stderr
                    RenderAnimated _ -> do
                        hPutStr stderr ('\r' : clearLineCode ++ rendered ++ "\n")
                        hFlush stderr
                        writeIORef (rendererLineOpen renderer) False
                        writeIORef (rendererLastDraw renderer) now
                writeIORef (rendererCurrent renderer) Nothing
        _ -> pure ()

handleRemote :: Renderer -> ByteString -> IO ()
handleRemote renderer payload = do
    buffered <- readIORef (rendererRemoteBuffer renderer)
    let (messages, trailing) = consumeRemotePayload buffered payload
    writeIORef (rendererRemoteBuffer renderer) trailing
    unless (null messages) $ do
        _ <- clearLiveLine renderer
        forM_ messages $ \message -> do
            hPutStr stderr (renderRemoteLine (rendererColor renderer) message)
            hPutStr stderr "\n"
        hFlush stderr
        drawLive renderer True

-- | Print the tail of a remote progress line that the server never completed,
-- e.g. if the connection closed right after a message.
flushRemoteTail :: Renderer -> IO ()
flushRemoteTail renderer = do
    buffered <- readIORef (rendererRemoteBuffer renderer)
    let message = dropWhileEnd isSpace buffered
    writeIORef (rendererRemoteBuffer renderer) ""
    unless (null message) $ do
        _ <- clearLiveLine renderer
        hPutStr stderr (renderRemoteLine (rendererColor renderer) message)
        hPutStr stderr "\n"
        hFlush stderr

handleFinish :: Renderer -> IO ()
handleFinish renderer = do
    now <- getCurrentTime
    flushRemoteTail renderer
    case renderMode renderer of
        RenderAnimated _ -> do
            stillActive <- clearLiveLine renderer
            when stillActive $ do
                hPutStr stderr "\n"
                hFlush stderr
        RenderPlain -> pure ()
    writeIORef (rendererCurrent renderer) Nothing
    let elapsedMs = millisecondsBetween (rendererTotalStarted renderer) now
    hPutStr stderr (renderFinishLine (rendererColor renderer) elapsedMs ++ "\n")
    hFlush stderr

handleAbort :: Renderer -> IO ()
handleAbort renderer = do
    _ <- clearLiveLine renderer
    writeIORef (rendererCurrent renderer) Nothing

-- | Erase the live line, if one is being shown. Returns 'True' when a line was
-- actually open, so the caller can terminate it with a newline.
clearLiveLine :: Renderer -> IO Bool
clearLiveLine renderer =
    case renderMode renderer of
        RenderPlain -> pure False
        RenderAnimated _ -> do
            lineOpen <- readIORef (rendererLineOpen renderer)
            when lineOpen $ do
                hPutStr stderr ('\r' : clearLineCode)
                hFlush stderr
                writeIORef (rendererLineOpen renderer) False
            pure lineOpen

-- | Erase a live line left behind by a phase that was never ended (for example
-- when the next phase begins early) without printing a final line for it.
discardLiveLine :: Renderer -> IO ()
discardLiveLine renderer = do
    active <- isJust <$> readIORef (rendererCurrent renderer)
    when active $ do
        wasOpen <- clearLiveLine renderer
        when wasOpen $ do
            hPutStr stderr "\n"
            hFlush stderr
        writeIORef (rendererCurrent renderer) Nothing

drawLive :: Renderer -> Bool -> IO ()
drawLive renderer force =
    case renderMode renderer of
        RenderPlain -> pure ()
        RenderAnimated _ -> do
            active <- readIORef (rendererCurrent renderer)
            case active of
                Nothing -> pure ()
                Just state -> do
                    now <- getCurrentTime
                    lastDraw <- readIORef (rendererLastDraw renderer)
                    let sinceDraw = secondsBetween lastDraw now
                    when (force || sinceDraw >= redrawIntervalSeconds) $ do
                        frame <- readIORef (rendererSpinner renderer)
                        let rendered =
                                renderLine
                                    (rendererColor renderer)
                                    (stateToLineState state frame now)
                        hPutStr stderr ('\r' : clearLineCode ++ rendered)
                        hFlush stderr
                        writeIORef (rendererLastDraw renderer) now
                        writeIORef (rendererSpinner renderer) (frame + 1)
                        writeIORef (rendererLineOpen renderer) True

stateToLineState :: PhaseState -> Int -> UTCTime -> LineState
stateToLineState state frame now =
    LineState
        { linePhase = phaseKind state
        , lineSpinnerFrame = frame
        , lineObjectsDone = phaseObjectsDone state
        , lineObjectsTotal = phaseObjectsTotal state
        , lineReceivedBytes = phaseReceivedBytes state
        , lineRateBytesPerSecond = networkRate state
        , lineElapsedMs = millisecondsBetween (phaseStarted state) now
        , lineDone = False
        }

renderMode :: Renderer -> RenderMode
renderMode renderer
    | rendererAnimated renderer = RenderAnimated (rendererColor renderer)
    | otherwise = RenderPlain
