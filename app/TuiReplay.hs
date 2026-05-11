module Main where

import Control.Concurrent (forkIO, threadDelay)
import Data.IORef
import Text.Read (readMaybe)
import Data.Time
import Backtest
import Strategies
import TUI

-- Microseconds between candles.  5 000 µs ≈ 200 candles/sec.
-- Lower = faster replay.  0 = as fast as possible.
replayDelayUs :: Int
replayDelayUs = 5000

main :: IO ()
main = do
    let dataFile = "1YearHistoricalData10Min.csv"
    let strategy = customRangeReversionStrategy 0.45 0.25

    contents <- readFile dataFile
    let candles = mapMaybe' parseCsvRow (lines contents)
    let total   = length candles          -- forces full parse before TUI starts

    tuiHandle <- newTuiHandle
    setTuiTotal tuiHandle total
    stateRef  <- newIORef initialBacktestState

    _ <- forkIO (replayLoop tuiHandle stateRef strategy candles)
    runTUI tuiHandle

replayLoop :: TuiHandle -> IORef BacktestState -> Strategy -> [MarketData] -> IO ()
replayLoop _         _        _        []        = return ()
replayLoop tuiHandle stateRef strategy (md:rest) = do
    oldState <- readIORef stateRef
    let dec      = strategy md
        newState = stepBacktest (\_ -> dec) oldState md
    writeIORef stateRef newState
    updateTUI tuiHandle newState md dec
    threadDelay replayDelayUs
    replayLoop tuiHandle stateRef strategy rest

-- ─────────────────────────────────────────────
--  CSV parsing  (mirrors CsvReplay.hs logic)
-- ─────────────────────────────────────────────

mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' _ []     = []
mapMaybe' f (x:xs) = case f x of
    Nothing -> mapMaybe' f xs
    Just y  -> y : mapMaybe' f xs

-- Expected format: timestamp,open,high,low,close,decision
parseCsvRow :: String -> Maybe MarketData
parseCsvRow str =
    case splitOn ',' str of
        [t, o, h, l, c, _] -> do
            time     <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (take 19 t)
            openVal  <- readMaybe o
            highVal  <- readMaybe h
            lowVal   <- readMaybe l
            closeVal <- readMaybe c
            return MarketData
                { timestamp  = time
                , openPrice  = openVal
                , highPrice  = highVal
                , lowPrice   = lowVal
                , closePrice = closeVal
                }
        _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn sep s =
    case break (== sep) s of
        (a, [])        -> [a]
        (a, _ : rest)  -> a : splitOn sep rest
