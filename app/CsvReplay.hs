module Main where

import Text.Read (readMaybe)
import Backtest
import Strategies
import Data.Time

main :: IO ()
main = do
    let dataFiles = [ ("1YearHistoricalData1Min.csv",  7000, 64)
                    , ("1YearHistoricalData10Min.csv", 1500, 64)
                    ]

    -- baseline
    putStrLn "\n=== BASELINE: customRangeReversionStrategy 0.45 0.25 ==="
    mapM_ (\(f,tp,tw) -> do
        contents <- readFile f
        let mds = mapMaybe parseCsvRow (lines contents)
        let fs  = runBacktest (customRangeReversionStrategy 0.45 0.25) mds
        putStrLn $ "\n-- " ++ f ++ " --"
        printResult "0.45/0.25 baseline" tp tw fs
      ) dataFiles

    -- V2 sweep: bodyRatio, proximity, emaPeriod, maxHold, stopLossPct
    -- stopLossPct=0   → only flip if profitable (strictest)
    -- stopLossPct=0.003 → flip if loss <0.3% (recommended)
    -- stopLossPct=999  → always flip (= original)
    let sweepParams =
          [ (0.45, 0.25, 0,  0, 0.003)
          , (0.45, 0.25, 0,  0, 0.005)
          , (0.45, 0.25, 0,  0, 0.01 )
          , (0.45, 0.25, 0,  0, 999.0)
          , (0.40, 0.25, 0,  0, 0.003)
          , (0.40, 0.25, 0,  0, 0.005)
          , (0.40, 0.25, 0,  0, 0.01 )
          , (0.40, 0.30, 0,  0, 0.003)
          , (0.40, 0.30, 0,  0, 0.005)
          , (0.45, 0.30, 0,  0, 0.003)
          , (0.45, 0.30, 0,  0, 0.005)
          , (0.45, 0.25, 0, 20, 0.005)
          ]

    putStrLn "\n=== customRangeReversionV2 parameter sweep ==="
    mapM_ (\(f,tp,tw) -> do
        putStrLn $ "\n-- " ++ f ++ " --"
        contents <- readFile f
        let mds = mapMaybe parseCsvRow (lines contents)
        mapM_ (\(br,pr,ep,mh,sl) -> do
            let strat = StatefulStrategy initialRRV2State (stepRRV2 br pr ep mh sl)
            let (fs, _) = runStatefulBacktest strat mds
            printResult (show br ++ "/prox" ++ show pr ++ "/sl" ++ show sl ++ "/mh" ++ show mh) tp tw fs
          ) sweepParams
      ) dataFiles

printResult :: String -> Int -> Int -> BacktestState -> IO ()
printResult label targetNet targetWr st = do
    let wins  = winningTrades st
        total = totalTrades st
        wr    = if total == 0 then 0.0
                else fromIntegral wins / fromIntegral total * 100 :: Double
        pf    = if grossLoss st == 0 then 999.0
                else grossProfit st / grossLoss st
        netI  = round (netProfit st) :: Int
        wrI   = round wr :: Int
        ok    = if netI >= targetNet && wrI >= targetWr then " OK" else " --"
    putStrLn $ "  [" ++ label ++ "]"
            ++ "  net=" ++ show netI
            ++ "  wr="  ++ show wrI ++ "%"
            ++ "  trades=" ++ show total
            ++ "  pf=" ++ show (fromIntegral (round (pf * 100) :: Int) / 100.0 :: Double)
            ++ ok

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Nothing -> mapMaybe f xs
    Just y  -> y : mapMaybe f xs

parseCsvRow :: String -> Maybe MarketData
parseCsvRow str =
    case splitComma str of
        [t,o,h,l,c,_] -> do
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

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
    (a, [])     -> [a]
    (a, _:rest) -> a : splitComma rest
