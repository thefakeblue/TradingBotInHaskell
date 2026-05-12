module Main where

import Text.Read (readMaybe)
import Backtest
import Strategies
import Data.Time

main :: IO ()
main = do
    yr1min  <- readFile "1YearHistoricalData1Min.csv"
    yr10min <- readFile "1YearHistoricalData10Min.csv"
    d20min  <- readFile "HistoricalData1MinPAST20Days.csv"

    let parse f = mapMaybe parseCsvRow (lines f)
    let mds1yr  = parse yr1min
    let mds10yr = parse yr10min
    let mds20d  = parse d20min

    -- ── Baseline: RRV2 (pure range reversion, no trend filter) ────────────────
    putStrLn "\n=================================================="
    putStrLn   "  BASELINE  RRV2"
    putStrLn   "=================================================="
    let rrv2Base mds = fst $ runStatefulBacktest
                          (StatefulStrategy initialRRV2State (stepRRV2 0.45 0.25 0 0 0.005))
                          mds
    printResult "1yr-1min"  7000 64 (rrv2Base mds1yr)
    printResult "1yr-10min" 1500 64 (rrv2Base mds10yr)
    printResult "20d-1min"   400 60 (rrv2Base mds20d)

    -- ── TARR best-known (reference) ────────────────────────────────────────────
    putStrLn "\n=================================================="
    putStrLn   "  TARR  best-known  cd=5 bb=off"
    putStrLn   "=================================================="
    let tarrBest mds = fst $ runStatefulBacktest
                          (StatefulStrategy initialTARRState
                              (stepTARR 0.45 0.25 10 25 20 0.005 0.0005 0.001 5 0))
                          mds
    printResult "1yr-1min"  7000 64 (tarrBest mds1yr)
    printResult "1yr-10min" 1500 64 (tarrBest mds10yr)
    printResult "20d-1min"   400 60 (tarrBest mds20d)

    -- ── TARR-P + TP: profit-only flips + take-profit ───────────────────────────
    -- stopLossPct=0  →  flip ONLY when the closing trade is at profit.
    -- takeProfitPct > 0  →  lock in gains before the move can reverse (boosts WR).
    -- Combined target: 20d >= 600  AND  wr >= 70%.
    putStrLn "\n=================================================="
    putStrLn   "  TARR-P+TP  profit-flips + take-profit  (target: 20d>=600 wr>=70%)"
    putStrLn   "=================================================="
    let tarrPsweep =
          -- (bodyRatio, proximity, fastP, slowP, maxHold, trendThresh, hardStop, cooldown, takeProfit)
          [ (0.45, 0.25, 10, 25, 20, 0.0005, 0.001, 5, 0.0003)  -- f10/s25 small tp
          , (0.45, 0.25, 10, 25, 20, 0.0005, 0.001, 5, 0.0005)  -- f10/s25 tp=5e-4
          , (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 5, 0.0003)  -- f5/s13 small tp
          , (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 5, 0.0005)  -- f5/s13 tp=5e-4
          , (0.45, 0.25,  8, 21, 20, 0.0005, 0.001, 5, 0.0003)  -- f8/s21 small tp
          , (0.45, 0.25,  8, 21, 20, 0.0005, 0.001, 5, 0.0005)  -- f8/s21 tp=5e-4
          , (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 3, 0.0005)  -- cd=3
          , (0.45, 0.25,  5, 13, 30, 0.0005, 0.001, 5, 0.0005)  -- mh=30
          , (0.40, 0.25,  5, 13, 20, 0.0005, 0.001, 5, 0.0005)  -- br=0.40
          , (0.45, 0.25, 10, 25, 20, 0.0005, 0.001, 5, 0.0)     -- no tp (plain TARR-P)
          ]
    mapM_ (\(br,pr,fp,sp,mh,tt,hs,cd,tp) -> do
        let run mds = fst $ runStatefulBacktest
                          (StatefulStrategy initialTARRState
                              (stepTARR br pr fp sp mh 0.0 tt hs cd tp))
                          mds
        let lbl = "br=" ++ show br ++ " f" ++ show fp ++ "/s" ++ show sp
               ++ " tt=" ++ show tt ++ " hs=" ++ show hs
               ++ " tp=" ++ show tp
               ++ " mh=" ++ show mh ++ " cd=" ++ show cd
        putStrLn $ "\n  [" ++ lbl ++ "]"
        printResult "  1yr-1min"  5000 70 (run mds1yr)
        printResult "  1yr-10min" 1000 70 (run mds10yr)
        printResult "  20d-1min"   600 70 (run mds20d)
      ) tarrPsweep

    -- ── SNAP+ sweep: profit-flip enabled (target: 20d>=600  wr>=70%) ────────────
    -- profitFlip=True: when a reversal signal fires while the position is AT PROFIT,
    -- flip direction instead of going flat.  Loss exits still go flat (SNAP rule).
    -- This preserves the high WR (only flip from winning positions) while capturing
    -- the reversal move that SNAP would otherwise miss.
    putStrLn "\n=================================================="
    putStrLn   "  SNAP+  profit-flip enabled  (target: 20d>=600  wr>=70%)"
    putStrLn   "=================================================="
    let snapPlusSweep =
          -- (bodyRatio, proximity, fastP, slowP, maxHold, trendThresh, hardStop, takeProfit, cooldown)
          [ (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 0.0005,  5)  -- f5/s13 hybrid tp
          , (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 0.001,   5)  -- f5/s13 profit-only tp
          , (0.45, 0.25,  8, 21, 20, 0.0005, 0.001, 0.0005,  5)  -- f8/s21 hybrid tp
          , (0.45, 0.25,  8, 21, 20, 0.0005, 0.001, 0.001,   5)  -- f8/s21 profit-only tp
          , (0.45, 0.25,  5, 13, 30, 0.0005, 0.001, 0.0005,  5)  -- mh=30
          , (0.45, 0.25,  5, 13, 30, 0.0005, 0.001, 0.001,   5)  -- mh=30 profit-only tp
          , (0.40, 0.25,  5, 13, 20, 0.0005, 0.001, 0.0005,  5)  -- br=0.40
          , (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 0.0005,  3)  -- cd=3
          , (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 0.0005,  0)  -- cd=0
          ]
    mapM_ (\(br,pr,fp,sp,mh,tt,hs,tp,cd) -> do
        let run mds = fst $ runStatefulBacktest
                          (StatefulStrategy initialSNAPState (stepSNAP br pr fp sp mh tt hs tp cd True))
                          mds
        let lbl = "br=" ++ show br ++ " pr=" ++ show pr
               ++ " f"  ++ show fp  ++ "/s" ++ show sp
               ++ " hs=" ++ show hs ++ " tp=" ++ show tp
               ++ " mh=" ++ show mh ++ " cd=" ++ show cd
        putStrLn $ "\n  [" ++ lbl ++ "]"
        printResult "  1yr-1min"  5000 70 (run mds1yr)
        printResult "  1yr-10min" 1000 70 (run mds10yr)
        printResult "  20d-1min"   600 70 (run mds20d)
      ) snapPlusSweep

    -- ── SNAP+ extended sweep: push 20d toward 600 ─────────────────────────────
    putStrLn "\n=================================================="
    putStrLn   "  SNAP+  extended sweep  (target: 20d>=600  wr>=70%)"
    putStrLn   "=================================================="
    let snapPlusExt =
          [ (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 0.001,   0)  -- cd=0 + tp=0.001
          , (0.45, 0.35,  5, 13, 20, 0.0005, 0.001, 0.001,   5)  -- pr=0.35
          , (0.45, 0.35,  5, 13, 20, 0.0005, 0.001, 0.001,   0)  -- pr=0.35 cd=0
          , (0.35, 0.25,  5, 13, 20, 0.0005, 0.001, 0.001,   5)  -- br=0.35
          , (0.40, 0.25,  5, 13, 20, 0.0005, 0.001, 0.001,   0)  -- br=0.40 cd=0
          , (0.45, 0.25,  5, 13, 20, 0.0003, 0.001, 0.001,   5)  -- trendThresh=0.0003
          , (0.45, 0.25,  5, 13, 20, 0.0001, 0.001, 0.001,   5)  -- trendThresh=0.0001
          , (0.45, 0.25,  3,  8, 20, 0.0005, 0.001, 0.001,   5)  -- faster EMA f3/s8
          , (0.45, 0.25,  3,  8, 20, 0.0005, 0.001, 0.0005,  5)  -- f3/s8 hybrid tp
          , (0.45, 0.25, 20, 20, 20, 0.0005, 0.001, 0.001,   5)  -- fastP=slowP (no EMA filter)
          , (0.45, 0.25, 20, 20, 20, 0.0005, 0.001, 0.0005,  5)  -- no EMA filter hybrid tp
          ]
    mapM_ (\(br,pr,fp,sp,mh,tt,hs,tp,cd) -> do
        let run mds = fst $ runStatefulBacktest
                          (StatefulStrategy initialSNAPState (stepSNAP br pr fp sp mh tt hs tp cd True))
                          mds
        let lbl = "br=" ++ show br ++ " pr=" ++ show pr
               ++ " f"  ++ show fp  ++ "/s" ++ show sp
               ++ " tt=" ++ show tt
               ++ " hs=" ++ show hs ++ " tp=" ++ show tp
               ++ " mh=" ++ show mh ++ " cd=" ++ show cd
        putStrLn $ "\n  [" ++ lbl ++ "]"
        printResult "  1yr-1min"  5000 70 (run mds1yr)
        printResult "  1yr-10min" 1000 70 (run mds10yr)
        printResult "  20d-1min"   600 70 (run mds20d)
      ) snapPlusExt

-- ── helpers ────────────────────────────────────────────────────────────────

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
        ok    = if netI >= targetNet && wrI >= targetWr then "OK" else "  "
    putStrLn $ ok ++ "  " ++ label
            ++ "  net=" ++ show netI
            ++ "  wr="  ++ show wrI ++ "%"
            ++ "  trades=" ++ show total
            ++ "  pf=" ++ show (fromIntegral (round (pf * 100) :: Int) / 100.0 :: Double)

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
