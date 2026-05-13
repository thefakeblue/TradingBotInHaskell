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

    -- ── SNAP-NoFlip: profitFlip=False — pure flat exits, no in-position flips ─
    -- Prevents direct long→short or short→long transitions.  Position always goes
    -- FLAT between sides.  Fewer shorts during uptrends because there's no
    -- profitFlip trigger; a short can only enter from a fresh flat state with cooldown.
    putStrLn "\n=================================================="
    putStrLn   "  SNAP-NoFlip  profitFlip=False  (target: 20d>=400  wr>=70%)"
    putStrLn   "=================================================="
    let snapNoFlipSweep =
          [ (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 0.001,  5)
          , (0.45, 0.25,  5, 13, 20, 0.0005, 0.001, 0.0005, 5)
          , (0.45, 0.25,  3,  8, 20, 0.0005, 0.001, 0.001,  5)
          , (0.45, 0.25,  5, 13, 30, 0.0005, 0.001, 0.001,  5)
          , (0.45, 0.35,  5, 13, 20, 0.0005, 0.001, 0.001,  5)
          , (0.40, 0.25,  5, 13, 20, 0.0005, 0.001, 0.001,  0)
          ]
    mapM_ (\(br,pr,fp,sp,mh,tt,hs,tp,cd) -> do
        let run mds = fst $ runStatefulBacktest
                          (StatefulStrategy initialSNAPState (stepSNAP br pr fp sp mh tt hs tp cd False))
                          mds
        let lbl = "br=" ++ show br ++ " pr=" ++ show pr
               ++ " f"  ++ show fp  ++ "/s" ++ show sp
               ++ " hs=" ++ show hs ++ " tp=" ++ show tp
               ++ " mh=" ++ show mh ++ " cd=" ++ show cd ++ " NoFlip"
        putStrLn $ "\n  [" ++ lbl ++ "]"
        printResult "  1yr-1min"  4000 65 (run mds1yr)
        printResult "  1yr-10min"  800 65 (run mds10yr)
        printResult "  20d-1min"   400 65 (run mds20d)
      ) snapNoFlipSweep

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

    -- ── PRIMO-Safe: high win-rate, small avg loss ──────────────────────────────
    -- trailPct=0 → Safe mode: fixed TP + signalExit (new dip at profit → close).
    -- strictTrend=True: only enter when EMA cross is positive (blocks neutral regime).
    -- Tight hard stop keeps avg loss small.  Target: WR >= 70%, net >= 300 on 20d.
    putStrLn "\n=================================================="
    putStrLn   "  PRIMO-Safe  high WR  (target: 20d>=300  wr>=70%)"
    putStrLn   "=================================================="
    let primoSafeSweep =
          -- (br, pr, fastP, slowP, tt, hs, tp, trail, mh, cd, strict)
          -- trail=0.0 + tp>0 → Safe mode: fixed TP + signalExit (new dip at profit → close).
          -- strict=True → only enter when EMA cross is positive (trendDir > 0)
          -- strict=False → enter when EMA cross is neutral or positive (trendDir >= 0)
          [ (0.40, 0.25, 5, 13, 0.0003, 0.0008, 0.0010, 0.0, 12, 3, False)  -- baseline loose
          , (0.40, 0.25, 5, 13, 0.0003, 0.0008, 0.0012, 0.0, 12, 3, False)  -- wider tp
          , (0.40, 0.25, 5, 13, 0.0003, 0.0006, 0.0008, 0.0, 12, 3, False)  -- tighter hs
          , (0.40, 0.25, 5, 13, 0.0003, 0.0006, 0.0010, 0.0, 12, 3, False)  -- tighter hs bigger tp
          , (0.40, 0.25, 5, 13, 0.0003, 0.0008, 0.0010, 0.0, 12, 2, False)  -- cd=2
          , (0.40, 0.25, 5, 13, 0.0003, 0.0008, 0.0010, 0.0, 15, 3, False)  -- mh=15
          , (0.40, 0.30, 5, 13, 0.0003, 0.0008, 0.0010, 0.0, 12, 3, False)  -- wider pr
          , (0.35, 0.30, 5, 13, 0.0003, 0.0008, 0.0010, 0.0, 12, 3, False)  -- wider br+pr
          , (0.40, 0.25, 3,  8, 0.0003, 0.0008, 0.0010, 0.0, 12, 3, False)  -- f3/s8 loose
          -- strict=True variants (higher WR, fewer trades)
          , (0.40, 0.25, 5, 13, 0.0003, 0.0008, 0.0008, 0.0, 12, 3, True)
          , (0.40, 0.25, 5, 13, 0.0003, 0.0008, 0.0010, 0.0, 12, 3, True)
          , (0.40, 0.25, 3,  8, 0.0003, 0.0008, 0.0008, 0.0, 12, 3, True)
          ]
    mapM_ (\(br,pr,fp,sp,tt,hs,tp,trail,mh,cd,strict) -> do
        let run mds = fst $ runStatefulBacktest
                          (StatefulStrategy initialPRIMOState
                              (stepPRIMO br pr fp sp tt hs tp trail mh cd strict))
                          mds
        let lbl = "br=" ++ show br ++ " pr=" ++ show pr
               ++ " f"  ++ show fp  ++ "/s" ++ show sp
               ++ " tt=" ++ show tt
               ++ " hs=" ++ show hs ++ " tp=" ++ show tp
               ++ " mh=" ++ show mh ++ " cd=" ++ show cd
               ++ if strict then " STRICT" else ""
        putStrLn $ "\n  [" ++ lbl ++ "]"
        printResult "  1yr-1min"  2000 65 (run mds1yr)
        printResult "  1yr-10min"  400 65 (run mds10yr)
        printResult "  20d-1min"   300 70 (run mds20d)
      ) primoSafeSweep

    -- ── PRIMO-Max: maximum net profit, trailing stop ────────────────────────────
    -- trailPct > 0 → Max mode: no fixed TP, trailing stop lets winners run.
    -- Trail fires when close drops trailPct below peak (only after peak > entry).
    -- Hard stop still acts as absolute floor.  signalExit is disabled in Max mode.
    -- Target: WR >= 60%, net >= 400 on 20d.
    putStrLn "\n=================================================="
    putStrLn   "  PRIMO-Max  high net  (target: 20d>=400  wr>=60%)"
    putStrLn   "=================================================="
    let primoMaxSweep =
          -- (br, pr, fastP, slowP, tt, hs, tp, trail, mh, cd, strict)
          -- tp=0.0 trail=0.0 → Hold mode: signalExit disabled; exits via hs/trendBreak/maxHold only
          -- tp=0.0 trail>0.0 → Trailing stop mode
          -- Hold mode lets winners run; trendBreak (trendDir<0) and hard stop protect downside
          [ (0.40, 0.25, 5, 13, 0.0003, 0.0010, 0.0, 0.0, 20, 3, False)  -- hold mh=20
          , (0.40, 0.25, 5, 13, 0.0003, 0.0010, 0.0, 0.0, 30, 3, False)  -- hold mh=30
          , (0.40, 0.25, 5, 13, 0.0003, 0.0010, 0.0, 0.0, 40, 3, False)  -- hold mh=40
          , (0.40, 0.25, 5, 13, 0.0003, 0.0008, 0.0, 0.0, 30, 3, False)  -- tighter hs
          , (0.40, 0.25, 5, 13, 0.0003, 0.0008, 0.0, 0.0, 20, 3, False)  -- tighter hs mh=20
          , (0.40, 0.25, 3,  8, 0.0003, 0.0010, 0.0, 0.0, 20, 3, False)  -- f3/s8 hold
          , (0.40, 0.25, 3,  8, 0.0003, 0.0010, 0.0, 0.0, 30, 3, False)  -- f3/s8 mh=30
          , (0.35, 0.30, 5, 13, 0.0003, 0.0010, 0.0, 0.0, 25, 2, False)  -- wider entry
          , (0.40, 0.30, 5, 13, 0.0003, 0.0010, 0.0, 0.0, 25, 2, False)  -- wider pr
          , (0.40, 0.25, 5, 13, 0.0002, 0.0010, 0.0, 0.0, 25, 3, False)  -- looser tt
          , (0.40, 0.25, 5, 13, 0.0003, 0.0010, 0.0, 0.0, 25, 3, True)   -- strict hold
          -- trailing stop variants (trail>0 disables signalExit independently)
          , (0.40, 0.25, 5, 13, 0.0003, 0.0010, 0.0, 0.0010, 30, 3, False)  -- trail=0.001
          , (0.40, 0.25, 5, 13, 0.0003, 0.0010, 0.0, 0.0005, 30, 3, False)  -- trail=0.0005 tighter
          ]
    mapM_ (\(br,pr,fp,sp,tt,hs,tp,trail,mh,cd,strict) -> do
        let run mds = fst $ runStatefulBacktest
                          (StatefulStrategy initialPRIMOState
                              (stepPRIMO br pr fp sp tt hs tp trail mh cd strict))
                          mds
        let lbl = "br=" ++ show br ++ " pr=" ++ show pr
               ++ " f"  ++ show fp  ++ "/s" ++ show sp
               ++ " tt=" ++ show tt
               ++ " hs=" ++ show hs ++ " trail=" ++ show trail
               ++ " mh=" ++ show mh ++ " cd=" ++ show cd
               ++ if strict then " STRICT" else ""
        putStrLn $ "\n  [" ++ lbl ++ "]"
        printResult "  1yr-1min"  3000 60 (run mds1yr)
        printResult "  1yr-10min"  600 60 (run mds10yr)
        printResult "  20d-1min"   400 60 (run mds20d)
      ) primoMaxSweep

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
