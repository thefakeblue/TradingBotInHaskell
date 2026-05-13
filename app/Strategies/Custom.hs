module Strategies.Custom
    ( customTrendBreakoutStrategy
    , customRangeReversionStrategy
    , customRangeReversionConservative
    , CustomATRState(..)
    , initialATRState
    , stepATRTrendStrategy
    , customEmaBreakoutStrategy
    , CustomEMAState(..)
    , initialEMAState
    , stepEMABreakoutStrategy
    -- V2: stateful range reversion with EMA filter, re-entry, and close-only flips
    , V2Pos(..)
    , RRV2State(..)
    , initialRRV2State
    , stepRRV2
    -- TARR: Trend-Aware Range Reversion (dual-EMA regime filter)
    , TARRState(..)
    , initialTARRState
    , stepTARR
    -- SNAP: Scalp with No Alternation Protocol
    , SNAPState(..)
    , initialSNAPState
    , stepSNAP
    -- PRIMO: Pullback-Reversion In Momentum Only (Long-Only)
    , PRIMOState(..)
    , initialPRIMOState
    , stepPRIMO
    ) where

import Backtest
import Data.List (foldl')

-- | Trend breakout with momentum and strong close.
-- Buy when the candle is bullish, closes near the high, and is larger than recent average range.
-- Sell when the candle is bearish, closes near the low, and is larger than recent average range.
customTrendBreakoutStrategy :: Double -> Double -> Strategy
customTrendBreakoutStrategy proximity threshold md
    | range == 0 = Hold
    | bullish && closeNearHigh && strongMove = Buy 1
    | bearish && closeNearLow  && strongMove = Sell 1
    | otherwise = Hold
  where
    openP = openPrice md
    closeP = closePrice md
    highP = highPrice md
    lowP = lowPrice md
    range = highP - lowP
    body = closeP - openP
    relMove = body / max 1 openP
    posInRange = (closeP - lowP) / range
    bullish = body > 0
    bearish = body < 0
    closeNearHigh = posInRange >= (1 - proximity)
    closeNearLow = posInRange <= proximity
    strongMove = abs relMove >= threshold

-- | Range reversion that fades extreme candles.
-- Sell when close is near the high after a long bullish candle.
-- Buy when close is near the low after a long bearish candle.
customRangeReversionStrategy :: Double -> Double -> Strategy
customRangeReversionStrategy bodyRatio proximity md
    | range == 0 = Hold
    | bullish && bodyRatio' >= bodyRatio && closeNearHigh = Sell 1
    | bearish && bodyRatio' >= bodyRatio && closeNearLow  = Buy 1
    | otherwise = Hold
  where
    openP = openPrice md
    closeP = closePrice md
    highP = highPrice md
    lowP = lowPrice md
    range = highP - lowP
    body = abs (closeP - openP)
    bodyRatio' = body / range
    posInRange = (closeP - lowP) / range
    bullish = closeP > openP
    bearish = closeP < openP
    closeNearHigh = posInRange >= (1 - proximity)
    closeNearLow = posInRange <= proximity

-- | Conservative range reversion: less frequent shorts, stronger buy signals.
-- Only goes short if body ratio is VERY high (0.65+) and close is pinned to high.
-- Buys more frequently on reversals from low.
customRangeReversionConservative :: Double -> Double -> Strategy
customRangeReversionConservative bodyRatio proximity md
    | range == 0 = Hold
    | bearish && bodyRatio' >= bodyRatio && closeNearLow = Buy 1
    | bullish && bodyRatio' >= (bodyRatio + 0.15) && closeNearHigh = Sell 1
    | otherwise = Hold
  where
    openP = openPrice md
    closeP = closePrice md
    highP = highPrice md
    lowP = lowPrice md
    range = highP - lowP
    body = abs (closeP - openP)
    bodyRatio' = body / range
    posInRange = (closeP - lowP) / range
    bullish = closeP > openP
    bearish = closeP < openP
    closeNearHigh = posInRange >= (1 - proximity)
    closeNearLow = posInRange <= proximity

-- | ATR-based trend strategy keeps a rolling ATR and looks for strong range breakouts.
-- Buy when price closes above the high range and ATR is growing.
-- Sell when price closes below the low range and ATR is growing.

data CustomATRState = CustomATRState
    { atrPrevCloses :: [Double]
    , atrPrevHighs  :: [Double]
    , atrPrevLows   :: [Double]
    , atrValues     :: [Double]
    } deriving (Show, Eq)

initialATRState :: CustomATRState
initialATRState = CustomATRState [] [] [] []

stepATRTrendStrategy
    :: Int -- ATR period
    -> Double -- ATR growth threshold relative to average
    -> CustomATRState
    -> MarketData
    -> (Decision, CustomATRState)
stepATRTrendStrategy period atrGrowth st md
    | length prices < period + 1 = (Hold, newSt)
    | otherwise = (decision, newSt)
  where
    closeP = closePrice md
    highP = highPrice md
    lowP = lowPrice md
    prevCloses = closeP : atrPrevCloses st
    prevHighs = highP : atrPrevHighs st
    prevLows = lowP : atrPrevLows st
    prices = prevCloses
    bars = zip3 prevHighs prevLows prevCloses
    trs = zipWith (\new@(h,l,_) old@(_,_,pc) -> maximum [h-l, abs (h-pc), abs (l-pc)]) bars (tail bars)
    atr = sum (take period trs) / fromIntegral period
    atrHistory = take period (atr : atrValues st)
    avgATR = sum atrHistory / fromIntegral (length atrHistory)
    trendHigh = closeP > highP - 0.05 * (highP - lowP)
    trendLow = closeP < lowP + 0.05 * (highP - lowP)
    atrGrowing = avgATR > 0 && atr >= avgATR * atrGrowth
    decision
        | trendHigh && atrGrowing = Buy 1
        | trendLow  && atrGrowing = Sell 1
        | otherwise = Hold
    newSt = CustomATRState prevCloses prevHighs prevLows atrHistory

-- | EMA breakout strategy.
-- Buy when price reclaims the EMA after a dip; sell when price drops below EMA after a high.

data CustomEMAState = CustomEMAState
    { emaPrices :: [Double]
    , emaPrev   :: Maybe Double
    } deriving (Show, Eq)

initialEMAState :: CustomEMAState
initialEMAState = CustomEMAState [] Nothing

calcEMA :: Int -> [Double] -> Maybe Double
calcEMA n prices
    | length prices < n = Nothing
    | otherwise =
        let k = 2.0 / fromIntegral (n + 1)
            seed = sum (take n (reverse (take n prices))) / fromIntegral n
            newer = take (length prices - n) prices
        in Just $ foldl' (\ema p -> p * k + ema * (1 - k)) seed (reverse newer)

stepEMABreakoutStrategy
    :: Int
    -> CustomEMAState
    -> MarketData
    -> (Decision, CustomEMAState)
stepEMABreakoutStrategy period st md
    | otherwise = (decision, newSt)
  where
    closeP = closePrice md
    newPrices = take (period + 50) (closeP : emaPrices st)
    currEMA = calcEMA period newPrices
    prevEMA = emaPrev st
    decision = case (prevEMA, currEMA) of
        (Just pe, Just ce)
            | pe > closeP && closeP > ce -> Buy 1
            | pe < closeP && closeP < ce -> Sell 1
            | otherwise -> Hold
        _ -> Hold
    newSt = CustomEMAState newPrices currEMA

customEmaBreakoutStrategy :: Int -> CustomEMAState -> MarketData -> (Decision, CustomEMAState)
customEmaBreakoutStrategy = stepEMABreakoutStrategy

-- ─────────────────────────────────────────────────────────────────────────────
-- V2: Stateful range reversion — solves the NinjaTrader "instant flip" problem.
--
-- Problems with the original stateless strategy in live trading:
--   • NinjaTrader's EnterLong/EnterShort automatically closes the opposite side,
--     so SELL while long = close long AND instantly open short in one candle.
--   • This causes "switches to sell when it should just do another buy".
--
-- How V2 fixes this:
--   1. Tracks position state (Long / Short / Flat) internally.
--   2. Opposite-direction signal → sends CLOSE this candle, re-enters opposite
--      direction NEXT candle.  NinjaTrader sees: CLOSE → (hold 1 candle) → SELL.
--      No more same-candle flip.
--   3. Same-direction signal while PROFITABLE → Close (lock in P&L), re-enter
--      same direction next candle ("repo in the same direction").
--   4. Same-direction signal while LOSING → HOLD (don't realise the loss,
--      wait for price to recover or the opposite signal to exit).
--   5. maxHold > 0: forced time-based exit to prevent runaway losses.
-- ─────────────────────────────────────────────────────────────────────────────

data V2Pos = V2Flat | V2Long | V2Short deriving (Show, Eq)

data RRV2State = RRV2State
    { v2Pos       :: V2Pos
    , v2EntryPx   :: Double
    , v2HoldCount :: Int
    , v2Prices    :: [Double]   -- close history for optional EMA
    } deriving (Show, Eq)

initialRRV2State :: RRV2State
initialRRV2State = RRV2State V2Flat 0 0 []

-- | Hybrid stateful range-reversion strategy.
--
-- Behaviour:
--   • Small-loss or profitable flip → instant direction change (like original, high performance)
--   • Deep-loss flip  → CLOSE only (go flat, avoids the "huge loss" double-flip the user reports)
--   • Same-dir signal → HOLD (let winners run, don't truncate with a repo)
--   • maxHold > 0     → forced flat exit after N candles
--   • emaPeriod > 0   → EMA filter for flat entries only
--
-- stopLossPct controls the deep-loss threshold:
--   0.0   = only flip if profitable (strictest)
--   0.003 = allow flip if loss < 0.3% (recommended for most setups)
--   999   = always flip instantly (= original behaviour)
stepRRV2
    :: Double -> Double -> Int -> Int -> Double
    -> RRV2State -> MarketData
    -> (Decision, RRV2State)
stepRRV2 bodyRatio proximity emaPeriod maxHold stopLossPct st md =
    let closeP = closePrice md
        openP  = openPrice  md
        highP  = highPrice  md
        lowP   = lowPrice   md

        newPrices = if emaPeriod > 0
                    then take (emaPeriod + 60) (closeP : v2Prices st)
                    else []
        ema       = if emaPeriod > 0 then calcEMA emaPeriod newPrices else Nothing
        st1       = st { v2Prices = newPrices }

        emaOkBuy  = case ema of { Nothing -> True; Just e -> closeP <= e }
        emaOkSell = case ema of { Nothing -> True; Just e -> closeP >= e }

        range      = highP - lowP
        body       = abs (closeP - openP)
        bodyR      = if range == 0 then 0 else body / range
        posInRange = if range == 0 then 0.5 else (closeP - lowP) / range

        rawBuy  = closeP < openP && bodyR >= bodyRatio && posInRange <= proximity
        rawSell = closeP > openP && bodyR >= bodyRatio && posInRange >= (1 - proximity)

        entryPx  = v2EntryPx st1
        hc       = v2HoldCount st1
        maxHit   = maxHold > 0 && hc >= maxHold

        goLong  = (Buy  1, st1 { v2Pos = V2Long,  v2EntryPx = closeP, v2HoldCount = 0 })
        goShort = (Sell 1, st1 { v2Pos = V2Short, v2EntryPx = closeP, v2HoldCount = 0 })
        goFlat  = (Close,  st1 { v2Pos = V2Flat,  v2HoldCount = 0 })
        stay    = (Hold,   st1 { v2HoldCount = hc + 1 })

    in case v2Pos st1 of

        V2Flat ->
            if      rawBuy  && emaOkBuy  then goLong
            else if rawSell && emaOkSell then goShort
            else                              (Hold, st1)

        V2Long ->
            -- flip is allowed if loss is within stopLossPct; only CLOSE on deep losses
            let allowFlip = closeP >= entryPx * (1 - stopLossPct)
            in
            if      rawSell && allowFlip  then goShort  -- within tolerance: instant flip
            else if rawSell               then goFlat   -- deep loss: CLOSE only, no short
            else if maxHit                then goFlat
            else                               stay     -- same-dir or neutral: hold position

        V2Short ->
            let allowFlip = closeP <= entryPx * (1 + stopLossPct)
            in
            if      rawBuy && allowFlip   then goLong   -- within tolerance: instant flip
            else if rawBuy                then goFlat   -- deep loss: CLOSE only
            else if maxHit                then goFlat
            else                               stay     -- same-dir or neutral: hold position

-- ─────────────────────────────────────────────────────────────────────────────
-- TARR V1: Trend-Aware Range Reversion
--
-- Problem with pure range reversion (RRV2): it shorts every bullish extreme and
-- buys every bearish extreme.  In a strong trend it bleeds: it fades each new
-- high/low, gets stopped out, repeats — death by a thousand small losses.
--
-- Fix: dual-EMA regime filter.
--   fast EMA > slow EMA  (uptrend)   → only long (buy-pullback) entries
--   fast EMA < slow EMA  (downtrend) → only short (sell-rally) entries
--   EMAs nearly equal    (neutral)   → full two-way reversion as before
--
-- trendThresh controls sensitivity: min |fastEMA - slowEMA| / price to call a
-- trend.  0.0002 ≈ 1.5 pts at 7400 — catches sustained directional moves while
-- ignoring one-bar noise.
-- ─────────────────────────────────────────────────────────────────────────────

data TARRState = TARRState
    { tarrPos      :: V2Pos
    , tarrEntryPx  :: Double
    , tarrHoldCount :: Int
    , tarrPrices   :: [Double]   -- recent closes (newest first)
    , tarrShortCd  :: Int        -- cooldown: bars before re-entering short after adverse exit
    , tarrLongCd   :: Int        -- cooldown: bars before re-entering long after adverse exit
    } deriving (Show, Eq)

initialTARRState :: TARRState
initialTARRState = TARRState V2Flat 0 0 [] 0 0

stepTARR
    :: Double  -- bodyRatio    e.g. 0.45
    -> Double  -- proximity    e.g. 0.25
    -> Int     -- fast EMA period  e.g. 10
    -> Int     -- slow EMA period  e.g. 25
    -> Int     -- maxHold  (0 = disabled; skipped when trend is with position)
    -> Double  -- stopLossPct  flip tolerance: 0 = flip only at profit, 0.005 = allow flips within 0.5% loss
    -> Double  -- trendThresh  min (fastEMA-slowEMA)/price to call a trend e.g. 0.0005
    -> Double  -- hardStopPct  price-based stop-loss e.g. 0.001  (0 = off)
    -> Int     -- adverseCooldown  bars to block same-dir re-entry after hard-stop/trend exit (0 = off)
    -> Double  -- takeProfitPct  lock-in gains when price moves this far in our favour (0 = off)
    -> TARRState -> MarketData
    -> (Decision, TARRState)
stepTARR bodyRatio proximity fastP slowP maxHold stopLossPct trendThresh hardStopPct cooldown takeProfitPct st md =
    let closeP = closePrice md
        openP  = openPrice  md
        highP  = highPrice  md
        lowP   = lowPrice   md

        newPrices = take (slowP + 60) (closeP : tarrPrices st)
        fastEMA   = calcEMA fastP newPrices
        slowEMA   = calcEMA slowP newPrices

        shortCd = max 0 (tarrShortCd st - 1)
        longCd  = max 0 (tarrLongCd  st - 1)

        st1 = st { tarrPrices = newPrices, tarrShortCd = shortCd, tarrLongCd = longCd }

        trendDir = case (fastEMA, slowEMA) of
            (Just fe, Just se)
                | (fe - se) / closeP >  trendThresh -> ( 1 :: Int)
                | (fe - se) / closeP < -trendThresh -> (-1 :: Int)
                | otherwise                          ->  0
            _ -> 0

        range      = highP - lowP
        body       = abs (closeP - openP)
        bodyR      = if range == 0 then 0 else body / range
        posInRange = if range == 0 then 0.5 else (closeP - lowP) / range

        rawBuy  = closeP < openP && bodyR >= bodyRatio && posInRange <= proximity
        rawSell = closeP > openP && bodyR >= bodyRatio && posInRange >= (1 - proximity)

        filtBuy  = rawBuy  && trendDir >= 0 && longCd  == 0
        filtSell = rawSell && trendDir <= 0 && shortCd == 0

        entryPx = tarrEntryPx st1
        hc      = tarrHoldCount st1

        longHardStop   = hardStopPct   > 0 && entryPx > 0 && closeP <= entryPx * (1 - hardStopPct)
        shortHardStop  = hardStopPct   > 0 && entryPx > 0 && closeP >= entryPx * (1 + hardStopPct)
        longTakeProfit = takeProfitPct > 0 && entryPx > 0 && closeP >= entryPx * (1 + takeProfitPct)
        shortTakeProfit= takeProfitPct > 0 && entryPx > 0 && closeP <= entryPx * (1 - takeProfitPct)

        goLong  = (Buy  1, st1 { tarrPos = V2Long,  tarrEntryPx = closeP, tarrHoldCount = 0 })
        goShort = (Sell 1, st1 { tarrPos = V2Short, tarrEntryPx = closeP, tarrHoldCount = 0 })
        goFlat  = (Close,  st1 { tarrPos = V2Flat,  tarrHoldCount = 0 })
        goCoolShort = (Close, st1 { tarrPos = V2Flat, tarrHoldCount = 0, tarrShortCd = cooldown })
        goCoolLong  = (Close, st1 { tarrPos = V2Flat, tarrHoldCount = 0, tarrLongCd  = cooldown })
        stay    = (Hold,   st1 { tarrHoldCount = hc + 1 })

    in case tarrPos st1 of

        V2Flat ->
            if      filtBuy  then goLong
            else if filtSell then goShort
            else                  (Hold, st1)

        V2Long ->
            let allowFlip   = closeP >= entryPx * (1 - stopLossPct)
                trendWithUs = trendDir > 0
                maxHit      = maxHold > 0 && hc >= maxHold && not trendWithUs
            in
            if      longHardStop          then goCoolLong
            else if longTakeProfit        then goFlat
            else if filtSell && allowFlip then goShort
            else if filtSell              then goFlat
            else if maxHit                then goFlat
            else                               stay

        V2Short ->
            let allowFlip    = closeP <= entryPx * (1 + stopLossPct)
                trendAgainst = trendDir > 0
                maxHit       = maxHold > 0 && hc >= maxHold
            in
            if      shortHardStop         then goCoolShort
            else if shortTakeProfit       then goFlat
            else if trendAgainst          then goCoolShort
            else if filtBuy && allowFlip  then goLong
            else if filtBuy               then goFlat
            else if maxHit                then goFlat
            else                               stay

-- ─────────────────────────────────────────────────────────────────────────────
-- SNAP: Scalp with No Alternation Protocol
--
-- Problems with TARR:
--   • Flips directly long→short on the same candle when a reversal fires.
--   • In a rising market with neutral EMA gap, it keeps alternating directions.
--   • User wants Long→Long when trend is up, not Long→Short→Long.
--
-- How SNAP fixes this:
--   1. NO in-position flips.  To go the other direction the strategy must first
--      go FLAT, then a fresh entry fires.  Long → flat → flat/long (never L→S).
--   2. Explicit take-profit.  Exits winning positions quickly rather than
--      waiting for a reversal signal.  Fewer "win turns into loss" trades →
--      higher win rate.
--   3. EMA trend filter blocks entries against the trend (same as TARR).
--   4. Cooldown after hard-stop or trend-against exit blocks same-dir re-entry.
--
-- Expected behaviour:
--   • Oscillating market: rapid open/close cycles capturing small moves → high
--     trade count with good win rate.
--   • Trending up: shorts filtered out (trendDir=1), longs take-profit and
--     re-enter on each pullback candle → consecutive longs, no shorts.
-- ─────────────────────────────────────────────────────────────────────────────

data SNAPState = SNAPState
    { snapPos     :: V2Pos
    , snapEntry   :: Double
    , snapHolds   :: Int
    , snapPrices  :: [Double]
    , snapShortCd :: Int
    , snapLongCd  :: Int
    } deriving (Show, Eq)

initialSNAPState :: SNAPState
initialSNAPState = SNAPState V2Flat 0 0 [] 0 0

stepSNAP
    :: Double  -- bodyRatio       e.g. 0.45
    -> Double  -- proximity       e.g. 0.25
    -> Int     -- fast EMA period e.g. 10
    -> Int     -- slow EMA period e.g. 25
    -> Int     -- maxHold  candles before forced exit (0 = off)
    -> Double  -- trendThresh     e.g. 0.0005
    -> Double  -- hardStopPct     e.g. 0.001  (~7.4 pts at 7400)
    -> Double  -- takeProfitPct   e.g. 0.0005 (~3.7 pts)  (0 = off)
    -> Int     -- cooldown        bars to block re-entry after hard-stop / trend exit
    -> Bool    -- profitFlip: when True, a reversal signal while AT PROFIT flips direction
               --   instead of going flat.  False = pure SNAP (always go flat first).
    -> SNAPState -> MarketData
    -> (Decision, SNAPState)
stepSNAP bodyRatio proximity fastP slowP maxHold trendThresh hardStopPct takeProfitPct cooldown profitFlip st md =
    let closeP = closePrice md
        openP  = openPrice  md
        highP  = highPrice  md
        lowP   = lowPrice   md

        newPrices = take (slowP + 60) (closeP : snapPrices st)
        fastEMA   = calcEMA fastP newPrices
        slowEMA   = calcEMA slowP newPrices

        shortCd   = max 0 (snapShortCd st - 1)
        longCd    = max 0 (snapLongCd  st - 1)
        st1       = st { snapPrices = newPrices, snapShortCd = shortCd, snapLongCd = longCd }

        trendDir  = case (fastEMA, slowEMA) of
            (Just fe, Just se)
                | (fe - se) / closeP >  trendThresh -> ( 1 :: Int)
                | (fe - se) / closeP < -trendThresh -> (-1 :: Int)
                | otherwise                          ->  0
            _ -> 0

        range      = highP - lowP
        body       = abs (closeP - openP)
        bodyR      = if range == 0 then 0 else body / range
        posInRange = if range == 0 then 0.5 else (closeP - lowP) / range

        rawBuy  = closeP < openP && bodyR >= bodyRatio && posInRange <= proximity
        rawSell = closeP > openP && bodyR >= bodyRatio && posInRange >= (1 - proximity)

        filtBuy  = rawBuy  && trendDir >= 0 && longCd  == 0
        filtSell = rawSell && trendDir <= 0 && shortCd == 0

        entryPx = snapEntry st1
        hc      = snapHolds st1
        maxHit  = maxHold > 0 && hc >= maxHold

        longHardStop   = hardStopPct   > 0 && entryPx > 0 && closeP <= entryPx * (1 - hardStopPct)
        longTakeProfit = takeProfitPct > 0 && entryPx > 0 && closeP >= entryPx * (1 + takeProfitPct)

        shortHardStop   = hardStopPct   > 0 && entryPx > 0 && closeP >= entryPx * (1 + hardStopPct)
        shortTakeProfit = takeProfitPct > 0 && entryPx > 0 && closeP <= entryPx * (1 - takeProfitPct)

        goLong      = (Buy  1, st1 { snapPos = V2Long,  snapEntry = closeP, snapHolds = 0 })
        goShort     = (Sell 1, st1 { snapPos = V2Short, snapEntry = closeP, snapHolds = 0 })
        goFlat      = (Close,  st1 { snapPos = V2Flat,  snapHolds = 0 })
        goCoolShort = (Close,  st1 { snapPos = V2Flat,  snapHolds = 0, snapShortCd = cooldown })
        goCoolLong  = (Close,  st1 { snapPos = V2Flat,  snapHolds = 0, snapLongCd  = cooldown })
        stay        = (Hold,   st1 { snapHolds = hc + 1 })

    in case snapPos st1 of

        V2Flat ->
            if      filtBuy  then goLong
            else if filtSell then goShort
            else                  (Hold, st1)

        V2Long ->
            -- halfStop = entryPx * (1 - takeProfitPct):
            --   tp <  hs  →  signal exits small losses AND profits (hybrid)
            --   tp >= hs  →  hard stop fires before halfStop; signal exits only at profit
            -- profitFlip: when True, a sell signal at profit flips to Short instead of going Flat.
            --   False (default SNAP) = always go Flat, never flip in-position.
            let trendWithUs   = trendDir > 0
                timeExit      = maxHit && not trendWithUs
                trendExit     = trendDir < 0
                halfStop      = entryPx * (1 - takeProfitPct)
                atProfit      = closeP >= entryPx
                signalExit    = filtSell && (atProfit || closeP <= halfStop)
            in
            if      longHardStop                       then goCoolLong
            else if longTakeProfit                     then goFlat
            else if trendExit                          then goCoolLong
            else if profitFlip && filtSell && atProfit then goShort
            else if signalExit                         then goFlat
            else if timeExit                           then goFlat
            else                                            stay

        V2Short ->
            let trendWithUs   = trendDir < 0
                timeExit      = maxHit && not trendWithUs
                trendExit     = trendDir > 0
                halfStop      = entryPx * (1 + takeProfitPct)
                atProfit      = closeP <= entryPx
                signalExit    = filtBuy && (atProfit || closeP >= halfStop)
            in
            if      shortHardStop                      then goCoolShort
            else if shortTakeProfit                    then goFlat
            else if trendExit                          then goCoolShort
            else if profitFlip && filtBuy && atProfit  then goLong
            else if signalExit                         then goFlat
            else if timeExit                           then goFlat
            else                                            stay

-- ─────────────────────────────────────────────────────────────────────────────
-- PRIMO: Pullback-Reversion In Momentum Only (Long-Only)
--
-- Two modes controlled by trailPct:
--
--   PRIMO-Safe  (trailPct = 0)  — high win-rate, small losses
--     • Fixed take-profit (takeProfitPct) caps wins to keep WR high.
--     • Signal exit: new dip fires while in profit → close immediately
--       (dynamic peak-capture; this is what pushes WR above 65%).
--     • Tight hard stop (hardStopPct small) keeps avg loss small.
--     • strictTrend=True recommended.
--
--   PRIMO-Max   (trailPct > 0)  — maximum net, WR ≥ 60%
--     • No fixed TP.  Position rides the trend until a trailing stop fires.
--     • Trailing stop: tracks the highest close seen since entry (peak);
--       exits when close falls trailPct below that peak AND peak was already
--       above entry (i.e., we locked in some profit).
--     • Hard stop still acts as the absolute floor while below entry.
--     • Signal exit disabled — let winners run.
--     • Average winner can be 15-30 pts on strong moves.
--
-- Entry (both modes):
--   • Bearish dip candle (close < open, body ≥ bodyRatio, close in bottom
--     [proximity] of range).
--   • close ≥ slowEMA — downtrend guard (no entries below slow EMA).
--   • EMA cross: trendDir ≥ 0 (loose) or > 0 (strictTrend).
--   • No active cooldown.
-- ─────────────────────────────────────────────────────────────────────────────

data PRIMOState = PRIMOState
    { primoInLong   :: Bool
    , primoEntry    :: Double
    , primoPeak     :: Double   -- highest close since entry (for trailing stop)
    , primoHolds    :: Int
    , primoPrices   :: [Double]
    , primoCooldown :: Int
    } deriving (Show, Eq)

initialPRIMOState :: PRIMOState
initialPRIMOState = PRIMOState False 0 0 0 [] 0

stepPRIMO
    :: Double  -- bodyRatio       e.g. 0.40
    -> Double  -- proximity       e.g. 0.25
    -> Int     -- fast EMA period e.g. 5
    -> Int     -- slow EMA period e.g. 13
    -> Double  -- trendThresh     e.g. 0.0003
    -> Double  -- hardStopPct     e.g. 0.0008  (absolute floor; fires regardless of peak)
    -> Double  -- takeProfitPct   e.g. 0.0006  (Safe mode only; 0 = disabled)
    -> Double  -- trailPct        e.g. 0.0015  (Max mode; 0 = disabled → Safe mode)
    -> Int     -- maxHold         e.g. 12
    -> Int     -- cooldown        e.g. 3
    -> Bool    -- strictTrend: True = only enter when trendDir > 0 (Safe default)
    -> PRIMOState -> MarketData
    -> (Decision, PRIMOState)
stepPRIMO bodyRatio proximity fastP slowP trendThresh hardStopPct takeProfitPct trailPct maxHold cooldown strictTrend st md =
    let closeP = closePrice md
        openP  = openPrice  md
        highP  = highPrice  md
        lowP   = lowPrice   md

        newPrices = take (slowP + 60) (closeP : primoPrices st)
        fastEMA   = calcEMA fastP newPrices
        slowEMA   = calcEMA slowP newPrices
        cd        = max 0 (primoCooldown st - 1)
        st1       = st { primoPrices = newPrices, primoCooldown = cd }

        trendDir = case (fastEMA, slowEMA) of
            (Just fe, Just se)
                | (fe - se) / closeP >  trendThresh -> ( 1 :: Int)
                | (fe - se) / closeP < -trendThresh -> (-1 :: Int)
                | otherwise                          ->  0
            _ -> 0

        range      = highP - lowP
        body       = abs (closeP - openP)
        bodyR      = if range == 0 then 0 else body / range
        posInRange = if range == 0 then 0.5 else (closeP - lowP) / range

        dipSignal = closeP < openP && bodyR >= bodyRatio && posInRange <= proximity

        trendOk = if strictTrend then trendDir > 0 else trendDir >= 0

        priceAboveEma = case slowEMA of
            Nothing -> False
            Just se -> closeP >= se

        pullbackBuy = dipSignal && trendOk && priceAboveEma && cd == 0

        entryPx  = primoEntry st1
        hc       = primoHolds st1
        maxHit   = maxHold > 0 && hc >= maxHold

        -- Track the highest close since entry (used by trailing stop)
        newPeak = if primoInLong st1 then max closeP (primoPeak st1) else 0
        st2     = st1 { primoPeak = newPeak }

        -- Hard stop: absolute floor, fires whether above or below entry
        hardStop = hardStopPct > 0 && entryPx > 0
                && closeP <= entryPx * (1 - hardStopPct)

        -- Safe mode exits (trailPct == 0)
        -- Fixed TP: closes at a known gain
        takeProfit  = trailPct == 0 && takeProfitPct > 0 && entryPx > 0
                   && closeP >= entryPx * (1 + takeProfitPct)
        -- Signal exit: new dip appears while at any profit → grab the gain
        -- Only active in Safe mode (takeProfitPct > 0); tp=0 disables this for hold/max modes.
        signalExit  = trailPct == 0 && takeProfitPct > 0 && dipSignal && entryPx > 0 && closeP > entryPx

        -- Max mode exit (trailPct > 0)
        -- Trailing stop: fires only after peak has moved above entry (profit locked)
        -- then closes when price pulls back trailPct from that peak
        trailStop   = trailPct > 0 && newPeak > entryPx
                   && closeP <= newPeak * (1 - trailPct)

        trendBreak  = trendDir < 0

        goLong     = (Buy  1, st2 { primoInLong = True,  primoEntry = closeP
                                  , primoPeak = closeP, primoHolds = 0 })
        goFlat     = (Close,  st2 { primoInLong = False, primoEntry = 0
                                  , primoPeak = 0, primoHolds = 0 })
        goCoolFlat = (Close,  st2 { primoInLong = False, primoEntry = 0
                                  , primoPeak = 0, primoHolds = 0
                                  , primoCooldown = cooldown })
        stay       = (Hold,   st2 { primoHolds = hc + 1 })

    in if primoInLong st1
        then
            if      hardStop   then goCoolFlat  -- always first
            else if trailStop  then goFlat       -- locked profit, trail pulled us out
            else if takeProfit then goFlat       -- safe-mode fixed TP
            else if signalExit then goFlat       -- safe-mode dynamic peak capture
            else if trendBreak then goCoolFlat   -- trend reversed, exit + cooldown
            else if maxHit     then goFlat
            else                    stay
        else
            if pullbackBuy then goLong
            else                (Hold, st2 { primoInLong = False })
