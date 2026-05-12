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

        addLong  = (Buy  1, st1 { v2HoldCount = 0 })
        addShort = (Sell 1, st1 { v2HoldCount = 0 })

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
            else if rawBuy                then addLong  -- same-dir: accumulate like baseline

            else                               stay

        V2Short ->
            let allowFlip = closeP <= entryPx * (1 + stopLossPct)
            in
            if      rawBuy && allowFlip   then goLong   -- within tolerance: instant flip
            else if rawBuy                then goFlat   -- deep loss: CLOSE only
            else if maxHit                then goFlat
            else if rawSell               then addShort -- same-dir: accumulate like baseline
            else                               stay
