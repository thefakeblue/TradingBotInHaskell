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
