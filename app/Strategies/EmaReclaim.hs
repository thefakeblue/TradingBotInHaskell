module Strategies.EmaReclaim
    ( EmaReclaimState(..)
    , initialEmaReclaimState
    , ADXState(..)
    , initialADXState
    , stepEmaReclaimStrategy
    , emaReclaimStrategy
    ) where

import Backtest
import Data.List (foldl')

-- | Translated from C# NinjaTrader strategy: MESVolatilityBreakout_V14
-- Entry: price dips below EMA then reclaims it, with trend/volatility filters.

-- ── Indicator helpers ─────────────────────────────────────────

calcEMA :: Int -> [Double] -> Maybe Double
calcEMA n prices
    | length prices < n = Nothing
    | otherwise =
        let k     = 2.0 / fromIntegral (n + 1)
            seed  = sum (take n (reverse (take n prices))) / fromIntegral n
            newer = take (length prices - n) prices
        in Just $ foldl' (\ema p -> p * k + ema * (1 - k)) seed (reverse newer)

trueRange :: Double -> Double -> Double -> Double -> Double
trueRange high low prevClose _ =
    maximum [ high - low
            , abs (high - prevClose)
            , abs (low  - prevClose)
            ]

calcATR :: Int -> [(Double, Double, Double)] -> Maybe Double
calcATR n bars
    | length bars < n + 1 = Nothing
    | otherwise =
        let trs = zipWith (\(h,l,_) (_,_,pc) -> trueRange h l pc 0)
                          bars (tail bars)
        in calcEMA n trs

-- ── ADX indicator ─────────────────────────────────────────────

data ADXState = ADXState
    { adxValues    :: [Double]
    , adxPrevHigh  :: Maybe Double
    , adxPrevLow   :: Maybe Double
    , adxPrevClose :: Maybe Double
    , adxPlusDMs   :: [Double]
    , adxMinusDMs  :: [Double]
    , adxTRs       :: [Double]
    } deriving (Show, Eq)

initialADXState :: ADXState
initialADXState = ADXState [] Nothing Nothing Nothing [] [] []

stepADX :: Int -> ADXState -> Double -> Double -> Double -> (Maybe Double, ADXState)
stepADX period st high low close =
    case (adxPrevHigh st, adxPrevLow st, adxPrevClose st) of
        (Nothing, _, _) ->
            (Nothing, st { adxPrevHigh  = Just high
                         , adxPrevLow   = Just low
                         , adxPrevClose = Just close })
        (Just ph, Just pl, Just pc) ->
            let tr         = trueRange high low pc 0
                plusDM     = if high - ph > pl - low && high - ph > 0 then high - ph else 0
                minusDM    = if pl - low > high - ph && pl - low > 0  then pl - low  else 0
                newTRs     = take period (tr     : adxTRs     st)
                newPlusDMs = take period (plusDM : adxPlusDMs st)
                newMinusDMs= take period (minusDM: adxMinusDMs st)
                newSt = st { adxPrevHigh  = Just high
                           , adxPrevLow   = Just low
                           , adxPrevClose = Just close
                           , adxTRs       = newTRs
                           , adxPlusDMs   = newPlusDMs
                           , adxMinusDMs  = newMinusDMs }
            in if length newTRs < period
               then (Nothing, newSt)
               else
                   let sumTR   = sum newTRs
                       plusDI  = 100 * sum newPlusDMs  / sumTR
                       minusDI = 100 * sum newMinusDMs / sumTR
                       dx      = if plusDI + minusDI == 0 then 0
                                 else 100 * abs (plusDI - minusDI) / (plusDI + minusDI)
                       newADXs = take period (dx : adxValues newSt)
                       adxVal  = if length newADXs < period then Nothing
                                 else Just (sum newADXs / fromIntegral period)
                   in (adxVal, newSt { adxValues = newADXs })
        _ -> (Nothing, st)

-- ── EMA Reclaim state ─────────────────────────────────────────

data EmaReclaimState = EmaReclaimState
    { erPrices         :: [Double]
    , erHighs          :: [Double]
    , erLows           :: [Double]
    , erATRs           :: [Double]
    , erADXState       :: ADXState
    , erBarsSinceTrade :: Int
    } deriving (Show, Eq)

initialEmaReclaimState :: EmaReclaimState
initialEmaReclaimState = EmaReclaimState
    { erPrices         = []
    , erHighs          = []
    , erLows           = []
    , erATRs           = []
    , erADXState       = initialADXState
    , erBarsSinceTrade = 999
    }

-- ── Strategy step ─────────────────────────────────────────────

stepEmaReclaimStrategy
    :: Int -> Int -> Int -> Int -> Double -> Int
    -> EmaReclaimState -> MarketData -> (Decision, EmaReclaimState)
stepEmaReclaimStrategy emaPeriod atrPeriod atrAvgPeriod adxPeriod adxThresh cooldown st md =
    let close = closePrice md
        high  = highPrice  md
        low   = lowPrice   md

        newPrices = take (emaPeriod + 250) (close : erPrices st)
        newHighs  = take (atrPeriod + 2)   (high  : erHighs  st)
        newLows   = take (atrPeriod + 2)   (low   : erLows   st)

        hlcBars = zip3 newHighs newLows newPrices
        newATR  = calcATR atrPeriod hlcBars
        newATRs = case newATR of
            Nothing -> erATRs st
            Just v  -> take atrAvgPeriod (v : erATRs st)
        atrAvg  = if length newATRs < atrAvgPeriod then Nothing
                  else Just (sum newATRs / fromIntegral atrAvgPeriod)

        (adxVal, newADXSt) = stepADX adxPeriod (erADXState st) high low close

        currEMA    = calcEMA emaPeriod newPrices
        prevEMA    = calcEMA emaPeriod (drop 10 newPrices)
        prevEMAVal = calcEMA emaPeriod (tail newPrices)

        signal = case (currEMA, prevEMAVal, adxVal, newATR, atrAvg) of
            (Just ce, Just pe, Just adx, Just atr, Just atrA) ->
                let emaRising  = case prevEMA of
                                     Just prev -> ce > prev
                                     Nothing   -> False
                    volOK      = atr > atrA
                    trendOK    = adx > adxThresh
                    prevDipped = case erLows st of
                                     (pl:_) -> pl < pe
                                     []     -> False
                    reclaimed  = close > ce
                    cooldownOK = erBarsSinceTrade st >= cooldown
                in emaRising && volOK && trendOK && prevDipped && reclaimed && cooldownOK
            _ -> False

        decision     = if signal then Buy 0.1 else Hold
        newBarsSince = if signal then 0 else erBarsSinceTrade st + 1

        newSt = st
            { erPrices         = newPrices
            , erHighs          = newHighs
            , erLows           = newLows
            , erATRs           = newATRs
            , erADXState       = newADXSt
            , erBarsSinceTrade = newBarsSince
            }
    in (decision, newSt)

-- | Default parameters matching the original C# version (EMA 20 for faster warmup).
emaReclaimStrategy :: EmaReclaimState -> MarketData -> (Decision, EmaReclaimState)
emaReclaimStrategy = stepEmaReclaimStrategy 20 14 30 14 15.0 3
