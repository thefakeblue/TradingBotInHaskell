module Strategies.RSI
    ( RSIState(..)
    , initialRSIState
    , stepRSIStrategy
    , rsiStrategy
    ) where

import Backtest
import Data.List (foldl')

-- | RSI state: tracks rolling gains and losses close-to-close.
-- RSI < 30 -> oversold -> Buy
-- RSI > 70 -> overbought -> Sell
data RSIState = RSIState
    { rsiPeriod    :: Int
    , rsiGains     :: [Double]
    , rsiLosses    :: [Double]
    , rsiPrevClose :: Maybe Double
    } deriving (Show, Eq)

initialRSIState :: Int -> RSIState
initialRSIState n = RSIState
    { rsiPeriod    = n
    , rsiGains     = []
    , rsiLosses    = []
    , rsiPrevClose = Nothing
    }

computeRSI :: RSIState -> Maybe Double
computeRSI st
    | length (rsiGains st) < rsiPeriod st = Nothing
    | avgLoss == 0                         = Just 100
    | otherwise                            = Just (100 - 100 / (1 + rs))
  where
    n       = rsiPeriod st
    avgGain = sum (take n (rsiGains  st)) / fromIntegral n
    avgLoss = sum (take n (rsiLosses st)) / fromIntegral n
    rs      = avgGain / avgLoss

-- | Single step. Returns Hold until enough bars have accumulated.
-- oversold (e.g. 30) -> Buy, overbought (e.g. 70) -> Sell.
stepRSIStrategy :: Double -> Double -> RSIState -> MarketData -> (Decision, RSIState)
stepRSIStrategy oversold overbought st md =
    let price  = closePrice md
        newSt  = case rsiPrevClose st of
            Nothing   -> st { rsiPrevClose = Just price }
            Just prev ->
                let change    = price - prev
                    gain      = max change 0
                    loss      = abs (min change 0)
                    n         = rsiPeriod st
                    newGains  = take n (gain : rsiGains  st)
                    newLosses = take n (loss : rsiLosses st)
                in st { rsiGains     = newGains
                      , rsiLosses    = newLosses
                      , rsiPrevClose = Just price }
        decision = case computeRSI newSt of
            Nothing  -> Hold
            Just rsi
                | rsi < oversold   -> Buy  1
                | rsi > overbought -> Sell 1
                | otherwise        -> Hold
    in (decision, newSt)

-- | Convenience: run RSI over a list of candles.
-- For live trading use stepRSIStrategy with an IORef instead.
rsiStrategy :: Int -> Double -> Double -> [MarketData] -> [Decision]
rsiStrategy period oversold overbought =
    snd . foldl' step (initialRSIState period, [])
  where
    step (st, ds) md =
        let (d, st') = stepRSIStrategy oversold overbought st md
        in (st', ds ++ [d])
