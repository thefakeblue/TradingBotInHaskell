module Strategies.MA
    ( MAState(..)
    , initialMAState
    , stepMAStrategy
    , maStrategy
    ) where

import Backtest
import Data.List (foldl')

-- | Moving average crossover state.
-- Golden cross (fast crosses above slow) -> Buy.
-- Death cross  (fast crosses below slow) -> Sell.
--
-- Common periods:
--   fast=9  slow=21  (aggressive)
--   fast=20 slow=50  (conservative)
data MAState = MAState
    { maFastPeriod :: Int
    , maSlowPeriod :: Int
    , maPrices     :: [Double]
    , maPrevFast   :: Maybe Double
    , maPrevSlow   :: Maybe Double
    } deriving (Show, Eq)

initialMAState :: Int -> Int -> MAState
initialMAState fast slow = MAState
    { maFastPeriod = fast
    , maSlowPeriod = slow
    , maPrices     = []
    , maPrevFast   = Nothing
    , maPrevSlow   = Nothing
    }

simpleMovingAverage :: Int -> [Double] -> Maybe Double
simpleMovingAverage n prices
    | length prices < n = Nothing
    | otherwise         = Just $ sum (take n prices) / fromIntegral n

stepMAStrategy :: MAState -> MarketData -> (Decision, MAState)
stepMAStrategy st md =
    let price     = closePrice md
        newPrices = take (maSlowPeriod st) (price : maPrices st)
        currFast  = simpleMovingAverage (maFastPeriod st) newPrices
        currSlow  = simpleMovingAverage (maSlowPeriod st) newPrices
        decision  = case (maPrevFast st, maPrevSlow st, currFast, currSlow) of
            (Just pf, Just ps, Just cf, Just cs)
                | pf <= ps && cf > cs -> Buy  1
                | pf >= ps && cf < cs -> Sell 1
            _                         -> Hold
        newSt = st { maPrices   = newPrices
                   , maPrevFast = currFast
                   , maPrevSlow = currSlow }
    in (decision, newSt)

-- | Convenience: run MA crossover over a list of candles.
-- For live trading use stepMAStrategy with an IORef instead.
maStrategy :: Int -> Int -> [MarketData] -> [Decision]
maStrategy fast slow =
    snd . foldl' step (initialMAState fast slow, [])
  where
    step (st, ds) md =
        let (d, st') = stepMAStrategy st md
        in (st', ds ++ [d])
