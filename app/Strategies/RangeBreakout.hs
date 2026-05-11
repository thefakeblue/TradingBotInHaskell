module Strategies.RangeBreakout
    ( rangeBreakoutStrategy
    ) where

import Backtest

-- | Buy when close is near the candle high (strong close),
-- sell when close is near the candle low (weak close).
--
-- proximity: 0.0 - 1.0
--   0.25 means top/bottom 25% of the candle range
--   lower = more selective, fewer trades
rangeBreakoutStrategy :: Double -> Strategy
rangeBreakoutStrategy proximity md
    | range == 0                    = Hold
    | posInRange >= (1 - proximity) = Buy  1
    | posInRange <= proximity       = Sell 1
    | otherwise                     = Hold
  where
    range      = highPrice md - lowPrice md
    posInRange = (closePrice md - lowPrice md) / range
