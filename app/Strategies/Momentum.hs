module Strategies.Momentum
    ( momentumStrategy
    ) where

import Backtest

-- | Only act when the move is larger than a threshold.
-- Filters out tiny, noisy candles.
-- Threshold is expressed as a fraction of open price (e.g. 0.001 = 0.1%).
--
-- Typical values:
--   1-min  chart: 0.0005 - 0.002
--   5-min  chart: 0.001  - 0.003
--   10-min chart: 0.002  - 0.005
momentumStrategy :: Double -> Strategy
momentumStrategy threshold md
    | relMove >  threshold = Buy  1
    | relMove < -threshold = Sell 1
    | otherwise            = Hold
  where
    relMove = (closePrice md - openPrice md) / openPrice md
