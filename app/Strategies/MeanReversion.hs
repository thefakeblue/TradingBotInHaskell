module Strategies.MeanReversion
    ( meanReversionStrategy
    ) where

import Backtest

-- | Fade strong moves, expecting price to snap back.
-- Strong up candle -> Sell (overbought). Strong down candle -> Buy (oversold).
--
-- Typical values:
--   1-min  chart: 0.0005 - 0.002
--   10-min chart: 0.002  - 0.005
meanReversionStrategy :: Double -> Strategy
meanReversionStrategy threshold md
    | relMove >  threshold = Sell 1
    | relMove < -threshold = Buy  1
    | otherwise            = Hold
  where
    relMove = (closePrice md - openPrice md) / openPrice md
