module Strategies.Simple
    ( simpleStrategy
    ) where

import Backtest

-- | Original simple strategy: green candle -> Buy, red -> Sell
simpleStrategy :: Strategy
simpleStrategy md
    | closePrice md > openPrice md = Buy 1
    | closePrice md < openPrice md = Sell 1
    | otherwise                    = Hold
