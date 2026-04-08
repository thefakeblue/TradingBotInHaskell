module Strategies
    ( simpleStrategy
    ) where

import Backtest

simpleStrategy :: Strategy -- basic strategy for testing
simpleStrategy marketData
    | closePrice marketData > openPrice marketData = Buy 1
    | closePrice marketData < openPrice marketData = Sell 1
    | otherwise = Hold

-- add strategies here. can have user toggle through them and backtest