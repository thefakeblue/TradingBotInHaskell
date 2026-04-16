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



-------- shared helper methods for strategies ----------

closePrices :: [MarketData] -> [Double]
closePrices = map closePrice

lastNCandles :: Int -> [a] -> [a]
lastNCandles n xs = drop (length xs - min n (length xs)) xs -- get last n candles, or all if less than n

movingAverage :: Int -> [MarketData] -> Maybe Double
movingAverage n xs
    | length xs < n = Nothing
    | otherwise =
        let closes = closePrices (lastNCandles n xs)
        in Just (sum closes / fromIntegral n)

momentumOver :: Int -> [MarketData] -> Maybe Double -- momentum over k candles
momentumOver k xs
    | length xs < k+1 = Nothing
    | otherwise =
        let recent = lastNCandles (k+1) xs
        in Just (closePrice (last recent) - closePrice (head recent)) -- momentum = change in price over k candles