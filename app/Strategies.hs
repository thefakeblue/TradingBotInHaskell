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

-------------- STRATEGIES (combining decisions): -------------------
-- only buy if multiple (determined) strategies give buy signal (sell if one or multiple give sell signal)
-- only buy if the majority of strategies give buy signal (can weigh certain votes)
-- BEST OPTION: use certain strategies for changing market conditions (ex: momentum strategy in strong trends, mean reversion in ranging markets, etc)
-- filter: only use a certain strategy if certain conditions are met (ex: if user want to use MAC, only allow it if volatility is high enough) otherwise hold









------------ STRATEGY DECISIONS to implement in strategies depending on market conditions, voting, etc. -------------

-- 1. Momentum strategy: buy if the price has been going up for the last k candles, sell if it's been going down
-- 2. Moving average crossover: buy if the short term moving average crosses above the long
-- 3. Mean reversion: buy if the price is below the moving average, sell if it's above
-- 4. Breakout: buy if the price breaks above the high of the last k candles, sell if it breaks below the low

-- (1)
momentumStrategyDecision :: Int -> [MarketData] -> Decision -- buy if price has been going up for last k candles, sell if it's been going down
momentumStrategyDecision k history = -- k should probably depend on size of the candles (5 min, 10 etc.)
    case momentumOver k history of
        Nothing -> Hold
        Just m -- can change this to recommend different quantities based on the strength of the momentum, but for now just buy/sell 1 unit
        -- considering making final trade decision amounts to be based on how many strategy decisions recommend it, ex: if 3 strategies recommend buy, buy 3 units.
             | if m > 0 then Buy 1
             | else if m < 0 then Sell 1
             | else Hold

-- (2)
movingAverageCrossoverDecision :: Int -> Int -> [MarketData] -> Decision -- buy if short term moving average crosses above long term, sell if it crosses below
movingAverageCrossoverDecision shortN longN history =
    case movingAverageCrossover shortN longN history of
        Nothing -> Hold

        Just ordering ->
            case ordering of
                GT -> Buy 1
                LT -> Sell 1
                EQ -> Hold

-- (3)
meanReversionDecision :: Int -> [MarketData] -> Decision -- buy if price is below moving average, sell if it's above
meanReversionDecision n history = -- price > avg -> sell. price < avg -> buy. price = avg -> hold. can also add a threshold for how far above/below the price is from the average to avoid false signals, but for now just simple comparison
    case aboveMovingAverage n history of
        Nothing -> Hold
        Just above ->
            if above
            then Sell 1
            else Buy 1

-- (4)
breakoutDecision :: Int -> [MarketData] -> Decision -- buy if price breaks above high of last k candles, sell if it breaks below low
breakoutDecision k history
    | length history < k+1 = Hold
    | otherwise =
        let currentCandle = last history
            previousCandles = lastNCandles k (init history) -- get last k candles excluding current one

            currentPrice = closePrice currentCandle
            highs = map highPrice previousCandles
            lows = map lowPrice previousCandles

            maxHigh = maximum highs
            minLow = minimum lows
        in if currentPrice > maxHigh
            then Buy 1
            else if currentPrice < minLow
                then Sell 1
                else Hold 







------------- shared helper methods for strategies ---------------

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

isBullish :: MarketData -> Bool -- prices rising, optimism high
isBullish md = closePrice md > openPrice md

isBearish :: MarketData -> Bool -- price declining
isBearish md = closePrice md < openPrice md

--might want to take into account the size of the candle, not just direction.
candleRange :: MarketData -> Double -- range of the candle, can indicate volatility
candleRange md = highPrice md - lowPrice md

averageRange :: Int -> [MarketData] -> Maybe Double -- average range of last n candles, can indicate overall volatility
averageRange n xs
    | length xs < n = Nothing
    | otherwise =
        let recent = lastNCandles n xs
            ranges = map candleRange recent
        in Just (sum ranges / fromIntegral n)

aboveMovingAverage :: Int -> [MarketData] -> Maybe Bool -- is the current price above the moving average, can indicate bullishness
aboveMovingAverage n xs =
    case movingAverage n xs of
        Nothing -> Nothing
        Just avg -> Just (closePrice (last xs) > avg)

belowMovingAverage :: Int -> [MarketData] -> Maybe Bool -- is the current price below the moving average, can indicate bearishness
belowMovingAverage n xs =
    case movingAverage n xs of
        Nothing -> Nothing
        Just avg -> Just (closePrice (last xs) < avg)

movingAverageCrossover :: Int -> Int -> [MarketData] -> Maybe Ordering -- has the short term moving average crossed above the long term moving average, can indicate bullish crossover
movingAverageCrossover shortN longN xs =
    case (movingAverage shortN xs, movingAverage longN xs) of
        (Just shortAvg, Just longAvg) -> Just (compare shortAvg longAvg)
        _ -> Nothing

determineK :: Int -> Int -> Int -- for momentumStrategy. might want to hardcode lookbackMinutes (30?) so user isn't overwhelmed with nitty gritty decisions
determineK lookbackMinutes candleMinutes =
    max 1 (lookbackMinutes `div` candleMinutes) -- determine k based on lookback time and candle size, at least 1

