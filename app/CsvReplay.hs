module Main where

import Text.Read (readMaybe)
import Backtest
import Strategies
import Data.Time

data TimeFrame
    = AllTime
    | DateRange UTCTime UTCTime
    | LastMinutes Integer

main :: IO ()
main = do
    -- Choose one data file by uncommenting it:
    let dataFile = "1YearHistoricalData1Min.csv"
    -- let dataFile = "1YearHistoricalData3Min.csv"
    -- let dataFile = "1YearHistoricalData5Min.csv"
    -- dataFile = "1YearHistoricalData10Min.csv"

    -- Choose one strategy by uncommenting it and commenting the others:
    -- let strategy = simpleStrategy
    -- let strategy = momentumStrategy 0.002
    -- let strategy = meanReversionStrategy 0.002
    -- let strategy = rangeBreakoutStrategy 0.25
    -- let strategy = customTrendBreakoutStrategy 0.25 0.002
    let strategy = customRangeReversionStrategy 0.45 0.25 -- best live strategy on 10-minute data
    -- let strategy = customRangeReversionConservative 0.45 0.25
    -- let strategy = stepRSIStrategy 30 70
    -- let strategy = stepMAStrategy 9 21
    -- let strategy = stepEMABreakoutStrategy 12

    let strategyLabel = "customRangeReversionStrategy 0.45 0.25"

    putStrLn $ "Loading data file: " ++ dataFile
    putStrLn $ "Using strategy: " ++ strategyLabel

    contents <- readFile dataFile
    let rows = lines contents
    let marketDataList = mapMaybe parseCsvRow rows
    let finalState = runBacktest strategy marketDataList

    let wins   = winningTrades finalState
    let losses = losingTrades  finalState
    let total  = totalTrades   finalState
    let winRate = if total == 0 then 0
                  else fromIntegral wins / fromIntegral total * 100 :: Double
    let profitFactor = if grossLoss finalState == 0 then 999
                       else grossProfit finalState / grossLoss finalState
    let finalQty = quantityOwned finalState
    let lastPrice = if null marketDataList then 0 else closePrice (last marketDataList)
    let finalEquity = cash finalState + finalQty * lastPrice

    putStrLn "────────────────────────────"
    putStrLn $ "Data file:      " ++ dataFile
    putStrLn $ "Strategy:       " ++ strategyLabel
    putStrLn $ "Net profit:     " ++ show (netProfit finalState)
    putStrLn $ "Gross profit:   " ++ show (grossProfit finalState)
    putStrLn $ "Gross loss:     " ++ show (grossLoss finalState)
    putStrLn $ "Profit factor:  " ++ show profitFactor
    putStrLn $ "Win rate:       " ++ show winRate ++ "%"
    putStrLn $ "Wins:           " ++ show wins
    putStrLn $ "Losses:         " ++ show losses
    putStrLn $ "Total trades:   " ++ show total
    putStrLn $ "Final cash:     " ++ show (cash finalState)
    putStrLn $ "Quantity held:  " ++ show finalQty
    putStrLn $ "Final equity:   " ++ show finalEquity
    putStrLn "────────────────────────────"

parseTimeFrame :: [String] -> [MarketData] -> IO TimeFrame -- lets user determine what candles to look back at
parseTimeFrame [] _ = return AllTime
parseTimeFrame ["last", minStr] _ =
    case readMaybe minStr of
        Just mins -> return (LastMinutes mins)
        Nothing -> do
            putStrLn "Invalid minutes argument. Use an integer, e.g. 'last 10'"
            return AllTime

-- helper
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
    case f x of
        Nothing -> mapMaybe f xs
        Just y  -> y : mapMaybe f xs


filterByTimeFrame :: TimeFrame -> [MarketData] -> [MarketData] -- filter the candles based on the time frame the user wants to look at
filterByTimeFrame AllTime xs = xs

filterByTimeFrame (DateRange start end) xs =
    filter (\md -> timestamp md >= start && timestamp md <= end) xs

filterByTimeFrame (LastMinutes mins) xs = -- get the latest timestamp, then filter for candles within the last n minutes of that timestamp
    case xs of
        [] -> []
        _  ->
            let latestTime = maximum (map timestamp xs)
                startTime = addUTCTime (fromInteger (-mins * 60)) latestTime
            in filter (\md -> timestamp md >= startTime && timestamp md <= latestTime) xs


-- parse CSV row: timestamp,open,high,low,close,DECISION
-- We IGNORE the decision and recompute it
parseCsvRow :: String -> Maybe MarketData -- takes a row of the csv and turns it into a MarketData type, which we can use to backtest our strategy
parseCsvRow str =
    case splitComma str of
        [t,o,h,l,c,_] -> do          -- 6 columns, timestamp first
            time     <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (take 19 t)
            openVal  <- readMaybe o
            highVal  <- readMaybe h
            lowVal   <- readMaybe l
            closeVal <- readMaybe c
            return MarketData
                { timestamp  = time
                , openPrice  = openVal
                , highPrice  = highVal
                , lowPrice   = lowVal
                , closePrice = closeVal
                }
        _ -> Nothing

parseTimeStamp :: String -> Maybe UTCTime -- parses the timestamp string into a UTCTime type, which we can use to filter by time frame
parseTimeStamp t =
    readMaybe t

splitComma :: String -> [String] -- takes a comma separated string and splits it into a list of strings, one for each value
splitComma s =
    case break (== ',') s of
        (a, []) -> [a]
        (a, _:rest) -> a : splitComma rest