module Main where


-- to backtest specific candles:
-- cabal run csvReplay -- last 10 (for last 10 minutes)
-- cabal run CsvReplay -- from "2026-05-05 12:00:00 UTC" "2026-05-05 13:00:00 UTC"

-- ask user what mode they want: 
-- 1 = all data, 2 = last n minutes, 3 = specific date range from start to end time
-- read their input with getLine
--convert that input into a TimeFrame data type
--run the same filter/backtest code



import System.Environment (getArgs)
import Backtest
import Strategies
import Data.Time
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data TimeFrame -- can determine which candles to backtest on based on time.
    = AllTime
    | LastMinutes Integer
    | DateRange UTCTime UTCTime
    deriving (Show, Eq)

main :: IO ()
main = do
    args <- getArgs

    -- determine which file we're looking at (different csv files have different sized candles)
    putStrLn "Enter CSV file name, or press Enter for trades.csv:"
    putStr "> "
    fileNameInput <- getLine

    let fileName = -- will change if we want a different default file
         if null fileNameInput
         then "trades.csv"
         else fileNameInput

    contents <- readFile fileName
    putStrLn ("File used: " ++ fileName)

    let rows = lines contents -- split into rows
        marketDataList = mapMaybe parseCsvRow rows

    timeFrame <- -- ask user for time frame, or parse from args
        if null args
        then askUserForTimeFrame
        else parseTimeFrame args marketDataList

    let filteredData = filterByTimeFrame timeFrame marketDataList -- filter data based on time frame
        finalState = runBacktest simpleStrategy filteredData

    putStrLn ("Args: " ++ show args)
    putStrLn ("Time frame: " ++ show timeFrame)
    putStrLn ("Total candles: " ++ show (length marketDataList))
    putStrLn ("Candles used: " ++ show (length filteredData))

    putStrLn "Final Backtest State:"
    print finalState

parseTimeFrame :: [String] -> [MarketData] -> IO TimeFrame -- lets user determine what candles to look back at
parseTimeFrame [] _ = return AllTime
parseTimeFrame ["last", minStr] _ =
    case readMaybe minStr of
        Just mins -> return (LastMinutes mins)
        Nothing -> do
            putStrLn "Invalid minutes argument. Use an integer, e.g. 'last 10'"
            return AllTime

parseTimeFrame ["from", startStr, endStr] _ = -- parse start and end time
    case (parseTimeStamp startStr, parseTimeStamp endStr) of
        (Just start, Just end) -> return (DateRange start end)
        _ -> do
            putStrLn "Invalid date range. Use format: YYYY-MM-DDTHH:MM:SS"
            return AllTime

parseTimeFrame _ _ = do -- unrecognized args
    putStrLn "Usage:"
    putStrLn "  cabal run csvReplay -- last 10"
    putStrLn "  cabal run csvReplay -- from 2026-05-05T12:00:00 2026-05-05T13:00:00"
    putStrLn "Using all data."
    return AllTime

askUserForTimeFrame :: IO TimeFrame -- if no args, ask user for time frame interactively
askUserForTimeFrame = do
    putStrLn "Choose timeframe:"
    putStrLn "1 = All data"
    putStrLn "2 = Last n minutes (enter n)"
    putStrLn "3 = Date range"
    putStr "> "
    choice <- getLine

    case choice of 
        "1" -> return AllTime
        "2" -> do
            putStrLn "How many minutes back?"
            putStr "> "
            minStr <- getLine

            case readMaybe minStr of
                Just mins -> return (LastMinutes mins)
                Nothing -> do
                    putStrLn "Invalid input. Using all data."
                    return AllTime
        "3" -> do
            putStrLn "Enter start time (YYYY-MM-DD HH:MM:SS UTC)"
            putStr "> "
            startStr <- getLine

            putStrLn "Enter end time (YYYY-MM-DD HH:MM:SS UTC)"
            putStr "> "
            endStr <- getLine

            case (parseTimeStamp startStr, parseTimeStamp endStr) of
                (Just start, Just end) -> return (DateRange start end)
                _ -> do
                    putStrLn "Invalid date format. Using all data."
                    return AllTime

        _ -> do
            putStrLn "Invalid choice. Using all data."
            return AllTime


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
        [ts,o,h,l,c,_] -> do
            timeVal  <- parseTimeStamp ts
            openVal  <- readMaybe o
            highVal  <- readMaybe h
            lowVal   <- readMaybe l
            closeVal <- readMaybe c
            return MarketData
                { timestamp = timeVal
                , openPrice = openVal
                , highPrice = highVal
                , lowPrice = lowVal
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