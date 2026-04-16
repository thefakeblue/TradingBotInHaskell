module Main where

import System.IO
import Text.Read (readMaybe)
import Backtest
import Strategies

main :: IO ()
main = do
    contents <- readFile "trades.csv"
    let rows = lines contents

    let marketDataList = mapMaybe parseCsvRow rows

    let finalState = runBacktest simpleStrategy marketDataList

    putStrLn "Final Backtest State:"
    print finalState


-- helper
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) =
    case f x of
        Nothing -> mapMaybe f xs
        Just y  -> y : mapMaybe f xs


<<<<<<< HEAD
-- parse CSV row: timestamp,open,high,low,close,DECISION
=======
-- parse CSV row: open,high,low,close,DECISION
>>>>>>> f66a7687d9fd158cab35b99aa202e855c2485329
-- We IGNORE the decision and recompute it (so we can test different strategies with same file)
parseCsvRow :: String -> Maybe MarketData
parseCsvRow str =
    case splitComma str of
<<<<<<< HEAD
        [ts,o,h,l,c,_] -> do
            timeVal <- readMaybe ts
=======
        [o,h,l,c,_] -> do
>>>>>>> f66a7687d9fd158cab35b99aa202e855c2485329
            openVal  <- readMaybe o
            highVal  <- readMaybe h
            lowVal   <- readMaybe l
            closeVal <- readMaybe c
            return MarketData
<<<<<<< HEAD
                { timestamp = timeVal
                , openPrice = openVal
=======
                { openPrice = openVal
>>>>>>> f66a7687d9fd158cab35b99aa202e855c2485329
                , highPrice = highVal
                , lowPrice = lowVal
                , closePrice = closeVal
                }
        _ -> Nothing


splitComma :: String -> [String]
splitComma s =
    case break (== ',') s of
        (a, []) -> [a]
        (a, _:rest) -> a : splitComma rest