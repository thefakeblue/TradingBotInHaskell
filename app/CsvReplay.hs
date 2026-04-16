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


-- parse CSV row: open,high,low,close,DECISION
-- We IGNORE the decision and recompute it (so we can test different strategies with same file)
parseCsvRow :: String -> Maybe MarketData
parseCsvRow str =
    case splitComma str of
        [o,h,l,c,_] -> do
            openVal  <- readMaybe o
            highVal  <- readMaybe h
            lowVal   <- readMaybe l
            closeVal <- readMaybe c
            return MarketData
                { openPrice = openVal
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