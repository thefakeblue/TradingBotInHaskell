{-# LANGUAGE OverloadedStrings #-}
import Backtest
import Strategies
import Strategies.StatefulRunner
import Strategies.Custom
import Text.Read (readMaybe)
import Data.Time
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Control.Monad (forM, forM_, when)

main :: IO ()
main = do
    let dataFiles = [ "1YearHistoricalData10Min.csv"
                    , "1YearHistoricalData5Min.csv"
                    , "1YearHistoricalData3Min.csv"
                    , "1YearHistoricalData1Min.csv"
                    ]
    datasets <- mapM loadMarketData dataFiles
    let candidates = statelessCandidates ++ statefulCandidates

    putStrLn "Starting strategy search..."
    results <- fmap concat $ forM datasets $ \(file, rows) -> do
        putStrLn $ "--- Testing on " ++ file ++ " (" ++ show (length rows) ++ " candles) ---"
        fmap concat $ forM candidates $ \cand -> do
            let st = testCandidate cand rows
            let profit = netProfit st
            when (profit >= 1000) $ do
                putStrLn $ "*** HIT 1k net profit: " ++ candidateName cand ++ " on " ++ file ++ " -> " ++ show profit
            return [Result file (candidateName cand) st]

    let best = take 20 $ reverse $ sortOn (netProfit . resultState) results
    putStrLn "\nTop 20 results across all tests:"
    mapM_ printResult best
    let winners = filter ((>= 1000) . netProfit . resultState) results
    when (null winners) $ putStrLn "No strategy hit 1000 net profit in this search."
    putStrLn "Search complete."

loadMarketData :: FilePath -> IO (FilePath, [MarketData])
loadMarketData file = do
    contents <- readFile file
    let rows = lines contents
    let marketDataList = mapMaybe parseCsvRow rows
    return (file, marketDataList)

data Candidate = Candidate
    { candidateName :: String
    , candidateTest :: [MarketData] -> BacktestState
    }

testCandidate :: Candidate -> [MarketData] -> BacktestState
testCandidate cand = candidateTest cand

printResult :: Result -> IO ()
printResult r = do
    let st = resultState r
    putStrLn $ resultFile r ++ " | " ++ resultName r ++ " | net=" ++ show (netProfit st)
             ++ " | cash=" ++ show (cash st)
             ++ " | trades=" ++ show (totalTrades st)
             ++ " | wins=" ++ show (winningTrades st)
             ++ " | losses=" ++ show (losingTrades st)
             ++ " | pf=" ++ showPF st

showPF :: BacktestState -> String
showPF st
    | grossLoss st == 0 = "999"
    | otherwise = show (grossProfit st / grossLoss st)

data Result = Result
    { resultFile :: FilePath
    , resultName :: String
    , resultState :: BacktestState
    }

statelessCandidates :: [Candidate]
statelessCandidates =
    [ Candidate "simpleStrategy" (runBacktest simpleStrategy)
    ] ++ momentumCandidates ++ meanReversionCandidates ++ rangeBreakoutCandidates ++ customStatelessCandidates

momentumCandidates :: [Candidate]
momentumCandidates =
    [ Candidate ("momentumStrategy " ++ show t) (runBacktest (momentumStrategy t))
    | t <- [0.001,0.002,0.003,0.005,0.007,0.01]
    ]

meanReversionCandidates :: [Candidate]
meanReversionCandidates =
    [ Candidate ("meanReversionStrategy " ++ show t) (runBacktest (meanReversionStrategy t))
    | t <- [0.001,0.002,0.003,0.005,0.007,0.01]
    ]

rangeBreakoutCandidates :: [Candidate]
rangeBreakoutCandidates =
    [ Candidate ("rangeBreakoutStrategy " ++ show p) (runBacktest (rangeBreakoutStrategy p))
    | p <- [0.1,0.15,0.2,0.25,0.3,0.35,0.4]
    ]

customStatelessCandidates :: [Candidate]
customStatelessCandidates =
    [ Candidate ("customTrendBreakout " ++ show p ++ " " ++ show th)
              (runBacktest (customTrendBreakoutStrategy p th))
    | p <- [0.15,0.2,0.25,0.3]
    , th <- [0.001,0.0025,0.005]
    ] ++
    [ Candidate ("customRangeReversion " ++ show b ++ " " ++ show p)
              (runBacktest (customRangeReversionStrategy b p))
    | b <- [0.45,0.5,0.55,0.6]
    , p <- [0.1,0.15,0.2,0.25]
    ] ++
    [ Candidate ("customRangeReversionConservative " ++ show b ++ " " ++ show p)
              (runBacktest (customRangeReversionConservative b p))
    | b <- [0.45,0.5,0.55,0.6]
    , p <- [0.1,0.15,0.2,0.25]
    ]

statefulCandidates :: [Candidate]
statefulCandidates =
    rsiCandidates ++ maCandidates ++ customStatefulCandidates

rsiCandidates :: [Candidate]
rsiCandidates =
    [ Candidate ("stepRSIStrategy " ++ show o ++ " " ++ show ob)
              (fst . runStatefulBacktest (StatefulStrategy (initialRSIState 30) (stepRSIStrategy o ob)))
    | (o,ob) <- [(30,70),(30,80),(20,60),(20,70),(40,70)]
    ]

maCandidates :: [Candidate]
maCandidates =
    [ Candidate ("stepMAStrategy " ++ show f ++ " " ++ show s)
              (fst . runStatefulBacktest (StatefulStrategy (initialMAState f s) stepMAStrategy))
    | (f,s) <- [(5,20),(9,21),(10,30),(20,50)]
    ]

customStatefulCandidates :: [Candidate]
customStatefulCandidates =
    [ Candidate ("stepATRTrendStrategy " ++ show p ++ " " ++ show g)
              (fst . runStatefulBacktest (StatefulStrategy initialATRState (stepATRTrendStrategy p g)))
    | p <- [10,14,20]
    , g <- [1.05,1.1,1.2]
    ] ++
    [ Candidate ("stepEMABreakoutStrategy " ++ show p)
              (fst . runStatefulBacktest (StatefulStrategy initialEMAState (stepEMABreakoutStrategy p)))
    | p <- [8,10,12,20,30]
    ]

parseCsvRow :: String -> Maybe MarketData
parseCsvRow str =
    case splitComma str of
        [t,o,h,l,c,_] -> do
            time <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (take 19 t)
            openVal <- readMaybe o
            highVal <- readMaybe h
            lowVal <- readMaybe l
            closeVal <- readMaybe c
            return MarketData
                { timestamp = time
                , openPrice = openVal
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
