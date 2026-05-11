module Backtest
    ( Decision(..)       -- already has (..)
    , decisionToString
    , MarketData(..)     -- add (..)
    , candleMomentum
    , Strategy
    , BacktestState(..)  -- add (..)
    , initialBacktestState
    , stepBacktest
    , runBacktest
     ) where

import Data.Time (UTCTime)

data Decision
    = Buy Double
    | Sell Double
    | Hold
    deriving (Show, Eq)

decisionToString :: Decision -> String
decisionToString (Buy _) = "BUY"
decisionToString (Sell _) = "SELL"
decisionToString Hold = "HOLD"

data MarketData = MarketData
    { timestamp  :: UTCTime
    , openPrice  :: Double
    , highPrice  :: Double
    , lowPrice   :: Double
    , closePrice :: Double
    } deriving (Show, Eq)

data CandleRow = CandleRow
    { candleData :: MarketData
    , expectedDecision :: Decision
    } deriving (Show, Eq)

candleMomentum :: MarketData -> Double
candleMomentum marketData =
    closePrice marketData - openPrice marketData

type Strategy = MarketData -> Decision

data BacktestState = BacktestState
    { cash :: Double, -- current cash holding
      quantityOwned :: Double, -- quantity of asset owned
      averagePrice :: Double, -- average price of owned asset
      netProfit :: Double,
      grossProfit :: Double,
      grossLoss :: Double,
      winningTrades :: Int, -- number of winning trades
      losingTrades :: Int, -- number of losing trades
      totalTrades :: Int, -- total number of trades executed
      lastDecision :: Decision -- last signal direction seen by the backtest
     } deriving (Show, Eq)

initialBacktestState :: BacktestState
initialBacktestState = BacktestState
    { cash = 10000, -- can adjust or make this an input parameter
      quantityOwned = 0,
      averagePrice = 0,
      netProfit = 0,
      grossProfit = 0,
      grossLoss = 0,
      winningTrades = 0,
      losingTrades = 0,
      totalTrades = 0,
      lastDecision = Hold
    }

stepBacktest :: Strategy -> BacktestState -> MarketData -> BacktestState
stepBacktest strategy state marketData =
    case strategy marketData of
        Buy amount ->
            if amount <= 0 || cash state < amount * closePrice marketData
                then state { lastDecision = Buy 0 }
                else
                    let oldQuantity = quantityOwned state
                        newQuantity = oldQuantity + amount
                        price = closePrice marketData
                        oldAveragePrice = averagePrice state
                        newAveragePrice = 
                            if oldQuantity == 0 
                            then price 
                            else ((oldQuantity*oldAveragePrice) + (amount*price)) / newQuantity
                        newCash = cash state - amount * price
                    in state
                        { quantityOwned = newQuantity
                        , averagePrice = newAveragePrice
                        , cash = newCash
                        , lastDecision = Buy 0
                        }
        Sell amount ->
            if amount <= 0 || quantityOwned state <= 0
            then state { lastDecision = Sell 0 }
            else
                let closeQty = quantityOwned state
                    price = closePrice marketData
                    profitPerUnit = price - averagePrice state
                    totalProfit = profitPerUnit * closeQty -- close entire long position
                    newCash = cash state + closeQty * price
                in state
                    { quantityOwned = 0
                    , cash = newCash
                    , netProfit = netProfit state + totalProfit
                    , grossProfit = grossProfit state + max totalProfit 0
                    , grossLoss = grossLoss state + abs (min totalProfit 0)
                    , winningTrades = winningTrades state + if totalProfit > 0 then 1 else 0
                    , losingTrades = losingTrades state + if totalProfit < 0 then 1 else 0
                    , totalTrades = totalTrades state + 1
                    , averagePrice = 0
                    , lastDecision = Sell 0
                    }
        Hold -> state

runBacktest :: Strategy -> [MarketData] -> BacktestState
runBacktest strategy history =
    foldl (stepBacktest strategy) initialBacktestState history