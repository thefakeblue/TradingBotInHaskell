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
<<<<<<< HEAD
     ) where

=======
    ) where
>>>>>>> 579d2d98a5b7683bcac9eeef31174489250816c0
import Data.Time

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
    { cash :: Double
    , quantityOwned :: Double
    , averagePrice :: Double
    , netProfit :: Double
    , grossProfit :: Double
    , grossLoss :: Double
    , winningTrades :: Int
    , losingTrades :: Int
    , totalTrades :: Int
     } deriving (Show, Eq)

initialBacktestState :: BacktestState
initialBacktestState = BacktestState
    { cash = 10000
    , quantityOwned = 0
    , averagePrice = 0
    , netProfit = 0
    , grossProfit = 0
    , grossLoss = 0
    , winningTrades = 0
    , losingTrades = 0
    , totalTrades = 0
    }

stepBacktest :: Strategy -> BacktestState -> MarketData -> BacktestState
stepBacktest strategy state marketData =
    case strategy marketData of
        Buy amount ->
            if amount <= 0 || cash state < amount * closePrice marketData
            then state
            else
                let oldQuantity = quantityOwned state
                    newQuantity = oldQuantity + amount
                    price = closePrice marketData
                    oldAveragePrice = averagePrice state
                    newAveragePrice =
                        if oldQuantity == 0
                        then price
                        else ((oldQuantity * oldAveragePrice) + (amount * price)) / newQuantity
                    newCash = cash state - amount * price
                in state
                    { quantityOwned = newQuantity
                    , averagePrice = newAveragePrice
                    , cash = newCash
                    }

        Sell amount ->
            if amount <= 0 || quantityOwned state < amount
            then state
            else
                let price = closePrice marketData
                    profitPerUnit = price - averagePrice state
                    totalProfit = profitPerUnit * amount
                    newQuantity = quantityOwned state - amount
                    newCash = cash state + amount * price
                in state
                    { quantityOwned = newQuantity
                    , cash = newCash
                    , netProfit = netProfit state + totalProfit
                    , grossProfit = grossProfit state + max totalProfit 0
                    , grossLoss = grossLoss state + abs (min totalProfit 0)
                    , winningTrades = winningTrades state + if totalProfit > 0 then 1 else 0
                    , losingTrades = losingTrades state + if totalProfit < 0 then 1 else 0
                    , totalTrades = totalTrades state + 1
                    , averagePrice = if newQuantity == 0 then 0 else averagePrice state
                    }

        Hold -> state

runBacktest :: Strategy -> [MarketData] -> BacktestState
runBacktest strategy history =
    foldl (stepBacktest strategy) initialBacktestState history