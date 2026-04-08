module Backtest
    ( Decision(..)
    , decisionToString
    , MarketData(..)
    , candleMomentum
    , Strategy
    , BacktestState(..)
    , initialBacktestState
    , stepBacktest
    , runBacktest
     ) where


data Decision
    = Buy Double -- if strategy recommends quantity to buy/sell
    | Sell Double
    | Hold
    deriving (Show, Eq)

decisionToString :: Decision -> String
decisionToString (Buy _) = "BUY"
decisionToString (Sell _) = "SELL"
decisionToString Hold = "HOLD"

-- must contain info needed to make strategy decision:
data MarketData = MarketData
    {openPrice :: Double,
     highPrice :: Double,
     lowPrice :: Double,
     closePrice :: Double
     -- currentPrice :: Double, -- can be used for momentum or other indicators, but not needed if strategy only looks at open/high/low/close
     -- shortMovingAverage :: Double, -- for short term vs long term momentum
     --longMovingAverage :: Double, -- for short term vs long term momentum
     -- can add other indicators like rsi, macd, etc
     --momentum :: Double,
     --holding :: Bool,
     --entryPrice :: Double
     } deriving (Show, Eq)

data CandleRow = CandleRow -- each row in cvs is one candle with given market data and expected decision for backtesting
    {candleData :: MarketData
    , expectedDecision :: Decision
    } deriving (Show, Eq) -- each row is [CandleRow] 

candleMomentum :: MarketData -> Double
candleMomentum marketData = closePrice marketData - openPrice marketData -- deriving momentum rather than storing it as a field

type Strategy = MarketData -> Decision

-- if data from C doesnt retrieve momentum, movingaverage etc, need functions

data BacktestState = BacktestState
    { cash :: Double, -- current cash holding
      --assetOwned :: Bool, -- include if we are dealing with one unit at a time, otherwise use quantity
      quantityOwned :: Double, -- quantity of asset owned
      averagePrice :: Double, -- average price of owned asset. don't include if only using one unit at a time
      netProfit :: Double,
      grossProfit :: Double,
      grossLoss :: Double,
      -- entryPrice :: Double, -- price at which we entered the position. Only need when dealing in one unit at a time
      winningTrades :: Int, -- number of winning trades
      losingTrades :: Int, -- number of losing trades
      totalTrades :: Int -- total number of trades executed
     } deriving (Show, Eq)

-- initial state for backtesting
initialBacktestState :: BacktestState
initialBacktestState = BacktestState
    { cash = 10000, -- can adjust or make this an input parameter
      quantityOwned = 0,
      --assetOwned = False,
      averagePrice = 0,
      netProfit = 0,
      grossProfit = 0,
      grossLoss = 0,
      --entryPrice = 0,
      winningTrades = 0,
      losingTrades = 0,
      totalTrades = 0
    }

-- one step of backtesting: apply strategy to current market data and update state accordingly
stepBacktest :: Strategy -> BacktestState -> MarketData -> BacktestState
stepBacktest strategy state marketData = -- strategy, backtest state, and current market data
    case strategy marketData of -- adjust the state based on strategy decision
        Buy amount ->
            if amount <=0 || cash state < amount*closePrice marketData
                then state -- invalid buy amount
                else
                    let oldQuantity = quantityOwned state
                        newQuantity = oldQuantity + amount
                        price = closePrice marketData
                        oldAveragePrice = averagePrice state
                        newAveragePrice = 
                            if oldQuantity == 0 
                            then price 
                            else ((oldQuantity*oldAveragePrice) + (amount*price)) / newQuantity
                        newCash = cash state - amount*price
                    in state
                        {quantityOwned = newQuantity,
                         averagePrice = newAveragePrice,
                         cash = newCash
                         } -- adjusted quantity, average price, and cash
        Sell amount ->
            if amount <=0 || quantityOwned state < amount
            then state -- invalid sell amount
            else
                let price = closePrice marketData
                    profitPerUnit = price - averagePrice state
                    totalProfit = profitPerUnit * amount -- profit from average prices
                    newQuantity = quantityOwned state - amount
                    newCash = cash state + amount * price
                in state
                    {quantityOwned = newQuantity,
                     cash = newCash,
                     netProfit = netProfit state + totalProfit,
                     grossProfit = grossProfit state + max totalProfit 0,
                     grossLoss = grossLoss state + abs(min totalProfit 0),
                     winningTrades = winningTrades state + if totalProfit > 0 then 1 else 0, -- counting each sell action with profit rather than individual units
                     losingTrades = losingTrades state + if totalProfit < 0 then 1 else 0, --same for losing trades
                     totalTrades = totalTrades state + 1,
                     averagePrice = if newQuantity == 0 then 0 else averagePrice state -- reset average price if quantity 0
                     } -- adjusted quantity, cash, profit/loss, and trade counts
        Hold -> state

runBacktest :: Strategy -> [MarketData] -> BacktestState
runBacktest strategy history =
    foldl (stepBacktest strategy) initialBacktestState history



-- pass csv through backtesting
-- output print statements
-- gui setup