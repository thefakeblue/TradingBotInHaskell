
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

import Data.Time (UTCTime)

data Decision
    = Buy Double
    | Sell Double
    | Close        -- exit current position (go flat), no new entry
    | Hold
    deriving (Show, Eq)

decisionToString :: Decision -> String
decisionToString (Buy _)  = "BUY"
decisionToString (Sell _) = "SELL"
decisionToString Close    = "CLOSE"
decisionToString Hold     = "HOLD"

data MarketData = MarketData
    { timestamp  :: UTCTime
    , openPrice  :: Double
    , highPrice  :: Double
    , lowPrice   :: Double
    , closePrice :: Double
    } deriving (Show, Eq)

data CandleRow = CandleRow
    { candleData       :: MarketData
    , expectedDecision :: Decision
    } deriving (Show, Eq)

candleMomentum :: MarketData -> Double
candleMomentum md = closePrice md - openPrice md

type Strategy = MarketData -> Decision

data BacktestState = BacktestState
    { cash           :: Double
    , quantityOwned  :: Double   -- long units
    , shortQuantity  :: Double   -- short units
    , averagePrice   :: Double   -- avg entry price for longs
    , shortAvgPrice  :: Double   -- avg entry price for shorts
    , netProfit      :: Double
    , grossProfit    :: Double
    , grossLoss      :: Double
    , winningTrades  :: Int
    , losingTrades   :: Int
    , totalTrades    :: Int
    , lastDecision   :: Decision
    } deriving (Show, Eq)

initialBacktestState :: BacktestState
initialBacktestState = BacktestState
    { cash          = 10000
    , quantityOwned = 0
    , shortQuantity = 0
    , averagePrice  = 0
    , shortAvgPrice = 0
    , netProfit     = 0
    , grossProfit   = 0
    , grossLoss     = 0
    , winningTrades = 0
    , losingTrades  = 0
    , totalTrades   = 0
    , lastDecision  = Hold
    }

-- Close short helper: closes shortQty units at price, returns updated state
closeShortAt :: Double -> BacktestState -> BacktestState
closeShortAt price st
    | shortQuantity st <= 0 = st
    | otherwise =
        let sQty  = shortQuantity st
            sAvg  = shortAvgPrice st
            sp    = (sAvg - price) * sQty
            -- cash already includes entry proceeds (added when shorting); just pay close price
            newCash = cash st - price * sQty
        in st
            { shortQuantity = 0
            , shortAvgPrice = 0
            , cash          = newCash
            , netProfit     = netProfit st + sp
            , grossProfit   = grossProfit st + max sp 0
            , grossLoss     = grossLoss st + abs (min sp 0)
            , winningTrades = winningTrades st + if sp > 0 then 1 else 0
            , losingTrades  = losingTrades  st + if sp < 0 then 1 else 0
            , totalTrades   = totalTrades   st + 1
            }

-- Close long helper: closes qty units at price, returns updated state
closeLongAt :: Double -> BacktestState -> BacktestState
closeLongAt price st
    | quantityOwned st <= 0 = st
    | otherwise =
        let lQty = quantityOwned st
            lAvg = averagePrice st
            lp   = (price - lAvg) * lQty
            newCash = cash st + price * lQty
        in st
            { quantityOwned = 0
            , averagePrice  = 0
            , cash          = newCash
            , netProfit     = netProfit st + lp
            , grossProfit   = grossProfit st + max lp 0
            , grossLoss     = grossLoss st + abs (min lp 0)
            , winningTrades = winningTrades st + if lp > 0 then 1 else 0
            , losingTrades  = losingTrades  st + if lp < 0 then 1 else 0
            , totalTrades   = totalTrades   st + 1
            }

stepBacktest :: Strategy -> BacktestState -> MarketData -> BacktestState
stepBacktest strategy state marketData =
    let price = closePrice marketData
    in case strategy marketData of

        Buy amount ->
            if amount <= 0 then state { lastDecision = Buy 0 }
            else
                -- close any short first, then enter long (one position at a time)
                let st1 = closeShortAt price state
                in if quantityOwned st1 > 0
                   -- already long: ignore the signal, stay in current position
                   then st1 { lastDecision = Buy 0 }
                   else if cash st1 < amount * price
                   then st1 { lastDecision = Buy 0 }
                   else st1
                       { quantityOwned = amount
                       , averagePrice  = price
                       , cash          = cash st1 - amount * price
                       , lastDecision  = Buy 0
                       }

        Sell amount ->
            if amount <= 0 then state { lastDecision = Sell 0 }
            else
                -- close any long first, then enter short (one position at a time)
                let st1 = closeLongAt price state
                in if shortQuantity st1 > 0
                   -- already short: ignore the signal, stay in current position
                   then st1 { lastDecision = Sell 0 }
                   else st1
                       { shortQuantity = amount
                       , shortAvgPrice = price
                       , cash          = cash st1 + amount * price
                       , lastDecision  = Sell 0
                       }

        Close ->
            -- just exit whatever position is open, go flat
            let st1 = closeLongAt price state
                st2 = closeShortAt price st1
            in st2 { lastDecision = Close }

        Hold -> state

runBacktest :: Strategy -> [MarketData] -> BacktestState
runBacktest strategy history =
    foldl (stepBacktest strategy) initialBacktestState history
