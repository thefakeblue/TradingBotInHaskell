module Strategies.StatefulRunner
    ( StatefulStrategy(..)
    , runStatefulBacktest
    ) where

import Backtest
import Data.List (foldl')

-- | Bundles a stateful strategy's step function with its current state.
-- Use this to run RSI, MA, EmaReclaim etc. through the backtester.
--
-- Example (CsvReplay.hs):
--   let strat = StatefulStrategy initialEmaReclaimState emaReclaimStrategy
--   let (finalBT, _) = runStatefulBacktest strat marketDataList
data StatefulStrategy s = StatefulStrategy
    { stratState :: s
    , stratStep  :: s -> MarketData -> (Decision, s)
    }

-- | Run a stateful strategy over a list of candles.
-- Returns final BacktestState and final indicator state.
runStatefulBacktest :: StatefulStrategy s -> [MarketData] -> (BacktestState, s)
runStatefulBacktest ss history =
    foldl' step (initialBacktestState, stratState ss) history
  where
    step (bt, indState) md =
        let (decision, indState') = stratStep ss indState md
            oneShot _ = decision
            bt'       = stepBacktest oneShot bt md
        in (bt', indState')
