module TUI (TuiHandle, newTuiHandle, runTUI, updateTUI) where

import Layoutz
import Backtest
import Data.IORef
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import System.IO.Unsafe (unsafePerformIO)

-- ─────────────────────────────────────────────
--  Shared state written by Main, read by TUI
-- ─────────────────────────────────────────────

data TuiState = TuiState
    { tuiBacktest     :: BacktestState
    , tuiLastCandle   :: Maybe MarketData
    , tuiLastDecision :: Maybe Decision
    , tuiLogs         :: [String]        -- newest first, capped at 20
    , tuiConnected    :: Bool
    , tuiCandleCount  :: Int
    }

initialTuiState :: TuiState
initialTuiState = TuiState
    { tuiBacktest     = initialBacktestState
    , tuiLastCandle   = Nothing
    , tuiLastDecision = Nothing
    , tuiLogs         = ["[--:--:--] Waiting for connection on port 5001..."]
    , tuiConnected    = False
    , tuiCandleCount  = 0
    }

-- Opaque handle passed back to Main
newtype TuiHandle = TuiHandle (IORef TuiState)

newTuiHandle :: IO TuiHandle
newTuiHandle = TuiHandle <$> newIORef initialTuiState

-- Called by Main on every candle
updateTUI :: TuiHandle -> BacktestState -> MarketData -> Decision -> IO ()
updateTUI (TuiHandle ref) bs md dec = do
    let logLine = "[" ++ fmtTime (timestamp md) ++ "] "
               ++ decisionToString dec
               ++ "  close=" ++ fmt2 (closePrice md)
               ++ "  net="   ++ fmtSigned (netProfit bs)
    modifyIORef ref $ \s -> s
        { tuiBacktest     = bs
        , tuiLastCandle   = Just md
        , tuiLastDecision = Just dec
        , tuiCandleCount  = tuiCandleCount s + 1
        , tuiConnected    = True
        , tuiLogs         = take 20 (logLine : tuiLogs s)
        }

-- ─────────────────────────────────────────────
--  Layoutz app
-- ─────────────────────────────────────────────

data AppMsg = Tick

data AppModel = AppModel { appHandle :: TuiHandle }

runTUI :: TuiHandle -> IO ()
runTUI h = runApp $ LayoutzApp
    { appInit          = (AppModel h, CmdNone)
    , appUpdate        = \Tick m -> (m, CmdNone)
    , appSubscriptions = \_ -> subEveryMs 500 Tick
    , appView          = \m ->
        let (TuiHandle ref) = appHandle m
            s = unsafePerformIO (readIORef ref)
        in renderDashboard s
    }

-- ─────────────────────────────────────────────
--  Formatters
-- ─────────────────────────────────────────────

fmt2 :: Double -> String
fmt2 x =
    let cents = round (abs x * 100) :: Int
        w     = cents `div` 100
        f     = cents `mod` 100
    in (if x < 0 then "-" else "")
       ++ show w ++ "." ++ (if f < 10 then "0" else "") ++ show f

fmtUSD :: Double -> String
fmtUSD x = "$" ++ fmt2 x

fmtSigned :: Double -> String
fmtSigned x
    | x >= 0    = "+$" ++ fmt2 x
    | otherwise = "-$" ++ fmt2 (abs x)

fmtTime :: UTCTime -> String
fmtTime = formatTime defaultTimeLocale "%H:%M:%S"

padL :: Int -> String -> String
padL n s = let t = take n s in replicate (n - length t) ' ' ++ t

winRate :: BacktestState -> String
winRate bs
    | totalTrades bs == 0 = "N/A"
    | otherwise =
        let r = fromIntegral (winningTrades bs)
              / fromIntegral (totalTrades bs) * 100 :: Double
        in fmt2 r ++ "%"

-- ─────────────────────────────────────────────
--  Colour helpers  (green-on-dark)
-- ─────────────────────────────────────────────

green, brightGreen, yellow, red :: L -> L
green       = withColor ColorGreen
brightGreen = withColor ColorBrightGreen
yellow      = withColor ColorYellow
red         = withColor ColorRed

bold, dim :: L -> L
bold = withStyle StyleBold
dim  = withStyle StyleDim

pnlColor :: Double -> L -> L
pnlColor x = if x >= 0 then brightGreen else red

decColor :: Decision -> L -> L
decColor (Buy  _) = brightGreen
decColor (Sell _) = red
decColor Hold     = yellow

-- ─────────────────────────────────────────────
--  Panels
-- ─────────────────────────────────────────────

portfolioPanel :: BacktestState -> L
portfolioPanel bs =
    withBorder BorderDouble $ green $ section "Portfolio / P&L"
        [ row [ statusCard "Cash"        (padL 11 $ fmtUSD (cash bs))
              , statusCard "Holdings"    (padL 11 $ fmtUSD (quantityOwned bs * averagePrice bs))
              , statusCard "Total Value" (padL 11 $ fmtUSD (cash bs + quantityOwned bs * averagePrice bs))
              ]
        , br
        , row [ pnlColor (netProfit bs) $ statusCard "Net P&L"     (padL 11 $ fmtSigned (netProfit bs))
              , brightGreen             $ statusCard "Gross Profit" (padL 11 $ fmtUSD (grossProfit bs))
              , red                     $ statusCard "Gross Loss"   (padL 11 $ fmtUSD (grossLoss bs))
              ]
        ]

statusPanel :: TuiState -> L
statusPanel s =
    let connLabel = if tuiConnected s
                        then brightGreen $ bold $ text "● CONNECTED"
                        else red         $ bold $ text "○ NO CLIENT"
        lastDec   = case tuiLastDecision s of
                        Nothing -> dim $ text "---"
                        Just d  -> decColor d $ bold $ text (decisionToString d)
        bs = tuiBacktest s
    in withBorder BorderDouble $ green $ section "Bot Status"
        [ row [ connLabel ]
        , br
        , row [ statusCard "Candles"  (padL 6 $ show (tuiCandleCount s))
              , statusCard "Trades"   (padL 6 $ show (totalTrades bs))
              , statusCard "Win Rate" (padL 8 $ winRate bs)
              ]
        , br
        , row [ statusCard "Last Signal" "  ", lastDec ]
        , br
        , row [ statusCard "Qty Held"  (padL 8 $ fmt2 (quantityOwned bs))
              , statusCard "Avg Entry" (padL 10 $ if quantityOwned bs == 0
                                                  then "---"
                                                  else fmtUSD (averagePrice bs))
              ]
        , br
        , dim $ text "[ESC] quit"
        ]

candlePanel :: TuiState -> L
candlePanel s =
    let body = case tuiLastCandle s of
                 Nothing -> [dim $ text "Waiting for first candle..."]
                 Just md ->
                     let mom = candleMomentum md
                     in [ row [ statusCard "Open"  (padL 10 $ fmtUSD (openPrice  md))
                              , statusCard "High"  (padL 10 $ fmtUSD (highPrice  md))
                              , statusCard "Low"   (padL 10 $ fmtUSD (lowPrice   md))
                              , statusCard "Close" (padL 10 $ fmtUSD (closePrice md))
                              ]
                        , br
                        , row [ pnlColor mom $ statusCard "Momentum" (padL 9 $ fmtSigned mom)
                              , statusCard "Range" (padL 9 $ fmtUSD (highPrice md - lowPrice md))
                              , statusCard "Time"  (padL 10 $ fmtTime (timestamp md))
                              ]
                        ]
    in withBorder BorderDouble $ green $ section "Live Candle" body

logPanel :: TuiState -> L
logPanel s =
    withBorder BorderNone $ section "Event Log"
        (map (\l -> dim $ green $ text ("  " ++ l)) (take 6 (tuiLogs s)))

renderDashboard :: TuiState -> L
renderDashboard s = layout
    [ center $ bold $ brightGreen $ text "▓▒░  HASKELL TRADER  ░▒▓"
    , br
    , row [ portfolioPanel (tuiBacktest s)
          , text "  "
          , statusPanel s
          , text "  "
          , candlePanel s
          ]
    , br
    , logPanel s
    ]