module TUI (TuiHandle, newTuiHandle, runTUI, updateTUI) where

import Layoutz
import Backtest
    ( BacktestState(..)
    , MarketData(..)
    , Decision(..)
    , decisionToString
    , candleMomentum
    , initialBacktestState
    )
import Data.IORef
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import System.IO.Unsafe (unsafePerformIO)

-- ─────────────────────────────────────────────
--  Shared state written by Main, read by TUI
-- ─────────────────────────────────────────────

data TuiState = TuiState
    { tuiBacktest      :: BacktestState
    , tuiLastCandle    :: Maybe MarketData
    , tuiLastDecision  :: Maybe Decision
    , tuiLogs          :: [(Maybe Decision, String)]  -- newest first, capped at 100
    , tuiConnected     :: Bool
    , tuiCandleCount   :: Int
    , tuiEquityHistory :: [Double]                    -- newest first, capped at 60
    , tuiPeakEquity    :: Double
    , tuiMaxDrawdown   :: Double
    }

initialTuiState :: TuiState
initialTuiState = TuiState
    { tuiBacktest      = initialBacktestState
    , tuiLastCandle    = Nothing
    , tuiLastDecision  = Nothing
    , tuiLogs          = [(Nothing, "[--:--:--] Waiting for connection on port 5001...")]
    , tuiConnected     = False
    , tuiCandleCount   = 0
    , tuiEquityHistory = []
    , tuiPeakEquity    = 0
    , tuiMaxDrawdown   = 0
    }

newtype TuiHandle = TuiHandle (IORef TuiState)

newTuiHandle :: IO TuiHandle
newTuiHandle = TuiHandle <$> newIORef initialTuiState

updateTUI :: TuiHandle -> BacktestState -> MarketData -> Decision -> IO ()
updateTUI (TuiHandle ref) bs md dec = do
    let logLine = "[" ++ fmtTime (timestamp md) ++ "] "
               ++ decisionToString dec
               ++ "  close=" ++ fmt2 (closePrice md)
               ++ "  net="   ++ fmtSigned (netProfit bs)
    let currentEquity = cash bs + quantityOwned bs * closePrice md
    modifyIORef ref $ \s ->
        let peak  = max (tuiPeakEquity s) currentEquity
            dd    = if peak > 0 then (peak - currentEquity) / peak * 100 else 0
            maxDd = max (tuiMaxDrawdown s) dd
        in s
            { tuiBacktest      = bs
            , tuiLastCandle    = Just md
            , tuiLastDecision  = Just dec
            , tuiCandleCount   = tuiCandleCount s + 1
            , tuiConnected     = True
            , tuiLogs          = take 100 ((Just dec, logLine) : tuiLogs s)
            , tuiEquityHistory = take 60 (currentEquity : tuiEquityHistory s)
            , tuiPeakEquity    = peak
            , tuiMaxDrawdown   = maxDd
            }

-- ─────────────────────────────────────────────
--  Layoutz app
-- ─────────────────────────────────────────────

data ViewMode = ViewDashboard | ViewLogHistory | ViewStats deriving (Eq)

nextView :: ViewMode -> ViewMode
nextView ViewDashboard  = ViewLogHistory
nextView ViewLogHistory = ViewStats
nextView ViewStats      = ViewDashboard

data AppMsg = Tick | KeyPress Key

data AppModel = AppModel
    { appHandle      :: TuiHandle
    , appCurrentView :: ViewMode
    , appLogScroll   :: Int
    }

runTUI :: TuiHandle -> IO ()
runTUI h = runApp $ LayoutzApp
    { appInit          = (AppModel h ViewDashboard 0, CmdNone)
    , appUpdate        = handleMsg
    , appSubscriptions = \_ -> subBatch
        [ subEveryMs 500 Tick
        , subKeyPress (Just . KeyPress)
        ]
    , appView          = \m ->
        let (TuiHandle ref) = appHandle m
            s = unsafePerformIO (readIORef ref)
        in renderView m s
    }

handleMsg :: AppMsg -> AppModel -> (AppModel, Cmd AppMsg)
handleMsg Tick         m = (m, CmdNone)
handleMsg (KeyPress k) m = handleKey k m

handleKey :: Key -> AppModel -> (AppModel, Cmd AppMsg)
handleKey KeyEscape       m = (m, CmdExit)
handleKey (KeyChar 'q')   m = (m, CmdExit)
handleKey (KeyChar 'Q')   m = (m, CmdExit)
handleKey KeyTab          m = (m { appCurrentView = nextView (appCurrentView m)
                                 , appLogScroll   = 0 }, CmdNone)
handleKey (KeyChar '1')   m = (m { appCurrentView = ViewDashboard,  appLogScroll = 0 }, CmdNone)
handleKey (KeyChar '2')   m = (m { appCurrentView = ViewLogHistory, appLogScroll = 0 }, CmdNone)
handleKey (KeyChar '3')   m = (m { appCurrentView = ViewStats,      appLogScroll = 0 }, CmdNone)
handleKey KeyUp           m = (m { appLogScroll = max 0 (appLogScroll m - 1) }, CmdNone)
handleKey KeyDown         m = (m { appLogScroll = appLogScroll m + 1 }, CmdNone)
handleKey _               m = (m, CmdNone)

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
--  Colour helpers
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
--  Navbar
-- ─────────────────────────────────────────────

navbar :: ViewMode -> L
navbar v =
    let tab lbl vt = (if v == vt then bold . brightGreen else dim . green) $ text lbl
    in row
        [ tab "[1] Dashboard"  ViewDashboard
        , text "  "
        , tab "[2] Log"        ViewLogHistory
        , text "  "
        , tab "[3] Stats"      ViewStats
        , text "    "
        , dim $ text "[TAB] cycle  [↑↓] scroll log  [Q] quit"
        ]

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

sparklinePanel :: TuiState -> L
sparklinePanel s =
    let hist = reverse (tuiEquityHistory s)
        body  = if null hist
                then [dim $ text "Waiting for equity data..."]
                else [ brightGreen $ plotSparkline hist
                     , br
                     , dim $ green $ text $
                         "  " ++ show (length hist) ++ " candles  |  "
                         ++ "peak: " ++ fmtUSD (tuiPeakEquity s)
                         ++ "  max drawdown: " ++ fmt2 (tuiMaxDrawdown s) ++ "%"
                     ]
    in withBorder BorderDouble $ green $ section "Equity Curve (last 60 candles)" body

logEntry :: (Maybe Decision, String) -> L
logEntry (Nothing,       s) = dim $ green   $ text ("  " ++ s)
logEntry (Just (Buy  _), s) = brightGreen   $ text ("  " ++ s)
logEntry (Just (Sell _), s) = red           $ text ("  " ++ s)
logEntry (Just Hold,     s) = yellow        $ text ("  " ++ s)

logPanel :: TuiState -> L
logPanel s =
    withBorder BorderNone $ section "Event Log"
        (map logEntry (take 6 (tuiLogs s)))

-- ─────────────────────────────────────────────
--  Views
-- ─────────────────────────────────────────────

dashboardView :: TuiState -> L
dashboardView s = layout
    [ row [ portfolioPanel (tuiBacktest s)
          , text "  "
          , statusPanel s
          , text "  "
          , candlePanel s
          ]
    , br
    , sparklinePanel s
    , br
    , logPanel s
    ]

{-
- TODO - Add the start balance value to from line 59 backtest code
-}

logHistoryView :: AppModel -> TuiState -> L
logHistoryView m s =
    let allLogs = tuiLogs s
        total   = length allLogs
        scroll  = min (appLogScroll m) (max 0 (total - 1))
        visible = take 20 (drop scroll allLogs)
        footer  = dim $ green $ text $
            "  Showing " ++ show scroll ++ "-" ++ show (min (scroll + 20) total)
            ++ " of " ++ show total ++ " entries"
    in layout
        [ withBorder BorderDouble $ green $ section "Full Event Log"
            (map logEntry visible ++ [br, footer])
        ]

statsView :: TuiState -> L
statsView s =
    let bs      = tuiBacktest s
        avgWin  = if winningTrades bs == 0 then 0
                  else grossProfit bs / fromIntegral (winningTrades bs)
        avgLoss = if losingTrades bs == 0 then 0
                  else grossLoss bs / fromIntegral (losingTrades bs)
        pf      = if grossLoss bs == 0 then 0
                  else grossProfit bs / grossLoss bs
        unrealized = case tuiLastCandle s of
            Nothing -> 0
            Just md -> quantityOwned bs * (closePrice md - averagePrice bs)
    in layout
        [ withBorder BorderDouble $ green $ section "Extended Statistics"
            [ row [ statusCard "Total Trades" (padL 8 $ show (totalTrades bs))
                  , statusCard "Winners"      (padL 8 $ show (winningTrades bs))
                  , statusCard "Losers"       (padL 8 $ show (losingTrades bs))
                  , statusCard "Win Rate"     (padL 8 $ winRate bs)
                  ]
            , br
            , row [ pnlColor (netProfit bs)  $ statusCard "Net P&L"      (padL 11 $ fmtSigned (netProfit bs))
                  , brightGreen              $ statusCard "Gross Profit"  (padL 11 $ fmtUSD (grossProfit bs))
                  , red                      $ statusCard "Gross Loss"    (padL 11 $ fmtUSD (grossLoss bs))
                  ]
            , br
            , row [ brightGreen $ statusCard "Avg Win"        (padL 11 $ fmtUSD avgWin)
                  , red         $ statusCard "Avg Loss"       (padL 11 $ fmtUSD avgLoss)
                  , statusCard              "Profit Factor"   (padL 8  $ fmt2 pf ++ "x")
                  ]
            , br
            , row [ red                      $ statusCard "Max Drawdown"   (padL 8  $ fmt2 (tuiMaxDrawdown s) ++ "%")
                  , statusCard                             "Peak Equity"   (padL 11 $ fmtUSD (tuiPeakEquity s))
                  , pnlColor unrealized      $ statusCard "Unrealized P&L" (padL 11 $ fmtSigned unrealized)
                  ]
            , br
            , row [ statusCard "Candles Seen" (padL 8  $ show (tuiCandleCount s))
                  , statusCard "Cash"         (padL 11 $ fmtUSD (cash bs))
                  , statusCard "Qty Held"     (padL 8  $ fmt2 (quantityOwned bs))
                  ]
            ]
        ]

-- ─────────────────────────────────────────────
--  Keybindings bar
-- ─────────────────────────────────────────────

key :: String -> String -> L
key k desc = row [ bold $ brightGreen $ text k, dim $ green $ text (" " ++ desc) ]

keybindingsBar :: ViewMode -> L
keybindingsBar v = withBorder BorderNone $ section "Keybindings"
    [ row [ key "[1]" "Dashboard"
          , text "   "
          , key "[2]" "Log view"
          , text "   "
          , key "[3]" "Stats view"
          , text "   "
          , key "[TAB]" "Cycle views"
          , text "   "
          , scrollHint
          , text "   "
          , key "[Q] / [ESC]" "Quit"
          ]
    ]
  where
    scrollHint = case v of
        ViewLogHistory -> key "[↑] [↓]" "Scroll log"
        _              -> dim $ green $ text "[↑] [↓]  Scroll log (in Log view)"

-- ─────────────────────────────────────────────
--  Root renderer
-- ─────────────────────────────────────────────

renderView :: AppModel -> TuiState -> L
renderView m s = layout
    [ center $ bold $ brightGreen $ text "▓▒░  HASKELL TRADER  ░▒▓"
    , br
    , navbar (appCurrentView m)
    , br
    , case appCurrentView m of
        ViewDashboard  -> dashboardView s
        ViewLogHistory -> logHistoryView m s
        ViewStats      -> statsView s
    , br
    , keybindingsBar (appCurrentView m)
    ]
