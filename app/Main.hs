import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever)
import Text.Read (readMaybe)
import System.IO
import Backtest
import Strategies
import Data.IORef
import Data.Time



main :: IO ()
main = withSocketsDo $ do
    addr <- resolve
    sock <- open addr
    putStrLn "Server listening on port 5001..."

    handle <- openFile "trades.csv" AppendMode

    stateRef     <- newIORef initialBacktestState
    stratStateRef <- newIORef initialRRV2State     -- stateful strategy state

    (conn, _) <- accept sock
    putStrLn "Client connected"
    handleClient conn handle stateRef stratStateRef

handleClient :: Socket -> Handle -> IORef BacktestState -> IORef RRV2State -> IO ()
handleClient conn handle stateRef stratStateRef = do
    msg <- NBS.recv conn 1024

    if BS.null msg
        then putStrLn "Client disconnected"
        else do
            let str = BS.unpack msg
            putStrLn ("Received: " ++ str)

            case parseCandle str of
                Nothing -> do
                    putStrLn "Parse error"
                    NBS.sendAll conn (BS.pack "HOLD\n")

                Just (time, o,h,l,c) -> do
                    let marketData = MarketData
                         { timestamp  = time
                         , openPrice  = o
                         , highPrice  = h
                         , lowPrice   = l
                         , closePrice = c
                         }
                                    -- ┌──────────────────────────────────────────────────────────┐
                                    -- │  LIVE STRATEGY: stepRRV2 V2 (stateful range reversion)  │
                                    -- │  bodyRatio=0.45  proximity=0.25  ema=0  maxHold=0        │
                                    -- │  stopLossPct=0.005  (flip if loss <0.5%; close otherwise)│
                                    -- │                                                          │
                                    -- │  Backtest: 1min 20117 net / 74% WR                      │
                                    -- │            10min 3885 net  / 74% WR                     │
                                    -- │                                                          │
                                    -- │  Sends BUY / SELL / CLOSE / HOLD to NinjaTrader.        │
                                    -- │  NinjaTrader handles CLOSE via ExitLong/ExitShort.       │
                                    -- └──────────────────────────────────────────────────────────┘
                    oldStratState <- readIORef stratStateRef
                    let (decision, newStratState) = stepRRV2 0.45 0.25 0 0 0.005 oldStratState marketData
                    writeIORef stratStateRef newStratState

                    let action = decisionToString decision

                    oldState <- readIORef stateRef
                    let newState = stepBacktest (\_ -> decision) oldState marketData
                    writeIORef stateRef newState

                    putStrLn ("Sending: " ++ action)
                    putStrLn "Backtest state: "
                    print newState

                    hPutStrLn handle $
                        show time ++ "," ++
                        show o ++ "," ++
                        show h ++ "," ++
                        show l ++ "," ++
                        show c ++ "," ++
                        action

                    hFlush handle

                    NBS.sendAll conn (BS.pack (action ++ "\n"))
            handleClient conn handle stateRef stratStateRef

parseCandle :: String -> Maybe (UTCTime, Double, Double, Double, Double)
parseCandle str =
    case splitComma str of
        [t,o,h,l,c] -> do
            time  <- parseTimeStamp t
            open  <- readMaybe o
            high  <- readMaybe h
            low   <- readMaybe l
            close <- readMaybe c
            return (time , open, high, low, close)

splitComma :: String -> [String]
splitComma s =
    case break (== ',') s of
        (a, ',' : rest) -> a : splitComma rest
        (a, "") -> [a]

resolve :: IO AddrInfo
resolve = do
    let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    head <$> getAddrInfo (Just hints) Nothing (Just "5001")

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr)
                   (addrSocketType addr)
                   (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 1
    return sock

parseTimeStamp :: String -> Maybe UTCTime
parseTimeStamp = parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q"))
