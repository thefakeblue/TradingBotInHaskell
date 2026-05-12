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
    stratStateRef <- newIORef initialSNAPState     -- stateful strategy state

    (conn, _) <- accept sock
    putStrLn "Client connected"
    handleClient conn handle stateRef stratStateRef

handleClient :: Socket -> Handle -> IORef BacktestState -> IORef SNAPState -> IO ()
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
                                    -- LIVE STRATEGY: SNAP+ (Scalp with No Alternation Protocol + profit flip)
                                    -- br=0.45 pr=0.25 fast=3 slow=8 mh=20 tt=0.0005 hs=0.001 tp=0.001 cd=5
                                    -- profitFlip=True: flip L→S only when closing trade is at profit
                                    -- hardStop=0.001 (~7 pts) exits before a bad position bleeds
                                    -- tp=0.001: lock in gains at ~7 pts above entry
                                    -- cd=5: 5-bar cooldown after hard-stop or trend-against exit
                                    -- Backtest: 20d net=656 wr=74%, 1yr net=6753 wr=72%
                    oldStratState <- readIORef stratStateRef
                    let (decision, newStratState) = stepSNAP 0.45 0.25 3 8 20 0.0005 0.001 0.001 5 True oldStratState marketData
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
