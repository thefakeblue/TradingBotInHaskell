import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Text.Read (readMaybe)
import System.IO
import Backtest
import Strategies
import Data.IORef
import Data.Time
import TUI



main :: IO ()
main = withSocketsDo $ do
    addr <- resolve
    sock <- open addr

    -- open CSV file once
    handle <- openFile "trades.csv" AppendMode

    stateRef  <- newIORef initialBacktestState
    tuiHandle <- newTuiHandle                  -- TUI shared state

    -- socket server on background thread; TUI owns the main thread
    _ <- forkIO $ do
        (conn, _) <- accept sock
        handleClient conn handle stateRef tuiHandle

    runTUI tuiHandle

handleClient :: Socket -> Handle -> IORef BacktestState -> TuiHandle -> IO ()
handleClient conn handle stateRef tuiHandle = do
    msg <- NBS.recv conn 1024

    if BS.null msg
        then return () -- Client disconnected
        else do
            let str = BS.unpack msg

            case parseCandle str of
                Nothing -> do
                    NBS.sendAll conn (BS.pack "HOLD\n")

                Just (time, o,h,l,c) -> do
                    let marketData = MarketData
                         { timestamp = time
                         , openPrice = o
                         , highPrice = h
                         , lowPrice = l
                         , closePrice = c
                         }
                        decision = simpleStrategy marketData
                        action   = decisionToString decision

                    oldState <- readIORef stateRef
                    let newState = stepBacktest simpleStrategy oldState marketData
                    writeIORef stateRef newState

                    updateTUI tuiHandle newState marketData decision -- update TUI

                    -- WRITE TO CSV, needs to be after action
                    hPutStrLn handle $
                        show time ++ "," ++
                        show o ++ "," ++
                        show h ++ "," ++
                        show l ++ "," ++
                        show c ++ "," ++
                        action

                    hFlush handle  -- force save immediately

                    NBS.sendAll conn (BS.pack (action ++ "\n"))
            handleClient conn handle stateRef tuiHandle -- loop to handle next message

parseCandle :: String -> Maybe (UTCTime, Double, Double, Double, Double) -- parses the Raw Candle data from C and makes it useable in haskell as doubles
parseCandle str =
    case splitComma str of
        [t,o,h,l,c] -> do -- 't' Date then time,'o' opening (first trades price), 'h' high  (top wick), 'l' Low (bottom wick), 'c' clsoe 'last trade done that candle'
            time  <- parseTimeStamp t
            open  <- readMaybe o
            high  <- readMaybe h
            low   <- readMaybe l
            close <- readMaybe c
            return (time , open, high, low, close)

splitComma :: String -> [String]-- takes the new list of items from above and makes them each their own thing
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
    head <$> getAddrInfo (Just hints) Nothing (Just "5001") -- 5000 wont work on my machine

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