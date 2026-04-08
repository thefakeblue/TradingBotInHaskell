import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever)
import Text.Read (readMaybe)
import System.IO
import Backtest
import Strategies
import Data.IORef


main :: IO ()
main = withSocketsDo $ do
    addr <- resolve
    sock <- open addr
    putStrLn "Server listening on port 5001..." -- 5000 not working for storey

    -- open CSV file once
    handle <- openFile "trades.csv" AppendMode

    stateRef <- newIORef initialBacktestState -- added by storey

    (conn, _) <- accept sock
    putStrLn "Client connected"
    handleClient conn handle stateRef

handleClient :: Socket -> Handle -> IORef BacktestState -> IO ()
handleClient conn handle stateRef = do
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

                Just (o,h,l,c) -> do 
                    let marketData = MarketData
                         { openPrice = o
                         , highPrice = h
                         , lowPrice = l
                         , closePrice = c
                         }
                        action = decisionToString (simpleStrategy marketData) -- given strategy (simpleStrategy is for testing)
                            
                    oldState <- readIORef stateRef
                    let newState = stepBacktest simpleStrategy oldState marketData
                    writeIORef stateRef newState

                    putStrLn ("Sending: " ++ action)
                    putStrLn "Backtest state: "
                    print newState -- prints updated state

                        -- WRITE TO CSV, needs to be after action
                    hPutStrLn handle $
                        show o ++ "," ++
                        show h ++ "," ++
                        show l ++ "," ++
                        show c ++ "," ++
                        action

                    hFlush handle  -- force save immediately

                    NBS.sendAll conn (BS.pack (action ++ "\n"))
            handleClient conn handle stateRef -- loop to handle next message

parseCandle :: String -> Maybe (Double, Double, Double, Double) -- parses the Raw Candle data from C and makes it useable in haskell as doubles
parseCandle str =
    case splitComma str of
        [o,h,l,c] -> do -- 'o' opening (first trades price), 'h' high  (top wick), 'l' Low (bottom wick), 'c' clsoe 'last trade done that candle'
            open  <- readMaybe o
            high  <- readMaybe h
            low   <- readMaybe l
            close <- readMaybe c
            return (open, high, low, close)
        _ -> Nothing

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