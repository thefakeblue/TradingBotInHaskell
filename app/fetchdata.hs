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
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS



main :: IO ()
main = withSocketsDo $ do
    addr <- resolve
    sock <- open addr
    putStrLn "Server listening on port 5001..." -- 5000 not working for storey


    existingTimes <- loadExistingTimestamps "HistoricalData1Min.csv"

    -- open CSV file once
    handle <- openFile "HistoricalData1Min.csv" AppendMode

    stateRef <- newIORef initialBacktestState -- added by storey

    timeSetRef <- newIORef existingTimes

    (conn, _) <- accept sock
    putStrLn "Client connected"
    handleClient conn handle stateRef timeSetRef

handleClient :: Socket -> Handle -> IORef BacktestState -> IORef (Set.Set UTCTime) -> IO ()
handleClient conn handle stateRef timeSetRef = do
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
                         { timestamp = time
                         , openPrice = o
                         , highPrice = h
                         , lowPrice = l
                         , closePrice = c
                         }
                        action = "HOLD"
                            
                    oldState <- readIORef stateRef
                    let newState = stepBacktest simpleStrategy oldState marketData
                    writeIORef stateRef newState

                    putStrLn ("Sending: " ++ action)
                    putStrLn "Backtest state: "
                    print newState -- prints updated state

                        -- WRITE TO CSV, needs to be after action
                    existingTimes <- readIORef timeSetRef

                    if Set.member time existingTimes
                        then putStrLn "Duplicate timestamp (already in CSV), skipping..."
                        else do
                            hPutStrLn handle $
                                formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time ++ "," ++
                                show o ++ "," ++
                                show h ++ "," ++
                                show l ++ "," ++
                                show c ++ "," ++
                                action

                            hFlush handle

                            writeIORef timeSetRef (Set.insert time existingTimes)

                            putStrLn "Logged new data"

                    NBS.sendAll conn (BS.pack (action ++ "\n"))
            handleClient conn handle stateRef timeSetRef -- loop to handle next message

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
parseTimeStamp t =
    parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (take 19 t)

loadExistingTimestamps :: FilePath -> IO (Set.Set UTCTime)
loadExistingTimestamps path = do
    exists <- doesFileExist path
    if not exists
        then return Set.empty
        else do
            content <- BS.readFile path  -- STRICT (no lazy issues)
            let ls = BS.lines content
                times = mapMaybe (extractTime . BS.unpack) ls
            return (Set.fromList times)

extractTime :: String -> Maybe UTCTime
extractTime line =
    case splitComma line of
        (t:_) -> parseTimeStamp t
        _     -> Nothing