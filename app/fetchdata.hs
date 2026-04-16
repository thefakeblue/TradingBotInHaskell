import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent (forkIO)
import Text.Read (readMaybe)
import System.IO
import Backtest
import Strategies
import Data.IORef
import Data.Time
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve
    sock <- open addr

    putStrLn "Server listening on port 5001..."

    existingTimes <- loadExistingTimestamps "trades.csv"

    handle <- openFile "trades.csv" AppendMode

    stateRef  <- newIORef initialBacktestState
    timeSetRef <- newIORef existingTimes

    _ <- forkIO $ do
        (conn, _) <- accept sock
        putStrLn "Client connected"
        handleClient conn handle stateRef timeSetRef

    -- keep main thread alive
    forever $ return ()

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

                        decision = simpleStrategy marketData
                        action   = decisionToString decision

                    oldState <- readIORef stateRef
                    let newState = stepBacktest simpleStrategy oldState marketData
                    writeIORef stateRef newState

                    print newState

                    existingTimes <- readIORef timeSetRef

                    if Set.member time existingTimes
                        then putStrLn "Duplicate timestamp, skipping..."
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

                    NBS.sendAll conn (BS.pack (action ++ "\n"))

            handleClient conn handle stateRef timeSetRef

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
parseTimeStamp t =
    parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (take 19 t)

loadExistingTimestamps :: FilePath -> IO (Set.Set UTCTime)
loadExistingTimestamps path = do
    exists <- doesFileExist path
    if not exists
        then return Set.empty
        else do
            content <- readFile path
            let ls = lines content
                times = mapMaybe extractTime ls
            return (Set.fromList times)

extractTime :: String -> Maybe UTCTime
extractTime line =
    case splitComma line of
        (t:_) -> parseTimeStamp t
        _     -> Nothing