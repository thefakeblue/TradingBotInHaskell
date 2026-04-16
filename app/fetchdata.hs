import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever)
import Text.Read (readMaybe)
import System.IO
import Data.Time
import qualified Data.Set as Set
import Data.IORef
import System.IO.Error (tryIOError)
import System.Directory (doesFileExist)
import Data.Maybe (mapMaybe)

-- MAIN
main :: IO ()
main = withSocketsDo $ do
    addr <- resolve
    sock <- open addr
    putStrLn "Data logger listening on port 5002..."

    -- Open CSV
    handle <- openFile "HistoricalData.csv" AppendMode

    -- Load existing timestamps
    existingTimes <- loadExistingTimestamps "HistoricalData.csv"
    timeSetRef <- newIORef existingTimes

    (conn, _) <- accept sock
    putStrLn "Client connected"
    handleClient conn handle timeSetRef

-- HANDLE CLIENT
handleClient :: Socket -> Handle -> IORef (Set.Set UTCTime) -> IO ()
handleClient conn handle timeSetRef = do
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

                Just (time, o, h, l, c) -> do
                    existingTimes <- readIORef timeSetRef

                    if Set.member time existingTimes
                        then putStrLn "Duplicate timestamp, skipping..."
                        else do
                            -- Write to CSV
                            hPutStrLn handle $
                                show time ++ "," ++
                                show o ++ "," ++
                                show h ++ "," ++
                                show l ++ "," ++
                                show c

                            hFlush handle

                            -- Update set
                            writeIORef timeSetRef (Set.insert time existingTimes)

                            putStrLn "Logged new data"

                    -- ALWAYS HOLD
                    NBS.sendAll conn (BS.pack "HOLD\n")

            handleClient conn handle timeSetRef

-- LOAD EXISTING CSV TIMES
loadExistingTimestamps :: FilePath -> IO (Set.Set UTCTime)
loadExistingTimestamps path = do
    exists <- doesFileExistSafe path
    if not exists
        then return Set.empty
        else do
            content <- readFile path
            let times = mapMaybe extractTime (lines content)
            return (Set.fromList times)

-- SAFE FILE CHECK
doesFileExistSafe :: FilePath -> IO Bool
doesFileExistSafe path = do
    result <- tryIOError (withFile path ReadMode (\_ -> return ()))
    case result of
        Left _  -> return False
        Right _ -> return True

-- EXTRACT TIME FROM CSV LINE
extractTime :: String -> Maybe UTCTime
extractTime line =
    case splitComma line of
        (t:_) -> parseTimeStamp t
        _     -> Nothing

-- PARSE CANDLE (same as before)
parseCandle :: String -> Maybe (UTCTime, Double, Double, Double, Double)
parseCandle str =
    case splitComma str of
        [t,o,h,l,c] -> do
            time  <- parseTimeStamp t
            open  <- readMaybe o
            high  <- readMaybe h
            low   <- readMaybe l
            close <- readMaybe c
            return (time, open, high, low, close)
        _ -> Nothing

-- SPLIT CSV
splitComma :: String -> [String]
splitComma s =
    case break (== ',') s of
        (a, ',' : rest) -> a : splitComma rest
        (a, "") -> [a]

-- SOCKET SETUP
resolve :: IO AddrInfo
resolve = do
    let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    head <$> getAddrInfo (Just hints) Nothing (Just "5002")

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr)
                   (addrSocketType addr)
                   (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 1
    return sock

-- TIME PARSER
parseTimeStamp :: String -> Maybe UTCTime
parseTimeStamp =
    parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q"))