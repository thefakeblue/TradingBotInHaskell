import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever)
import Text.Read (readMaybe)
import System.IO

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve
    sock <- open addr
    putStrLn "Server listening on port 5000..."

    -- open CSV file once
    handle <- openFile "trades.csv" AppendMode

    (conn, _) <- accept sock
    putStrLn "Client connected"

    forever $ do
        msg <- NBS.recv conn 1024

        if BS.null msg
            then putStrLn "Empty message"
            else do
                let str = BS.unpack msg
                putStrLn ("Received: " ++ str)

                case parseCandle str of
                    Nothing -> do
                        putStrLn "Parse error"
                        NBS.sendAll conn (BS.pack "HOLD\n")

                    Just (o,h,l,c) -> do 
                        let action = -- the real action and what it sends back to C#
                                if c > o then "BUY"
                                else if c < o then "SELL"
                                else "HOLD"

                        putStrLn ("Sending: " ++ action)

                        -- WRITE TO CSV, needs to be after action
                        hPutStrLn handle $
                            show o ++ "," ++
                            show h ++ "," ++
                            show l ++ "," ++
                            show c ++ "," ++
                            action

                        hFlush handle  -- force save immediately

                        NBS.sendAll conn (BS.pack (action ++ "\n"))

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
    head <$> getAddrInfo (Just hints) Nothing (Just "5000")

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr)
                   (addrSocketType addr)
                   (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 1
    return sock