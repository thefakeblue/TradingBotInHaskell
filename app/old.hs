import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever)
import Text.Read (readMaybe)

main :: IO ()
main = withSocketsDo $ do -- This initializes Windows networking.
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
            then return ()
            else do
                let str = BS.unpack msg
                putStrLn ("Received: " ++ str)

                case parseCandle str of -- if it got the raw candle data what it should do now
                    Nothing -> do
                        putStrLn "Parse error"
                        NBS.sendAll conn (BS.pack "HOLD\n")

                    Just (open,high,low,close) -> do -- debugging we should have it print to CSV later
                        putStrLn ("O:" ++ show open ++
                                " H:" ++ show high ++
                                " L:" ++ show low ++
                                " C:" ++ show close)

                        let action = -- the real action and what it sends back to C#
                                if close > open then "BUY"
                                else if close < open then "SELL"
                                else "HOLD"

                        -- WRITE TO CSV, needs to be after action
                        hPutStrLn handle $
                            show o ++ "," ++
                            show h ++ "," ++
                            show l ++ "," ++
                            show c ++ "," ++
                            action

                        putStrLn ("Sending: " ++ action)
                        NBS.sendAll conn (BS.pack (action ++ "\n"))

parseCandle :: String -> Maybe (Double, Double, Double, Double) -- parses the Raw Candle data from C and makes it useable in haskell
parseCandle str =
    case splitComma str of
        [o,h,l,c] -> do  -- 'o' opening (first trades price), 'h' high  (top wick), 'l' Low (bottom wick), 'c' clsoe 'last trade done that candle'
            open  <- readMaybe o
            high  <- readMaybe h
            low   <- readMaybe l
            close <- readMaybe c
            return (open,high,low,close)
        _ -> Nothing

splitComma :: String -> [String] -- takes the new list of items from above and makes them each their own thing
splitComma s =
    case break (== ',') s of
        (a, ',' : b) -> a : splitComma b
        (a, "")      -> [a]

resolve :: IO AddrInfo
resolve = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) Nothing (Just "5000")

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 1
    return sock