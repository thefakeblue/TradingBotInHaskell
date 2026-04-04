import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever)
import Text.Read (readMaybe)

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve
    sock <- open addr
    putStrLn "Server listening on port 5000..."

    (conn, _) <- accept sock
    putStrLn "Client connected"

    forever $ do
        msg <- NBS.recv conn 1024
        if BS.null msg
            then return ()
            else do
                let str = BS.unpack msg
                putStrLn ("Received: " ++ str)

                case parseEMAs str of
                    Nothing -> do
                        putStrLn "Parse error"
                        NBS.sendAll conn (BS.pack "HOLD\n")

                    Just (fast, slow) -> do
                        let action =
                                if fast > slow then "BUY"
                                else if fast < slow then "SELL"
                                else "HOLD"

                        putStrLn ("Sending: " ++ action)
                        NBS.sendAll conn (BS.pack (action ++ "\n"))

parseEMAs :: String -> Maybe (Double, Double)
parseEMAs str =
    case break (== ',') str of
        (f, ',' : s) -> do
            fast <- readMaybe f
            slow <- readMaybe s
            return (fast, slow)
        _ -> Nothing

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