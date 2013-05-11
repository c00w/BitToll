{-# LANGUAGE OverloadedStrings #-}
import BT.Global
import BT.Types
import BT.Mining
import BT.ZMQ
import System.Timeout (timeout)
import Control.Exception (catch, SomeException)
import Control.Concurrent (threadDelay)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

get_addr_btc :: PersistentConns -> IO (B.ByteString)
get_addr_btc conn = do
    resp <- timeout 1000000 $ send conn "address"
    case resp of
        Just a -> return a
        Nothing -> get_addr conn

get_addr_safe_exc :: PersistentConns -> SomeException -> IO B.ByteString
get_addr_safe_exc conn _ = do
    threadDelay 1000000
    get_addr_safe conn

get_addr_safe :: PersistentConns -> IO B.ByteString
get_addr_safe conn = catch (get_addr conn) (get_addr_safe_exc conn)

get_addr :: PersistentConns -> IO B.ByteString
get_addr conn = do
    mine_addr <- get_mining_address conn
    case mine_addr of
        Just a  -> return a
        Nothing -> do
            raw_addr <- get_addr_btc conn
            _ <- set_mining_address conn raw_addr
            resp <- get_mining_address conn
            case resp of
                Just a -> return a
                Nothing -> get_addr conn

main :: IO ()
main = do
    conn <- makeCons
    addr <- get_addr_safe conn
    BC.putStrLn addr
