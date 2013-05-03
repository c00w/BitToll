{-# LANGUAGE OverloadedStrings #-}
import BT.Redis (setnx, get)
import BT.Global
import BT.Types
import BT.ZMQ
import System.Timeout (timeout)
import Control.Exception (catch, SomeException)

import qualified Data.ByteString as B

get_mining_address :: PersistentConns -> IO (Maybe B.ByteString)
get_mining_address conn = get conn "g:" "global" "mining_address"

set_mining_address :: PersistentConns -> B.ByteString -> IO (Bool)
set_mining_address conn value = setnx conn "g:" "global" "mining_address" value

get_addr_btc :: PersistentConns -> IO (B.ByteString)
get_addr_btc conn = do
    resp <- timeout 1000000 $ send conn "address"
    case resp of
        Just a -> return a
        Nothing -> get_addr conn

get_addr_safe_exc :: PersistentConns -> SomeException -> IO B.ByteString
get_addr_safe_exc conn _ = get_addr_safe conn

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
    B.putStrLn addr
