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

getAddrBTC :: PersistentConns -> IO B.ByteString
getAddrBTC conn = do
    resp <- timeout 1000000 $ send conn "address"
    case resp of
        Just a -> return a
        Nothing -> getAddr conn

getAddrSafeExc :: PersistentConns -> SomeException -> IO B.ByteString
getAddrSafeExc conn _ = do
    threadDelay 1000000
    getAddrSafe conn

getAddrSafe :: PersistentConns -> IO B.ByteString
getAddrSafe conn = catch (getAddr conn) (getAddrSafeExc conn)

getAddr :: PersistentConns -> IO B.ByteString
getAddr conn = do
    mine_addr <- get_mining_address conn
    case mine_addr of
        Just a  -> return a
        Nothing -> do
            raw_addr <- getAddrBTC conn
            _ <- set_mining_address conn raw_addr
            resp <- get_mining_address conn
            case resp of
                Just a -> return a
                Nothing -> getAddr conn

main :: IO ()
main = do
    conn <- makeCons
    addr <- getAddrSafe conn
    BC.putStrLn addr
