{-# LANGUAGE OverloadedStrings #-}

module BT.Polling where



import BT.ZMQ
import BT.Types
import Data.Pool (Pool)
import qualified System.ZMQ3 as ZMQ
import Data.IORef (IORef, writeIORef)
import Network.Bitcoin (BTC)
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

pollOnce :: Pool (ZMQ.Socket ZMQ.Req) -> IORef BTC -> B.ByteString -> IO ()
pollOnce conn store name = do
    valueraw <- sendraw conn name
    let value = read . BC.unpack $ valueraw :: BTC
    liftIO $ writeIORef store value

poll :: PersistentConns -> IO ()
poll conns = do
    liftIO $ threadDelay 60000000 -- 60 seconds
    pollOnce (pool conns) (curPayout conns) "payout"
    pollOnce (pool conns) (curTarget conns) "target"
    poll conns
