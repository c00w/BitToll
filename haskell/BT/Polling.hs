{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}
module BT.Polling where

import Control.Monad.Loc

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

pollOnce :: Pool (ZMQ.Socket ZMQ.Req) -> IORef BTC -> B.ByteString -> BTIO ()
pollOnce conn store name = do
    valueraw <- sendraw conn name
    let value = read . BC.unpack $ valueraw :: BTC
    liftIO $ writeIORef store value

poll :: PersistentConns -> BTIO ()
poll conns = do
    liftIO $ threadDelay 60000000 -- 60 seconds
    pollOnce (pool conns) (curPayout conns) "payout"
    pollOnce (pool conns) (curTarget conns) "target"
    poll conns
