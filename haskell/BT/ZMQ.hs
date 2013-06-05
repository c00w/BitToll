{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}

module BT.ZMQ where

import Control.Monad.Loc

import qualified System.ZMQ3 as ZMQ
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import Data.Pool (withResource, Pool)
import BT.Types

sendraw :: Data.Pool.Pool (ZMQ.Socket ZMQ.Req) -> B.ByteString -> BTIO B.ByteString
sendraw conn msg = liftIO $ withResource conn (\s -> do
        liftIO $ ZMQ.send s [] msg
        liftIO $ ZMQ.receive s)

send :: PersistentConns -> B.ByteString -> BTIO B.ByteString
send conn = sendraw (pool conn)

sendmine :: PersistentConns -> B.ByteString -> BTIO B.ByteString
sendmine conn = sendraw (minePool conn)
