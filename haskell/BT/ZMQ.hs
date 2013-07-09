{-# LANGUAGE OverloadedStrings  #-}

module BT.ZMQ where

import qualified System.ZMQ3 as ZMQ
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import Data.Pool (withResource, Pool)
import BT.Types
import Control.Monad (when)
import Control.Exception (throw)

sendraw :: Data.Pool.Pool (ZMQ.Socket ZMQ.Req) -> B.ByteString -> IO B.ByteString
sendraw conn msg = liftIO $ withResource conn (\s -> do
        liftIO $ ZMQ.send s [] msg
        recv <- liftIO $ ZMQ.receive s
        when (recv == "error") $ do throw $ BackendException "zmq error"
        return recv
        )

send :: PersistentConns -> B.ByteString -> IO B.ByteString
send conn = sendraw (pool conn)

sendmine :: PersistentConns -> B.ByteString -> IO B.ByteString
sendmine conn = sendraw (minePool conn)
