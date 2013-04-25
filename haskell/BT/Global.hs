module BT.Global(makeCons) where

import Database.Redis as RD
import System.ZMQ3 as ZMQ
import Data.Pool
import BT.Types
import BT.Polling
import Data.IORef (newIORef)
import Control.Concurrent (forkIO)

makeZMQSocket :: ZMQ.Context -> IO (ZMQ.Socket ZMQ.Req)
makeZMQSocket ctx = do
    let connectTo = "tcp://127.0.0.1:3333"
    s <- ZMQ.socket ctx ZMQ.Req
    ZMQ.connect s connectTo
    return s

makemineZMQSocket :: ZMQ.Context -> IO (ZMQ.Socket ZMQ.Req)
makemineZMQSocket ctx = do
    let connectTo = "tcp://127.0.0.1:4444"
    s <- ZMQ.socket ctx ZMQ.Req
    ZMQ.connect s connectTo
    return s

makeCons :: IO PersistentConns
makeCons = do
    ctx <- ZMQ.context
    ZMQ.setIoThreads 4 ctx
    zmq_pool <- Data.Pool.createPool (makeZMQSocket ctx) ZMQ.close 1 5 50
    mine_zmq_pool <- Data.Pool.createPool (makemineZMQSocket ctx) ZMQ.close 1 5 50
    conn <- RD.connect defaultConnectInfo{connectPort = UnixSocket "/tmp/redis.sock", connectMaxConnections=500}
    payout <- newIORef 0
    target <- newIORef 0
    let p = PersistentConns{
        redis=conn,
        pool=zmq_pool,
        mine_pool=mine_zmq_pool,
        curPayout=payout,
        curTarget=target
        }
    _ <- forkIO $ poll p
    return p
