module BT.Global(PersistentConns, makeCons, redis, pool) where

import Database.Redis as RD
import System.IO
import System.ZMQ3 as ZMQ
import Data.Pool


data PersistentConns = PersistentConns {
        redis :: RD.Connection,
        pool :: Data.Pool.Pool (ZMQ.Socket ZMQ.Req)
        }

makeZMQSocket :: ZMQ.Context -> IO (ZMQ.Socket ZMQ.Req)
makeZMQSocket ctx = do
    let connectTo = "tcp://127.0.0.1:3333"
    s <- ZMQ.socket ctx ZMQ.Req
    ZMQ.connect s connectTo
    return s

makeCons :: IO PersistentConns
makeCons = do
    ctx <- ZMQ.context
    ZMQ.setIoThreads 4 ctx
    zmq_pool <- Data.Pool.createPool (makeZMQSocket ctx) ZMQ.close 1 5 50
    conn <- RD.connect defaultConnectInfo{connectPort = UnixSocket "/tmp/redis.sock", connectMaxConnections=500}
    return PersistentConns{redis=conn, pool=zmq_pool}
