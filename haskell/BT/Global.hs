module BT.Global(makeCons) where

import Database.Redis as RD
import System.ZMQ3 as ZMQ hiding (poll)
import Data.Pool
import BT.Types
import BT.Polling
import Data.IORef (newIORef)
import Control.Concurrent (forkIO)
import Data.Configurator (Worth(Required), load)
import Data.Configurator.Types (Config)

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

makeConfig :: IO (Config)
makeConfig = load [Required "/etc/bittoll/bittoll.conf"]

makeCons :: IO PersistentConns
makeCons = do
    ctx <- ZMQ.context
    ZMQ.setIoThreads 4 ctx
    zmq_pool <- Data.Pool.createPool (makeZMQSocket ctx) ZMQ.close 1 5 50
    mine_zmq_pool <- Data.Pool.createPool (makemineZMQSocket ctx) ZMQ.close 1 5 50
    conn <- RD.connect defaultConnectInfo{connectPort = UnixSocket "/tmp/redis.sock", connectMaxConnections=500}
    payout <- newIORef 0
    target <- newIORef 0
    configuration <- makeConfig
    let p = PersistentConns{
        redis=conn,
        pool=zmq_pool,
        mine_pool=mine_zmq_pool,
        curPayout=payout,
        curTarget=target,
        config=configuration
        }
    _ <- forkIO $ poll p
    return p
