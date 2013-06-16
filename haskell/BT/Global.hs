module BT.Global(makeCons, getConfigP) where

import Database.Redis as RD
import System.ZMQ3 as ZMQ hiding (poll)
import Data.Pool
import BT.Types
import BT.Polling
import BT.Config
import BT.Util
import BT.Log
import Data.IORef (newIORef)
import Control.Concurrent (forkIO)
import Data.Configurator.Types (Configured)
import Control.Exception (catch)

makeZMQSocket :: ZMQ.Context -> String -> IO (ZMQ.Socket ZMQ.Req)
makeZMQSocket ctx addr = do
    s <- ZMQ.socket ctx ZMQ.Req
    ZMQ.connect s addr
    return s

getConfigP :: Configured a => PersistentConns -> String -> IO a
getConfigP p = getConfig (config p)

makeCons :: IO PersistentConns
makeCons = do
    ctx <- ZMQ.context
    ZMQ.setIoThreads 4 ctx
    zMQPool <- Data.Pool.createPool (makeZMQSocket ctx "tcp://127.0.0.1:3333") ZMQ.close 1 5 50
    mineZMQPool <- Data.Pool.createPool (makeZMQSocket ctx "tcp://127.0.0.1:4444") ZMQ.close 1 5 50
    conn <- RD.connect defaultConnectInfo{connectPort = UnixSocket "/tmp/redis.sock", connectMaxConnections=500}
    payout <- newIORef 0
    target <- newIORef 0
    sink <- loggingSink
    configuration <- makeConfig
    let p = PersistentConns{
        redis=conn,
        pool=zMQPool,
        minePool=mineZMQPool,
        curPayout=payout,
        curTarget=target,
        config=configuration,
        logsink=sink
        }
    _ <- forkIO $ catch (poll p) logCatch
    return p
