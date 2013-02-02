module BT.Global(PersistentConns, makeCons, redis) where

import Database.Redis as RD
import System.IO
import System.ZMQ3 as ZMQ

data PersistentConns = PersistentConns {
        redis :: RD.Connection
        }

makeCons :: IO PersistentConns
makeCons = do
    conn <- RD.connect defaultConnectInfo{connectPort = UnixSocket "/tmp/redis.sock", connectMaxConnections=500}
    return PersistentConns{redis=conn}
