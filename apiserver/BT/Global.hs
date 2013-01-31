module BT.Global(PersistentConns, makeCons, redis) where

import Database.Redis
import System.IO

data PersistentConns = PersistentConns {
        redis :: Connection
        }

makeCons :: IO PersistentConns
makeCons = do
    conn <- connect defaultConnectInfo{connectPort = UnixSocket "/tmp/redis.sock", connectMaxConnections=500}
    return PersistentConns{redis=conn}
