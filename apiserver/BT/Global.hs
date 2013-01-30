module BT.Global(PersistentConns, makeCons, randomfd, redis) where

import Database.Redis
import System.IO

data PersistentConns = PersistentConns {
        randomfd :: Handle,
        redis :: Connection
        }

makeCons :: IO PersistentConns
makeCons = do
    conn <- connect defaultConnectInfo{connectPort = UnixSocket "/tmp/redis.sock", connectMaxConnections=500}
    fd <- openBinaryFile "/dev/urandom" ReadMode
    return PersistentConns{ randomfd=fd, redis=conn}
