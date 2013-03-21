module BT.Types where 

import Database.Redis as RD
import System.ZMQ3 as ZMQ
import Data.Pool


data PersistentConns = PersistentConns {
    redis :: RD.Connection,
    pool :: Data.Pool.Pool (ZMQ.Socket ZMQ.Req)
}

