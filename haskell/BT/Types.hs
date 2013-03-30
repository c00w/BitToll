{-# LANGUAGE DeriveDataTypeable #-}
module BT.Types where 

import Database.Redis as RD
import System.ZMQ3 as ZMQ
import Data.Pool
import Data.Typeable
import Control.Exception

data PersistentConns = PersistentConns {
    redis :: RD.Connection,
    pool :: Data.Pool.Pool (ZMQ.Socket ZMQ.Req),
    mine_pool :: Data.Pool.Pool (ZMQ.Socket ZMQ.Req)
}

data MyException = RedisException String | BackendException String | UserException String | SomeException
    deriving (Show, Typeable)

instance Exception MyException
