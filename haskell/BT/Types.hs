{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module BT.Types where 

import qualified Database.Redis as RD
import qualified System.ZMQ3 as ZMQ
import Data.Pool (Pool)
import Data.Typeable
import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)
import Data.IORef (IORef)
import Network.Bitcoin (BTC)
import Data.Configurator.Types (Config)
import Control.Exception (Exception)
import Network.Metric.Sink.Statsd (AnySink)

data PersistentConns = PersistentConns {
    redis       :: RD.Connection,
    pool        :: Pool (ZMQ.Socket ZMQ.Req),
    minePool    :: Pool (ZMQ.Socket ZMQ.Req),
    curPayout   :: IORef BTC,
    curTarget   :: IORef BTC,
    config      :: Config,
    logsink     :: AnySink
}

data MyException = RedisException String | BackendException String | UserException String | SomeException
    deriving (Show, Typeable)

instance Exception MyException

data MiningData = MiningData {
    method :: String,
    rpcid :: Value,
    getwork :: [String]
}

instance FromJSON MiningData where
    parseJSON (Object o) = MiningData <$> o .: "method"
                                      <*> o .: "id"
                                      <*> o .: "params"
    parseJSON _ = mzero
