module BT.ZMQ where
import qualified System.ZMQ3 as ZMQ
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import Data.Pool (withResource, Pool)
import BT.Types

sendraw :: Data.Pool.Pool (ZMQ.Socket ZMQ.Req) -> B.ByteString -> IO B.ByteString
sendraw conn msg = do
    resp <- liftIO $ withResource conn (\s -> do
        liftIO $ ZMQ.send s [] msg
        liftIO $ ZMQ.receive s)
    return resp

send :: PersistentConns -> B.ByteString -> IO B.ByteString
send conn msg = sendraw (pool conn) msg
