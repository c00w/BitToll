{-# LANGUAGE OverloadedStrings #-}
 
import Data.ByteString.Char8 

import qualified System.ZMQ3 as ZMQ
import Control.Concurrent

main = do
    let bindTo = "ipc:///tmp/bcbackend.sock"
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Req $ \s -> do
            ZMQ.connect s bindTo
            ZMQ.send s [] "PONG"
            request <- ZMQ.receive s
            Data.ByteString.Char8.putStrLn request
