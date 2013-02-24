{-# LANGUAGE OverloadedStrings #-}
 
import Data.ByteString.Char8 

import qualified System.ZMQ3 as ZMQ
import Control.Concurrent

main = do
    let bindTo = "tcp://127.0.0.1:3333"
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Req $ \s -> do
            ZMQ.connect s bindTo
            ZMQ.send s [] "recievedasd"
            request <- ZMQ.receive s
            Data.ByteString.Char8.putStrLn request
