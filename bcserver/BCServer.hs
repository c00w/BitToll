{-# LANGUAGE OverloadedStrings #-}
 
import Data.ByteString.Char8

import qualified System.ZMQ3 as ZMQ
import Control.Concurrent
import Control.Monad

main = do
    let bindTo = "tcp://*:3333"
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s bindTo
            forever $ do
                request <- ZMQ.receive s
                ZMQ.send s [] "PONG"
