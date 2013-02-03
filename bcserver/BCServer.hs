{-# LANGUAGE OverloadedStrings #-}
 
import Data.ByteString.Char8

import qualified System.ZMQ3 as ZMQ
import Control.Concurrent
import Control.Monad
import Network.Bitcoin as BTC

main = do
    let bindTo = "tcp://*:3333"
    let bcd = BTC.Auth "http://127.0.0.1:8332" "FGHJUYTUJKNMBVCCDFSTRdfyhydsaoiuyaustdyutyoiurewri" "jakhdkjahslkjdhlkjfhdskjlhflkjHJITYUIOTRRRRYII"
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s bindTo
            forever $ do
                request <- ZMQ.receive s
                diff <- BTC.getDifficulty bcd
                ZMQ.send s [] $ pack $ show diff
