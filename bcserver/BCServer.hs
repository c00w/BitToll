{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (take, drop)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString, drop, take)
import Data.Map

import qualified System.ZMQ3 as ZMQ
import Control.Concurrent
import Control.Monad
import Network.Bitcoin as BTC
import Data.Text.Encoding (encodeUtf8)

bcd = BTC.Auth "http://127.0.0.1:8332" "FGHJUYTUJKNMBVCCDFSTRdfyhydsaoiuyaustdyutyoiurewri" "jakhdkjahslkjdhlkjfhdskjlhflkjHJITYUIOTRRRRYII"

router = Data.Map.fromList $ [
    ("address", getaddress bcd)]

route :: ByteString -> IO ByteString
route request = case Data.Map.lookup (take 7 request) router of
    Nothing -> return "Error"
    Just a -> a (drop 7 request)

getaddress :: BTC.Auth -> ByteString -> IO ByteString
getaddress auth req = do
    addr <- BTC.getNewAddress auth Nothing
    return $ encodeUtf8 addr

main = do
    let bindTo = "tcp://*:3333"
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s bindTo
            forever $ do
                request <- ZMQ.receive s
                resp <- route request
                ZMQ.send s [] $ resp
