{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (take, drop)
import Data.ByteString (ByteString, drop, take)
import Data.ByteString.Lazy (toStrict)
import Data.Map

import qualified System.ZMQ3 as ZMQ
import Control.Monad
import Network.Bitcoin as BTC
import Data.Text.Encoding (decodeUtf8)

import Data.Aeson

bcdmine :: Auth
bcdmine = BTC.Auth "http://127.0.0.1:9332" "x" "x"

main :: IO ()
main = do
    let bindTo = "tcp://*:4444"
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s bindTo
            forever $ do
                request <- ZMQ.receive s
                resp <- route request
                ZMQ.send s [] $ resp

router :: Map ByteString (ByteString -> IO ByteString)
router = Data.Map.fromList $ [
    ("getwork", getwork bcdmine),
    ("recvwork", recvwork bcdmine)
    ]

route :: ByteString -> IO ByteString
route request = case Data.Map.lookup (take 7 request) router of
    Just a -> a (drop 7 request)
    Nothing -> case Data.Map.lookup (take 8 request) router of
        Just a -> a (drop 8 request)
        Nothing -> return "Error"

getwork :: BTC.Auth -> ByteString -> IO ByteString
getwork auth req = do
    recv <- BTC.getWork auth
    return . toStrict . encode $ recv

recvwork :: BTC.Auth -> ByteString -> IO ByteString
recvwork auth req = do
    resp <- BTC.solveBlock auth (decodeUtf8 req)
    return . toStrict . encode $ resp

