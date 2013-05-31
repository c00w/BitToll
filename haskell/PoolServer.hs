{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (take, drop)
import Data.ByteString (ByteString, drop, take)
import Data.ByteString.Lazy (toStrict)
import Data.Map

import qualified System.ZMQ3 as ZMQ
import Control.Monad
import Network.Bitcoin as BTC
import Data.Text.Encoding (decodeUtf8)
import Data.Text (pack)

import Data.Aeson

import BT.Config
import BT.Log

makebcd :: IO Auth
makebcd = do
    config <- makeConfig
    host <- getConfig config "p2pool.host" :: IO String
    port <- getConfig config "p2pool.port" :: IO Int
    let url = pack $ "http://" ++ host ++ ":" ++ show port
    return $ BTC.Auth url "x" "x"

main :: IO ()
main = do
    bcd <- makebcd
    let routes = makeRouter bcd
    let bindTo = "tcp://*:4444"
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s bindTo
            forever $ do
                request <- ZMQ.receive s
                resp <- route routes request
                ZMQ.send s [] resp

makeRouter :: Auth -> Map ByteString (ByteString -> IO ByteString)
makeRouter bcdmine = Data.Map.fromList [
    ("getwork", getwork bcdmine),
    ("recvwork", recvwork bcdmine)
    ]

route :: Map ByteString (ByteString -> IO ByteString) -> ByteString -> IO ByteString
route router request = case Data.Map.lookup (take 7 request) router of
    Just a -> a (drop 7 request)
    Nothing -> case Data.Map.lookup (take 8 request) router of
        Just a -> a (drop 8 request)
        Nothing -> return "Error"

getwork :: BTC.Auth -> ByteString -> IO ByteString
getwork auth _ = do
    recv <- BTC.getWork auth
    return . toStrict . encode $ recv

recvwork :: BTC.Auth -> ByteString -> IO ByteString
recvwork auth req = do
    logMsg "recvwork"
    resp <- BTC.solveBlock auth (decodeUtf8 req)
    logMsg $ "resp" ++ show resp
    logMsg $ "resp enc" ++ (show.encode) resp
    return . toStrict . encode $ resp

