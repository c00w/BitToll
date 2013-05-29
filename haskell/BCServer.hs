{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (take, drop)
import Data.ByteString.Char8 (pack, split, unpack, append)
import Data.ByteString (ByteString, drop, take)
import qualified Data.Map
import Data.Ratio

import qualified System.ZMQ3 as ZMQ
import Control.Monad
import Network.Bitcoin as BTC
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import BT.Config
import BT.Log

bcd :: Auth
bcd = BTC.Auth "http://127.0.0.1:19001" "FGHJUYTUJKNMBVCCDFSTRdfyhydsaoiuyaustdyutyoiurewri" "jakhdkjahslkjdhlkjfhdskjlhflkjHJITYUIOTRRRRYII"

main :: IO ()
main = do
    let bindTo = "tcp://*:3333"
    config <- makeConfig
    minconf <- getConfig config "btc.confirmation" :: IO Int
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s bindTo
            forever $ do
                request <- ZMQ.receive s
                resp <- route request minconf
                ZMQ.send s [] $ resp

router :: Data.Map.Map ByteString (ByteString -> Int -> IO ByteString)
router = Data.Map.fromList $ [
    ("recieved", getrecieved bcd),
    ("payout", getpayout bcd),
    ("target", gettarget bcd),
    ("sendto", sendBTC bcd),
    ("address", getaddress bcd)]

route :: ByteString -> Int -> IO ByteString
route request minconf = do
    logMsg $ unpack (append "Handling " request)
    resp <- case Data.Map.lookup (take 6 request) router of
        Just a -> a (drop 6 request) minconf
        Nothing -> case Data.Map.lookup (take 7 request) router of
            Just a -> a (drop 7 request) minconf
            Nothing -> case Data.Map.lookup (take 8 request) router of
                Just a -> a (drop 8 request) minconf
                Nothing -> return "Error"
    logMsg $ unpack resp
    return resp

getrecieved :: BTC.Auth -> ByteString -> Int -> IO ByteString
getrecieved auth req minconf = do
    recv <- BTC.getReceivedByAddress' auth (decodeUtf8 req) minconf
    return $ pack $ show recv

getaddress :: BTC.Auth -> ByteString -> Int -> IO ByteString
getaddress auth _ _ = do
    addr <- BTC.getNewAddress auth Nothing
    return $ encodeUtf8 addr

sendBTC :: BTC.Auth -> ByteString -> Int -> IO ByteString
sendBTC auth msg _ = do
    let address = decodeUtf8 . head . split '|' $ msg :: Address
    let amount = read. unpack . last . split '|' $ msg :: BTC
    resp <- BTC.sendToAddress auth address amount Nothing Nothing
    return $ encodeUtf8 resp

gettarget :: BTC.Auth -> ByteString -> Int -> IO ByteString
gettarget auth _ _ = do
    work <- BTC.getWork auth
    let target = hdTarget work
    return $ encodeUtf8 target

getpayout :: BTC.Auth -> ByteString -> Int -> IO ByteString
getpayout auth _ _ = do
    blockcount  <- BTC.getBlockCount auth
    let blockmult = blockcount % 210000 :: Rational -- forms fraction not mod
    let halves = truncate blockmult :: Int
    return $ (pack . show) (50 * ((1/2 :: Double) ^ (halves)))
