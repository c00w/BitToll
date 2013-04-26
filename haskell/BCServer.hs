{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (take, drop)
import Data.ByteString.Char8 (pack, split, unpack)
import Data.ByteString (ByteString, drop, take)
import qualified Data.Map
import Data.Ratio

import qualified System.ZMQ3 as ZMQ
import Control.Monad
import Network.Bitcoin as BTC
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

bcd :: Auth
bcd = BTC.Auth "http://127.0.0.1:19001" "FGHJUYTUJKNMBVCCDFSTRdfyhydsaoiuyaustdyutyoiurewri" "jakhdkjahslkjdhlkjfhdskjlhflkjHJITYUIOTRRRRYII"

main :: IO ()
main = do
    let bindTo = "tcp://*:3333"
    ZMQ.withContext $ \c ->
        ZMQ.withSocket c ZMQ.Rep $ \s -> do
            ZMQ.bind s bindTo
            forever $ do
                request <- ZMQ.receive s
                resp <- route request
                ZMQ.send s [] $ resp

router :: Data.Map.Map ByteString (ByteString -> IO ByteString)
router = Data.Map.fromList $ [
    ("recieved", getrecieved bcd),
    ("payout", getpayout bcd),
    ("target", gettarget bcd),
    ("sendto", sendBTC bcd),
    ("address", getaddress bcd)]

route :: ByteString -> IO ByteString
route request = case Data.Map.lookup (take 6 request) router of
    Just a -> a (drop 6 request)
    Nothing -> case Data.Map.lookup (take 7 request) router of
        Just a -> a (drop 7 request)
        Nothing -> case Data.Map.lookup (take 8 request) router of
            Just a -> a (drop 8 request)
            Nothing -> return "Error"

getrecieved :: BTC.Auth -> ByteString -> IO ByteString
getrecieved auth req = do
    recv <- BTC.getReceivedByAddress' auth (decodeUtf8 req) 0
    return $ pack $ show recv

getaddress :: BTC.Auth -> ByteString -> IO ByteString
getaddress auth _ = do
    addr <- BTC.getNewAddress auth Nothing
    return $ encodeUtf8 addr

sendBTC :: BTC.Auth -> ByteString -> IO ByteString
sendBTC auth msg = do
    let address = read . unpack . head . split '|' $ msg :: Address
    let amount = read. unpack . last . split '|' $ msg :: BTC
    resp <- BTC.sendToAddress auth address amount Nothing Nothing
    return $ encodeUtf8 resp

gettarget :: BTC.Auth -> ByteString -> IO ByteString
gettarget auth _ = do
    work <- BTC.getWork auth
    let target = hdTarget work
    return $ encodeUtf8 target

getpayout :: BTC.Auth -> ByteString -> IO ByteString
getpayout auth _ = do
    blockcount  <- BTC.getBlockCount auth
    let blockmult = blockcount % 210000 :: Rational -- forms fraction not mod
    let halves = truncate blockmult :: Int
    return $ (pack . show) (50 * ((1/2 :: Double) ^ (halves)))
