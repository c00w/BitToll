{-# LANGUAGE OverloadedStrings #-}
module BT.Redis where
import qualified Database.Redis as DR
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import BT.Types
import BT.Util
import Control.Monad.IO.Class (liftIO)
import Network.Bitcoin (BTC)

get :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)
get conn itemType item key = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.hget (BC.append itemType item) key
    return $ getRightRedis ok

set :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO Bool
set conn itemType item key value = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.hset (BC.append itemType item) key value
    return $ getRightRedis ok

increment :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> Integer -> IO Integer
increment conn itemType item key value = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.hincrby (BC.append itemType item) key value
    return $ getRightRedis ok

getbtc :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> IO BTC
getbtc conn itemType item key = do
    r <- get conn itemType item key
    case r of
        Nothing -> return 0
        Just a -> return $ (read . BC.unpack $  a :: BTC) * (10^^(-8 :: Integer))

setbtc :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> BTC -> IO Bool
setbtc conn itemType item key value = set conn itemType item key (BC.pack . show $ (truncate (value * 10^^(8 :: Integer)) :: Integer))

setnxbtc :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> BTC -> IO Bool
setnxbtc conn itemType item key value = setnx conn itemType item key (BC.pack . show $ (truncate (value * 10^^(8 :: Integer)) :: Integer))

incrementbtc :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> BTC -> IO BTC
incrementbtc conn itemType item key value = do
    r <- increment conn itemType item key (truncate (value * 10^^(8 :: Integer)) :: Integer)
    return $ fromIntegral r :: BTC

setnx :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO Bool
setnx conn itemType item key value = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.hsetnx (BC.append itemType item) key value
    return $ getRightRedis ok

zrem :: PersistentConns -> B.ByteString -> B.ByteString -> [B.ByteString] -> IO Integer
zrem conn object key members = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.zrem (BC.append object key) members
    return $ getRightRedis ok

zrangebyscore :: PersistentConns -> B.ByteString -> B.ByteString -> Double -> Double -> IO [B.ByteString]
zrangebyscore conn object key minscore maxscore = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.zrangebyscore (BC.append object key) minscore maxscore
    return $ getRightRedis ok

zrangebyscoreWithscores :: PersistentConns -> B.ByteString -> B.ByteString -> Double -> Double -> IO [(B.ByteString, Double)]
zrangebyscoreWithscores conn object key minscore maxscore = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.zrangebyscoreWithscores (BC.append object key) minscore maxscore
    return $ getRightRedis ok

zadd ::PersistentConns -> BC.ByteString -> BC.ByteString
              -> Double -> BC.ByteString -> IO Integer
zadd conn object key score member = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.zadd (BC.append object key) [(score, member)]
    return $ getRightRedis ok
