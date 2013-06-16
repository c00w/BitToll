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
    getRightRedis ok

set :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO Bool
set conn itemType item key value = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.hset (BC.append itemType item) key value
    getRightRedis ok

rdel :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool 
rdel conn itemType item = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.del [BC.append itemType item]
    r <- getRightRedis ok
    return (r == 1)


increment :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> Integer -> IO Integer
increment conn itemType item key value = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.hincrby (BC.append itemType item) key value
    liftIO . putStrLn $ "increment item,itemype: " ++ show itemType ++ show item ++ "key: " ++ show key ++ " value:" ++ show value
    getRightRedis ok

expire :: PersistentConns -> B.ByteString -> B.ByteString -> Integer -> IO Bool
expire conn itemType item time = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.expire (BC.append itemType item) time
    getRightRedis ok


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
    return $ fromIntegral r

setnx :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO Bool
setnx conn itemType item key value = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.hsetnx (BC.append itemType item) key value
    getRightRedis ok

rsetnx :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> IO Bool
rsetnx conn itemType item value = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.setnx (BC.append itemType item) value
    getRightRedis ok


zrem :: PersistentConns -> B.ByteString -> B.ByteString -> [B.ByteString] -> IO Integer
zrem conn object key members = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.zrem (BC.append object key) members
    getRightRedis ok

zrangebyscore :: PersistentConns -> B.ByteString -> B.ByteString -> Double -> Double -> IO [B.ByteString]
zrangebyscore conn object key minscore maxscore = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.zrangebyscore (BC.append object key) minscore maxscore
    getRightRedis ok

zrangebyscoreWithscores :: PersistentConns -> B.ByteString -> B.ByteString -> Double -> Double -> IO [(B.ByteString, Double)]
zrangebyscoreWithscores conn object key minscore maxscore = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.zrangebyscoreWithscores (BC.append object key) minscore maxscore
    getRightRedis ok

zadd ::PersistentConns -> BC.ByteString -> BC.ByteString
              -> Double -> BC.ByteString -> IO Integer
zadd conn object key score member = do
    ok <- liftIO $ DR.runRedis (redis conn) $
        DR.zadd (BC.append object key) [(score, member)]
    getRightRedis ok
