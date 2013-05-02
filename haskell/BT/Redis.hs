{-# LANGUAGE OverloadedStrings #-}
module BT.Redis where
import qualified Database.Redis as DR
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import BT.Types
import BT.Util
import Control.Monad.IO.Class (liftIO)

get :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)
get conn itemType item key = do
    ok <- liftIO $ DR.runRedis (redis conn) $ do
        DR.hget (BC.append itemType item) key
    return $ getRightRedis ok

set :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Bool)
set conn itemType item key value = do
    ok <- liftIO $ DR.runRedis (redis conn) $ do
        DR.hset (BC.append itemType item) key value
    return $ (getRightRedis ok)

setnx :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Bool)
setnx conn itemType item key value = do
    ok <- liftIO $ DR.runRedis (redis conn) $ do
        DR.hsetnx (BC.append itemType item) key value
    return $ (getRightRedis ok)
