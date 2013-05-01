{-# LANGUAGE OverloadedStrings #-}
module BT.Redis where
import qualified Database.Redis as DR
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import BT.Types
import BT.Util
import Control.Monad.IO.Class (liftIO)

get :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Maybe B.ByteString)
get conn key user = do
    ok <- liftIO $ DR.runRedis (redis conn) $ do
        DR.hget (BC.append "u:" user) key
    return $ getRightRedis ok

set :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Bool)
set conn key user value = do
    ok <- liftIO $ DR.runRedis (redis conn) $ do
        DR.hset (BC.append "u:" user) key value
    return $ (getRightRedis ok)

setnx :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Bool)
setnx conn key user value = do
    ok <- liftIO $ DR.runRedis (redis conn) $ do
        DR.hsetnx (BC.append "u:" user) key value
    return $ (getRightRedis ok)
