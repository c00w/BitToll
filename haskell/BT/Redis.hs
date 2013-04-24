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
        DR.get $ BC.append key user
    return $ getRightRedis ok

set :: PersistentConns -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Bool)
set conn key user value = do
    ok <- liftIO $ DR.runRedis (redis conn) $ do
        DR.set (BC.append key user) value
    return $ (getRightRedis ok) == DR.Ok
