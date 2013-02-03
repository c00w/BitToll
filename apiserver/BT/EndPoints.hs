{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, deposit) where
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Database.Redis
import Network.Wai (Request)
import Numeric (showHex)
import Text.JSON
import BT.Global
import System.Entropy
import qualified System.ZMQ3 as ZMQ
import Data.Pool

hex = foldr showHex "" . B.unpack

register :: Request -> PersistentConns-> IO BL.ByteString
register info conn = do
    usernum <- getEntropy 256
    saltnum <- getEntropy 256
    let user = hex usernum
    let salt = hex usernum
    ok <- runRedis (redis conn) $ do
        setnx (BC.pack $"user_" ++ user) (BC.pack salt)
    
    case ok of
        Right True -> return $ pack $ encode $ toJSObject [("username"::String, user), ("salt", salt)]
        _ -> register info conn

deposit :: Request -> PersistentConns-> IO BL.ByteString
deposit info conn = do
    resp <- withResource (pool conn) (\s -> do
        ZMQ.send s [] "REQUEST"
        resp <- ZMQ.receive s
        return resp)
    return $ BL.fromChunks [resp]
