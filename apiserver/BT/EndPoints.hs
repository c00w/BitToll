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
import System.Random (randomIO)
import qualified System.ZMQ3 as ZMQ
import Data.Pool
import Data.Word


randomNum :: IO Word64
randomNum = randomIO

register :: Request -> PersistentConns-> IO BL.ByteString
register info conn = do
    usernum <- randomNum
    saltnum <- randomNum
    let user = showHex usernum ""
    let salt = showHex usernum ""
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
