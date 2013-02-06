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
import Data.Pool (withResource)
import Data.Word (Word64)


randomNum :: IO Word64
randomNum = randomIO

randomString :: IO String
randomString = do
    a <- randomNum
    return $ showHex a ""

random256String :: IO String
random256String = do
    a <- randomString
    b <- randomString
    c <- randomString
    d <- randomString
    return $ a ++ b ++ c ++ d

register :: Request -> PersistentConns-> IO BL.ByteString
register info conn = do
    user <- random256String
    salt <- random256String
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
