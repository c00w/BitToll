{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, helloworld) where
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Database.Redis
import Network.Wai (Request)
import Numeric (showHex)
import System.Random (getStdRandom, random)
import Text.JSON
import System.IO

helloworld :: Request -> Connection -> IO BL.ByteString
helloworld info _ = do
    return $ BL.fromChunks ["Hello", "World"]

randomInt :: IO B.ByteString
randomInt = do
    fd <- openBinaryFile "/dev/urandom" ReadMode
    randata <- hGetContents fd
    return $ BC.pack $ take 256 randata

data RegisterResp = RegisterResp { username :: String, secret:: String }

hex = foldr showHex "" . B.unpack

register :: Request -> Connection -> IO BL.ByteString
register info conn = do
    usernum <- randomInt 
    saltnum <- randomInt 
    let user = hex usernum
    let salt = hex usernum
    ok <- runRedis conn $ do
        setnx (BC.pack user) (BC.pack salt)
    
    case ok of
        Right True -> return $ pack $ encode $ toJSObject [("username"::String, user), ("salt", salt)]
        _ -> register info conn
