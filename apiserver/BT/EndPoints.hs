{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, helloworld) where
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Database.Redis
import Network.Wai (Request)
import Numeric (showHex)
import System.Random (getStdRandom, random)
import Text.JSON

helloworld :: Request -> Connection -> IO BL.ByteString
helloworld info _ = do
    return $ BL.fromChunks ["Hello", "World"]


randomInt :: IO Int
randomInt = do
    num <- getStdRandom(random)
    return $ abs num

data RegisterResp = RegisterResp { username :: String, secret:: String }

register :: Request -> Connection -> IO BL.ByteString
register info conn = do
    usernum <- randomInt 
    saltnum <- randomInt 
    let user = showHex usernum ""
    let salt = showHex saltnum ""
    ok <- runRedis conn $ do
        setnx (BC.pack user) (BC.pack salt)
    
    case ok of
        Right True -> return $ pack $ encode $ toJSObject [("username"::String, user), ("salt", salt)]
        _ -> register info conn
