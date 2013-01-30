{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, ) where
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
import BT.Global


randomInt :: PersistentConns -> IO B.ByteString
randomInt conns = do
    fd <- openBinaryFile "/dev/urandom" ReadMode
    randata <- hGetContents fd
    hClose fd
    return $ BC.pack $ take 256 randata

hex = foldr showHex "" . B.unpack

register :: Request -> PersistentConns -> IO BL.ByteString
register info conn = do
    usernum <- randomInt conn
    saltnum <- randomInt conn
    let user = hex usernum
    let salt = hex usernum
    ok <- runRedis (redis conn) $ do
        setnx (BC.pack $"user_" ++ user) (BC.pack salt)
    
    case ok of
        Right True -> return $ pack $ encode $ toJSObject [("username"::String, user), ("salt", salt)]
        _ -> register info conn
