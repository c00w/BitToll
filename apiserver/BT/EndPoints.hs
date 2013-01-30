{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints where
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Database.Redis
import Network.Wai (Request)
import Numeric (showHex)
import System.Random (getStdRandom, random)
import Text.JSON (encode)
import Control.Monad.IO.Class (liftIO)

helloworld :: Request -> IO BL.ByteString
helloworld info = do
    return $ BL.fromChunks ["Hello", "World"]


randomInt :: IO Int
randomInt = do
    getStdRandom(random)

username :: String
username = "username"

register :: Request -> IO BL.ByteString
register info = do
    usernum <- randomInt 
    saltnum <- randomInt 
    let user = showHex usernum ""
    let salt = showHex saltnum ""
    conn <- connect defaultConnectInfo
    ok <- runRedis conn $ do
        liftIO $ setnx (BC.pack user) (BC.pack salt)
    case ok of
        True -> return $ pack $ encode [(username,user), ("secret", salt)]
        False -> register info
