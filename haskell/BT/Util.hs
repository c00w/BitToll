{-# LANGUAGE OverloadedStrings #-}
module BT.Util(jsonRPC, random256String, getRight, getMaybe, getResult, satoshi_big, satoshi_sub, satoshi_add, checkWatch, getRequestBody, getAesonResult) where
import System.Random (randomIO)
import Numeric (showHex)
import Data.Word (Word64)
import BT.Types
import Control.Exception(throw)
import Text.JSON (Result(Ok, Error))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Network.Bitcoin (BTC)
import Database.Redis (Redis)
import qualified Data.Aeson as A
import Data.Conduit
import Data.Conduit.List (consume)
import Control.Applicative
import Network.Wai (Request, requestBody)
import Data.Monoid (mconcat)

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

getResult :: Result a -> a
getResult res = case res of
    Ok val -> val
    Error err -> error err

getAesonResult :: A.Result a -> a
getAesonResult res = case res of
    A.Error err -> error err
    A.Success a -> a

getMaybe :: MyException -> Maybe a -> a
getMaybe b may = case may of
    Just a -> a
    _ -> throw b

getRight :: (a -> MyException) -> Either a b -> b
getRight exc i = case i of
    Right a -> a
    Left b -> throw (exc b)

checkWatch :: (Either a b) -> Redis ()
checkWatch a = do 
    case a of
        Left _ -> return $ error "watch"
        _ -> return ()

satoshi_big :: B.ByteString -> B.ByteString -> Bool 
satoshi_big a b = aBTC >= bBTC
    where
        aBTC = (read . BC.unpack) a :: BTC
        bBTC = (read . BC.unpack) b :: BTC

satoshi_sub :: B.ByteString -> B.ByteString -> B.ByteString
satoshi_sub a b = BC.pack . show $ aBTC - bBTC
    where
        aBTC = (read . BC.unpack) a :: BTC
        bBTC = (read . BC.unpack) b :: BTC

satoshi_add :: B.ByteString -> B.ByteString -> B.ByteString
satoshi_add a b = BC.pack . show $ aBTC + bBTC
    where
        aBTC = (read . BC.unpack) a :: BTC
        bBTC = (read . BC.unpack) b :: BTC

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = BL.fromStrict <$> mconcat <$> runResourceT (requestBody req $$ consume)

jsonRPC :: A.ToJSON a => A.Value -> a -> BL.ByteString 
jsonRPC rid mess = A.encode . A.object $ ["result" A..= (A.toJSON mess),
                          "error" A..= A.Null,
                          "id" A..= rid]
