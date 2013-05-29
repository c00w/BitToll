{-# LANGUAGE OverloadedStrings #-}
module BT.Util where 
import System.Random (randomIO)
import Numeric (showHex)
import Data.Word (Word64)
import BT.Types
import Control.Exception(throw)
import Text.JSON (Result(Ok, Error))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Data.Conduit
import Data.Conduit.List (consume)
import Control.Applicative
import Network.Wai (Request, requestBody)
import Data.Monoid (mconcat)
import Data.String (IsString)

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

zeroMaybe :: IsString a => Maybe a -> a
zeroMaybe a = case a of
    Just b  -> b
    Nothing -> "0"


getRight :: (a -> MyException) -> Either a b -> b
getRight exc i = case i of
    Right a -> a
    Left b -> throw (exc b)

getRightRedis :: Show a => Either a b -> b
getRightRedis = getRight (\s -> RedisException (show s))

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = BL.fromStrict <$> mconcat <$> runResourceT (requestBody req $$ consume)

jsonRPC :: A.ToJSON a => A.Value -> a -> BL.ByteString 
jsonRPC rid mess = A.encode . A.object $ [
                        "result" A..= (A.toJSON mess),
                        "error" A..= A.Null,
                        "id" A..= rid]
