{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}

module BT.Util where

import Control.Monad.Loc

import System.Random (randomIO)
import Numeric (showHex)
import Data.Word (Word64)
import BT.Types
import Control.Exception(throw)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Data.Conduit
import Data.Conduit.List (consume)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request, requestBody)
import Data.Monoid (mconcat)
import Data.String (IsString)
import Data.Maybe (fromMaybe)
import Control.Monad.Exception (EMT, showExceptionWithTrace)
import Control.Monad.Exception.Base (NoExceptions)
import qualified BT.Log

elogCatch :: [String] -> MyException -> EMT NoExceptions IO (Maybe a)
elogCatch loc e = do
    liftIO . BT.Log.elogMsg $ showExceptionWithTrace loc e
    return Nothing

logCatch :: [String] -> MyException -> EMT NoExceptions IO ()
logCatch loc e = liftIO . BT.Log.logMsg $ showExceptionWithTrace loc e

logMsg :: String -> BTIO ()
logMsg = liftIO . BT.Log.logMsg

randomNum :: BTIO Word64
randomNum = liftIO randomIO

randomString :: BTIO String
randomString = do
    a <- randomNum
    return $ showHex a ""

random256String :: BTIO String
random256String = do
    a <- randomString
    b <- randomString
    c <- randomString
    d <- randomString
    return $ a ++ b ++ c ++ d

getMaybe :: MyException -> Maybe a -> a
getMaybe b may = case may of
    Just a -> a
    _ -> throw b

zeroMaybe :: IsString a => Maybe a -> a
zeroMaybe = fromMaybe "0"

getRight :: (a -> MyException) -> Either a b -> b
getRight exc i = case i of
    Right a -> a
    Left b -> throw (exc b)

getRightRedis :: Show a => Either a b -> b
getRightRedis = getRight (RedisException . show)

getRequestBody :: Request -> BTIO BL.ByteString
getRequestBody req = liftIO $ BL.fromStrict <$> mconcat <$> runResourceT (requestBody req $$ consume)

jsonRPC :: A.ToJSON a => A.Value -> a -> BL.ByteString 
jsonRPC rid mess = A.encode . A.object $ [
                        "result" A..= A.toJSON mess,
                        "error" A..= A.Null,
                        "id" A..= rid]
