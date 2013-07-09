{-# LANGUAGE OverloadedStrings #-}


module BT.Util where



import System.Random (randomIO)
import Numeric (showHex)
import Data.Word (Word64)
import BT.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Data.Conduit
import Data.Conduit.List (consume)
import Control.Applicative
import Crypto.Hash.SHA512
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request, requestBody)
import Data.Monoid (mconcat)
import Data.String (IsString)
import Data.Maybe (fromMaybe)
import Control.Exception (throw)
import qualified BT.Log
import Data.Time.Clock(UTCTime)
import qualified Data.ByteString as B

hashPass :: B.ByteString -> B.ByteString -> B.ByteString
hashPass salt pass = hash.B.append (hash.B.append pass $ salt ) $ salt

elogCatch :: MyException -> IO (Maybe a)
elogCatch e = do
    BT.Log.elogMsg . show $ e
    return Nothing

logCount :: PersistentConns -> B.ByteString -> B.ByteString -> Integer -> IO ()
logCount conns = BT.Log.rlogCount (logSink conns)

logTimer :: PersistentConns -> B.ByteString -> B.ByteString -> UTCTime -> IO ()
logTimer conns = BT.Log.rlogTimer (logSink conns)


logCatch :: MyException -> IO ()
logCatch = BT.Log.logMsg . show

logMsg :: String -> IO ()
logMsg = BT.Log.logMsg

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

getMaybe :: MyException -> Maybe a -> IO a
getMaybe b may = case may of
    Just a -> return a
    _ -> throw b

zeroMaybe :: IsString a => Maybe a -> a
zeroMaybe = fromMaybe "0"

getRight :: (a -> MyException) -> Either a b -> IO b
getRight exc i = case i of
    Right a -> return a
    Left b -> throw (exc b)

getRightRedis :: Show a => Either a b -> IO b
getRightRedis = getRight (RedisException . show)

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = liftIO $ BL.fromStrict <$> mconcat <$> runResourceT (requestBody req $$ consume)

jsonRPC :: A.ToJSON a => A.Value -> a -> BL.ByteString 
jsonRPC rid mess = A.encode . A.object $ [
                        "result" A..= A.toJSON mess,
                        "error" A..= A.Null,
                        "id" A..= rid]
