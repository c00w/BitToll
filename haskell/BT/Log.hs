{-# LANGUAGE OverloadedStrings #-}

module BT.Log where

import Control.Concurrent (myThreadId)
import Data.Time.Clock (diffUTCTime, getCurrentTime, UTCTime)
import System.IO (hPutStrLn, stderr)
import Network.Metric.Sink.Statsd (open, AnySink, push)
import Network.Metric (Metric(Counter, Timer))
import qualified Data.ByteString as B

loggingSink :: IO AnySink
loggingSink = open "bittoll" "127.0.0.1" 8125

rlogCount :: AnySink -> B.ByteString -> B.ByteString -> Integer -> IO ()
rlogCount ssink namespace bucket count = push ssink $ Counter namespace bucket count

rlogTimer :: AnySink -> B.ByteString -> B.ByteString -> UTCTime -> IO ()
rlogTimer ssink namespace bucket start = do
    end <- getCurrentTime
    let time = fromRational . toRational $ diffUTCTime end start
    push ssink $ Timer namespace bucket (time * 1000)

elogMsg :: String -> IO ()
elogMsg s = do
    tid <- myThreadId
    hPutStrLn stderr ("[" ++ show tid ++ "] " ++ s)

logMsg :: String -> IO ()
logMsg s = do
    tid <- myThreadId
    putStrLn ("[" ++ show tid ++ "] " ++ s)
