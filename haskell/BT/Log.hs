{-# LANGUAGE OverloadedStrings #-}

module BT.Log where

import Control.Concurrent (myThreadId)
import System.IO (hPutStrLn, stderr)
import Network.Metric.Sink.Statsd (open, AnySink, push)
import Network.Metric (Metric(Counter))
import qualified Data.ByteString as B

loggingSink :: IO AnySink
loggingSink = open "localhost" "localhost" 8125

logCount :: B.ByteString -> B.ByteString -> Integer -> IO ()
logCount namespace bucket count = do
    ssink <- loggingSink
    push ssink $ Counter namespace bucket count

elogMsg :: String -> IO ()
elogMsg s = do
    tid <- myThreadId
    hPutStrLn stderr ("[" ++ show tid ++ "] " ++ s)

logMsg :: String -> IO ()
logMsg s = do
    tid <- myThreadId
    putStrLn ("[" ++ show tid ++ "] " ++ s)
