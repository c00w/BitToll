{-# LANGUAGE OverloadedStrings #-}
module BT.Log where
import Control.Concurrent (myThreadId)
import System.IO (hPutStrLn, stderr)

elogMsg :: String -> IO ()
elogMsg s = do
    tid <- myThreadId
    hPutStrLn stderr ("[" ++ show tid ++ "] " ++ s)

logMsg :: String -> IO ()
logMsg s = do
    tid <- myThreadId
    putStrLn ("[" ++ show tid ++ "] " ++ s)
