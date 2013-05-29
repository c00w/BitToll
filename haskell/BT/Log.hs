{-# LANGUAGE OverloadedStrings #-}
module BT.Log where
import Control.Concurrent (myThreadId)

logMsg :: String -> IO ()
logMsg s = do
    tid <- myThreadId
    putStrLn ("[" ++ (show tid) ++ "] " ++ s)
