{-# LANGUAGE OverloadedStrings #-}




import BT.Global
import BT.Types
import BT.Mining
import BT.ZMQ
import BT.Util
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

getAddr :: PersistentConns -> IO (Maybe B.ByteString)
getAddr conn = do
    mine_addr <- getMiningAddress conn
    case mine_addr of
        Just _  -> return mine_addr
        Nothing -> do
            raw_addr <- send conn "address"
            _ <- setMiningAddress conn raw_addr
            resp <- getMiningAddress conn
            case resp of
                Just _ -> return resp
                Nothing -> getAddr conn

main :: IO ()
main = do
    conn <- makeCons
    addr <- catch (getAddr conn) elogCatch
    case addr of
        Just a -> BC.putStrLn a
        _ -> main
