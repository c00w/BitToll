{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}

import Control.Monad.Loc

import BT.Global
import BT.Types
import BT.Mining
import BT.ZMQ
import BT.Util
import System.Timeout (timeout)
import Control.Monad.Exception (runEMT, catchWithSrcLoc)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

getAddr :: PersistentConns -> BTIO (Maybe B.ByteString)
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
    addr <- timeout 10000 $ runEMT $ catchWithSrcLoc (getAddr conn) elogCatch
    case addr of
        Just (Just a) -> BC.putStrLn a
        _ -> main
