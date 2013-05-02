{-# LANGUAGE OverloadedStrings #-}
import BT.Redis (setnx, get)
import BT.Global
import BT.Types
import BT.ZMQ

import qualified Data.ByteString as B

get_mining_address :: PersistentConns -> IO (Maybe B.ByteString)
get_mining_address conn = get conn "g:" "global" "mining_address"

set_mining_address :: PersistentConns -> B.ByteString -> IO (Bool)
set_mining_address conn value = setnx conn "g:" "global" "mining_address" value


main :: IO ()
main = do
    conn <- makeCons
    mine_addr <- get_mining_address conn
    addr <- case mine_addr of
        Just a  -> return a
        Nothing -> do
            raw_addr <- send conn "address"
            set_mining_address conn raw_addr
            resp <- get_mining_address conn 
            case resp of
                Just a -> return a
    B.putStrLn addr
