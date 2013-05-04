{-# LANGUAGE OverloadedStrings #-}
module BT.User where
import Database.Redis (runRedis, setnx, del, expire, )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM, when)
import Network.Bitcoin (BTC)
import qualified BT.Redis as BR
import BT.Types
import BT.Util
import BT.ZMQ

update_stored_balance :: B.ByteString -> B.ByteString -> PersistentConns -> IO ()
update_stored_balance bitcoinid userid conn = do
    lock_user conn userid

    liftIO $ putStrLn "Updating balance"
    actual_recv <- liftM (read . BC.unpack) $ send conn $ B.append "recieved" bitcoinid :: IO BTC

    stored_recv <- get_user_address_recieved conn userid

    _ <- putStrLn. show$ actual_recv

    when (stored_recv /= actual_recv) $ do
        liftIO $ BC.putStrLn "Updating balance"
        let diff = actual_recv - stored_recv
        _ <- increment_user_address_recieved conn userid diff
        _ <- increment_user_balance conn userid diff
        return ()

    unlock_user conn userid

    return ()

lock_user :: PersistentConns -> B.ByteString -> IO ()
lock_user conn user = do
    ok <- liftIO $ runRedis (redis conn) $ do
        setnx ( BC.append "user_lock_" user ) "h"
    case getRightRedis ok of
        True -> do
            _ <- liftIO $ runRedis (redis conn) $ do
                expire ( BC.append "user_lock_" user ) 1
            putStrLn $ "lock add " ++ show user
            return ()
        _ -> lock_user conn user

unlock_user :: PersistentConns -> B.ByteString -> IO ()
unlock_user conn user = do
    putStrLn $ "lock del " ++ show user
    ok <- liftIO $ runRedis (redis conn) $ do
        del [ BC.append "user_lock_" user ]
    case getRightRedis ok of
        _ -> return ()

get_user_balance :: PersistentConns -> B.ByteString -> IO (BTC)
get_user_balance conn user = BR.getbtc conn "u:" user "balance"

get_unconfirmed_balance :: PersistentConns -> B.ByteString -> IO (BTC)
get_unconfirmed_balance conn user = BR.getbtc conn "u:" user "balance_unconfirmed"

get_paid_balance :: PersistentConns -> B.ByteString -> IO (BTC)
get_paid_balance conn user = BR.getbtc conn "u:" user "balance_paid"

get_user_address :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
get_user_address conn user = BR.get conn "u:" user "address"

get_user_address_recieved :: PersistentConns -> B.ByteString -> IO (BTC)
get_user_address_recieved conn user = BR.getbtc conn "u:" user "address_recieved"

get_user_secret :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
get_user_secret conn user = BR.get conn "u:" user "secret"

set_user_balance :: PersistentConns -> B.ByteString -> BTC -> IO (Bool)
set_user_balance conn user key = BR.setbtc conn "u:" user "balance" key

increment_user_balance :: PersistentConns -> B.ByteString -> BTC -> IO (BTC)
increment_user_balance conn user key = BR.incrementbtc conn "u:" user "balance" key

set_unconfirmed_balance :: PersistentConns -> B.ByteString -> BTC -> IO (Bool)
set_unconfirmed_balance conn user key = BR.setbtc conn "u:" user "balance_unconfirmed" key

increment_unconfirmed_balance :: PersistentConns -> B.ByteString -> BTC -> IO (BTC)
increment_unconfirmed_balance conn user key = BR.incrementbtc conn "u:" user "balance_unconfirmed" key


set_paid_balance :: PersistentConns -> B.ByteString -> BTC -> IO (Bool)
set_paid_balance conn user key = BR.setbtc conn "u:" user "balance_paid" key

set_user_address :: PersistentConns -> B.ByteString -> B.ByteString-> IO (Bool)
set_user_address conn user value = BR.set conn "u:" user "address" value

set_user_address_recieved :: PersistentConns -> B.ByteString -> BTC -> IO (Bool)
set_user_address_recieved conn user value = BR.setbtc conn "u:" user  "address_recieved" value

increment_user_address_recieved :: PersistentConns -> B.ByteString -> BTC -> IO (BTC)
increment_user_address_recieved conn user value = BR.incrementbtc conn "u:" user  "address_recieved" value

set_user_secret :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_user_secret conn user value = BR.setnx conn "u:" user "secret" value
