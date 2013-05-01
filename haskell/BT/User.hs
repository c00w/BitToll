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

    stored_recv <- liftM (read . BC.unpack) $ get_user_address_recieved conn userid :: IO BTC
    stored_balance <- liftM (read . BC.unpack) $ get_user_balance conn userid :: IO BTC

    _ <- putStrLn. show$ actual_recv

    when (stored_recv /= actual_recv) $ do
        liftIO $ BC.putStrLn "Updating balance"
        let diff = actual_recv - stored_recv
        _ <- set_user_address_recieved conn userid $ BC.pack . show $ actual_recv
        _ <- set_user_balance conn userid $ BC.pack . show $ (stored_balance + diff)
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

get_user_balance :: PersistentConns -> B.ByteString -> IO (B.ByteString)
get_user_balance conn user = (liftM zeroMaybe) $ BR.get conn "balance" user

get_unconfirmed_balance :: PersistentConns -> B.ByteString -> IO (B.ByteString)
get_unconfirmed_balance conn user = (liftM zeroMaybe) $ BR.get conn "balance_unconfirmed" user

get_paid_balance :: PersistentConns -> B.ByteString -> IO (B.ByteString)
get_paid_balance conn user = (liftM zeroMaybe) $ BR.get conn "balance_paid" user

get_user_address :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
get_user_address conn user = BR.get conn "address" user

get_user_address_recieved :: PersistentConns -> B.ByteString -> IO (B.ByteString)
get_user_address_recieved conn user = (liftM zeroMaybe) $ BR.get conn "address_recieved" user


set_user_balance :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_user_balance conn user key = BR.set conn "balance" user key

set_unconfirmed_balance :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_unconfirmed_balance conn user key = BR.set conn "balance_unconfirmed" user key

set_paid_balance :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_paid_balance conn user key = BR.set conn "balance_paid" user key

set_user_address :: PersistentConns -> B.ByteString -> B.ByteString-> IO (Bool)
set_user_address conn user value = BR.set conn "address" user value

set_user_address_recieved :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_user_address_recieved conn user value = BR.set conn "address_recieved" user value

