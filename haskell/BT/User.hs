{-# LANGUAGE OverloadedStrings #-}
module BT.User where
import Database.Redis (runRedis, watch, setnx, del, TxResult( TxSuccess), multiExec, get, set, expire)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throw)
import qualified BT.Redis as BR
import BT.Types
import BT.Util
import BT.ZMQ

update_stored_balance :: B.ByteString -> B.ByteString -> PersistentConns -> IO ()
update_stored_balance bitcoinid userid conn = do
    runRedis (redis conn) $ do
        w_addr <- watch $ [ B.append "address_recieved_" bitcoinid ]
        checkWatch w_addr

        liftIO $ putStrLn "Updating balance"
        actual_recv <- liftIO $ send conn $ B.append "recieved" bitcoinid

        stored_recvraw <- get $ B.append "address_recieved_" bitcoinid
        let stored_recv = getRightRedis stored_recvraw
        bw <- watch $ [ B.append "balance_" userid]
        checkWatch bw
        stored_balanceraw <- get $ B.append "balance_" userid
        let stored_balance = getRightRedis stored_balanceraw
        _ <- liftIO $ BC.putStrLn  actual_recv
        case (stored_recv, stored_balance) of
            (Just stored, Just st_balance) -> do
                if stored == actual_recv
                then return ()
                else do
                    liftIO $ BC.putStrLn "Main path"
                    let diff = satoshi_sub actual_recv stored
                    cm <- multiExec $ do
                        _ <- set (B.append "address_recieved_" bitcoinid) stored
                        set (B.append "balance_" userid) (satoshi_add st_balance diff)
                    case cm of
                        TxSuccess _ -> return ()
                        _ -> liftIO $ update_stored_balance bitcoinid userid conn

            (Nothing, Just balance) -> do
                liftIO $ BC.putStrLn "stored balance, no stored recv"
                cm <- multiExec $ do
                    _ <- set (B.append "address_recieved_" bitcoinid) actual_recv
                    set (B.append "balance_" userid) (satoshi_add actual_recv balance)
                case cm of
                        TxSuccess _ -> return ()
                        _ -> liftIO $ update_stored_balance bitcoinid userid conn

            (Nothing, Nothing) -> do
                liftIO $ BC.putStrLn "No stored balance, no stored recieved"
                cm <- multiExec $ do
                    _ <- set (B.append "address_recieved_" bitcoinid) actual_recv
                    set (B.append "balance_" userid) actual_recv
                case cm of
                        TxSuccess _ -> return ()
                        _ -> liftIO $ update_stored_balance bitcoinid userid conn
            (Just _, Nothing) -> throw $ RedisException "Stored recv but no stored balance..."
    return ()


lock_user :: PersistentConns -> B.ByteString -> IO ()
lock_user conn user = do
    ok <- liftIO $ runRedis (redis conn) $ do
        setnx ( BC.append "user_lock_" user ) "h"
    case getRightRedis ok of
        True -> do
            liftIO $ runRedis (redis conn) $ do
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
get_user_balance conn user = do
    bal <- BR.get conn "balance_" user
    case bal of
        Just b -> return b
        Nothing -> return "0"

get_unconfirmed_balance :: PersistentConns -> B.ByteString -> IO (B.ByteString)
get_unconfirmed_balance conn user = do
    bal <- BR.get conn "balance_unconfirmed_" user
    case bal of
        Just b -> return b
        bothing -> return "0"

get_paid_balance :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
get_paid_balance conn user = BR.get conn "balance_paid_" user

get_user_address :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
get_user_address conn user = BR.get conn "address_" user

set_user_balance :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_user_balance conn user key = BR.set conn "balance_" user key

set_unconfirmed_balance :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_unconfirmed_balance conn user key = BR.set conn "balance_unconfirmed_" user key

set_paid_balance :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_paid_balance conn user key = BR.set conn "balance_paid_" user key


