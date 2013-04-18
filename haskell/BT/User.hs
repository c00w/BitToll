{-# LANGUAGE OverloadedStrings #-}
module BT.User (update_stored_balance) where
import Database.Redis (runRedis, watch, get, set, setnx, del, TxResult( TxSuccess), multiExec)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified System.ZMQ3 as ZMQ
import Control.Monad.IO.Class (liftIO)
import System.Timeout (timeout)
import Data.Pool (withResource)
import Control.Exception (throw)
import BT.Types
import BT.Util

update_stored_balance :: B.ByteString -> B.ByteString -> PersistentConns -> IO ()
update_stored_balance bitcoinid userid conn = do
    runRedis (redis conn) $ do
        w_addr <- watch $ [ B.append "address_recieved_" bitcoinid ]
        checkWatch w_addr
        liftIO $ putStrLn "Updating balance"
        ractual_recv <- liftIO $ timeout 3000000 $ withResource (pool conn) (\s -> do
            liftIO $ ZMQ.send s [] $ B.append "recieved" bitcoinid
            resp <-liftIO $ ZMQ.receive s
            return resp)
        let actual_recv = getMaybe (BackendException "Cannot talk to bc server") ractual_recv
        stored_recvraw <- get $ B.append "address_recieved_" bitcoinid
        let stored_recv = getRight (\s -> RedisException (show s)) stored_recvraw
        bw <- watch $ [ B.append "balance_" userid]
        checkWatch bw
        stored_balanceraw <- get $ B.append "balance_" userid
        let stored_balance = getRight (\s -> RedisException (show s)) stored_balanceraw
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
            _ -> throw $ RedisException "Stored balance but no address..."
    return ()


lock_user :: PersistentConns -> B.ByteString -> IO ()
lock_user conn user = do
    ok <- liftIO $ runRedis (redis conn) $ do
        setnx ( BC.append "user_lock_" user ) "h"
    case getRight (\s -> RedisException (show s)) ok of
        True -> return ()
        _ -> lock_user conn user

unlock_user :: PersistentConns -> B.ByteString -> IO ()
unlock_user conn user = do
    ok <- liftIO $ runRedis (redis conn) $ do
        del [ BC.pack $ "user_lock_" ++ show user ]
    case getRight (\s -> RedisException (show s)) ok of
        _ -> return ()

