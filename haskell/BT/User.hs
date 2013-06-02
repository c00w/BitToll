{-# LANGUAGE OverloadedStrings #-}
module BT.User where
import Database.Redis (runRedis, setnx, del, expire, )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM, when, unless)
import Network.Bitcoin (BTC)
import qualified BT.Redis as BR
import BT.Types
import BT.Util
import BT.ZMQ
import BT.Log

updateStoredBalance :: B.ByteString -> B.ByteString -> PersistentConns -> IO ()
updateStoredBalance bitcoin_addr userid conn = do
    lockUser conn userid

    liftIO $ logMsg "Updating balance"
    actual_recv <- liftM (read . BC.unpack) $ send conn $ B.append "recieved" bitcoin_addr :: IO BTC

    stored_recv <- getUserAddressRecieved conn userid

    _ <- logMsg. show$ actual_recv

    when (stored_recv /= actual_recv) $ do
        liftIO $ logMsg "Updating balance"
        let diff = actual_recv - stored_recv
        _ <- incrementUserAddressRecieved conn userid diff
        _ <- incrementUserBalance conn userid diff
        return ()

    unlockUser conn userid

    return ()

lockUser :: PersistentConns -> B.ByteString -> IO ()
lockUser conn user = do
    ok <- liftIO $ runRedis (redis conn) $
        setnx ( BC.append "user_lock_" user ) "h"
    if getRightRedis ok then do
            _ <- liftIO $ runRedis (redis conn) $
                expire ( BC.append "user_lock_" user ) 1
            logMsg $ "lock add " ++ show user
       else lockUser conn user

unlockUser :: PersistentConns -> B.ByteString -> IO ()
unlockUser conn user = do
    logMsg $ "lock del " ++ show user
    ok <- liftIO $ runRedis (redis conn) $
        del [ BC.append "user_lock_" user ]
    unless (getRightRedis ok == 1) (logMsg "Odd unlock failed")
    return ()

getUserBalance :: PersistentConns -> B.ByteString -> IO BTC
getUserBalance conn user = BR.getbtc conn "u:" user "balance"

getUnconfirmedBalance :: PersistentConns -> B.ByteString -> IO BTC
getUnconfirmedBalance conn user = BR.getbtc conn "u:" user "balance_unconfirmed"

getPaidBalance :: PersistentConns -> B.ByteString -> IO BTC
getPaidBalance conn user = BR.getbtc conn "u:" user "balance_paid"

getUserAddress :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getUserAddress conn user = BR.get conn "u:" user "address"

getUserAddressRecieved :: PersistentConns -> B.ByteString -> IO BTC
getUserAddressRecieved conn user = BR.getbtc conn "u:" user "address_recieved"

getUserSecret :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getUserSecret conn user = BR.get conn "u:" user "secret"

incrementUserBalance :: PersistentConns -> B.ByteString -> BTC -> IO BTC
incrementUserBalance conn user = BR.incrementbtc conn "u:" user "balance"

incrementUnconfirmedBalance :: PersistentConns -> B.ByteString -> BTC -> IO BTC
incrementUnconfirmedBalance conn user = BR.incrementbtc conn "u:" user "balance_unconfirmed"


setUserAddress :: PersistentConns -> B.ByteString -> B.ByteString-> IO Bool
setUserAddress conn user = BR.set conn "u:" user "address"

incrementUserAddressRecieved :: PersistentConns -> B.ByteString -> BTC -> IO BTC
incrementUserAddressRecieved conn user = BR.incrementbtc conn "u:" user  "address_recieved"

setUserSecret :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setUserSecret conn user = BR.setnx conn "u:" user "secret"
