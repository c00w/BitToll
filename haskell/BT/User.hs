{-# LANGUAGE OverloadedStrings #-}


module BT.User where



import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad (liftM, when, unless)
import Network.Bitcoin (BTC)
import qualified BT.Redis as BR
import BT.Types
import BT.ZMQ
import BT.Util (logMsg)

updateStoredBalance :: B.ByteString -> B.ByteString -> PersistentConns -> IO ()
updateStoredBalance bitcoin_addr userid conn = do
    lockUser conn userid

    logMsg "Updating balance"
    actual_recv <- liftM (read . BC.unpack) $ send conn $ B.append "recieved" bitcoin_addr :: IO BTC

    stored_recv <- getUserAddressRecieved conn userid

    _ <- logMsg . show $ actual_recv

    when (stored_recv /= actual_recv) $ do
        logMsg "Updating balance"
        let diff = actual_recv - stored_recv
        _ <- incrementUserAddressRecieved conn userid diff
        _ <- incrementUserBalance conn userid diff
        return ()

    unlockUser conn userid

    return ()

lockUser :: PersistentConns -> B.ByteString -> IO ()
lockUser conn user = do
    ok <- BR.rsetnx conn "ul:" user "h"
    if ok then do
        _ <- BR.expire conn "ul:" user 30
        logMsg $ "lock add " ++ show user
       else lockUser conn user

unlockUser :: PersistentConns -> B.ByteString -> IO ()
unlockUser conn user = do
    logMsg $ "lock del " ++ show user
    ok <- BR.rdel conn "ul:" user
    unless ok (logMsg "Odd unlock failed")
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

getAliasID :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getAliasID conn alias = BR.get conn "ua:" alias "id"

getAliasSalt :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getAliasSalt conn alias = BR.get conn "ua:" alias "salt"

getAliasPassword :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getAliasPassword conn alias = BR.get conn "ua:" alias "pass"

incrementUserBalance :: PersistentConns -> B.ByteString -> BTC -> IO BTC
incrementUserBalance conn user = BR.incrementbtc conn "u:" user "balance"

incrementUnconfirmedBalance :: PersistentConns -> B.ByteString -> BTC -> IO BTC
incrementUnconfirmedBalance conn user = BR.incrementbtc conn "u:" user "balance_unconfirmed"

setAliasID :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setAliasID conn alias = BR.set conn "ua:" alias "id"

setAliasSalt :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setAliasSalt conn alias = BR.set conn "ua:" alias "salt"

setAliasPassword :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setAliasPassword conn alias = BR.set conn "ua:" alias "pass"

setUserAddress :: PersistentConns -> B.ByteString -> B.ByteString-> IO Bool
setUserAddress conn user = BR.set conn "u:" user "address"

incrementUserAddressRecieved :: PersistentConns -> B.ByteString -> BTC -> IO BTC
incrementUserAddressRecieved conn user = BR.incrementbtc conn "u:" user  "address_recieved"

setUserSecret :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setUserSecret conn user = BR.setnx conn "u:" user "secret"
