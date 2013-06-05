{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}

module BT.User where

import Control.Monad.Loc

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad (liftM, when, unless)
import Network.Bitcoin (BTC)
import qualified BT.Redis as BR
import BT.Types
import BT.ZMQ
import BT.Util (logMsg)

updateStoredBalance :: B.ByteString -> B.ByteString -> PersistentConns -> BTIO ()
updateStoredBalance bitcoin_addr userid conn = do
    lockUser conn userid

    logMsg "Updating balance"
    actual_recv <- liftM (read . BC.unpack) $ send conn $ B.append "recieved" bitcoin_addr :: BTIO BTC

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

lockUser :: PersistentConns -> B.ByteString -> BTIO ()
lockUser conn user = do
    ok <- BR.rsetnx conn "ul:" user "h"
    if ok then do
        _ <- BR.expire conn "ul:" user 30
        logMsg $ "lock add " ++ show user
       else lockUser conn user

unlockUser :: PersistentConns -> B.ByteString -> BTIO ()
unlockUser conn user = do
    logMsg $ "lock del " ++ show user
    ok <- BR.rdel conn "ul:" user
    unless ok (logMsg "Odd unlock failed")
    return ()

getUserBalance :: PersistentConns -> B.ByteString -> BTIO BTC
getUserBalance conn user = BR.getbtc conn "u:" user "balance"

getUnconfirmedBalance :: PersistentConns -> B.ByteString -> BTIO BTC
getUnconfirmedBalance conn user = BR.getbtc conn "u:" user "balance_unconfirmed"

getPaidBalance :: PersistentConns -> B.ByteString -> BTIO BTC
getPaidBalance conn user = BR.getbtc conn "u:" user "balance_paid"

getUserAddress :: PersistentConns -> B.ByteString -> BTIO (Maybe B.ByteString)
getUserAddress conn user = BR.get conn "u:" user "address"

getUserAddressRecieved :: PersistentConns -> B.ByteString -> BTIO BTC
getUserAddressRecieved conn user = BR.getbtc conn "u:" user "address_recieved"

getUserSecret :: PersistentConns -> B.ByteString -> BTIO (Maybe B.ByteString)
getUserSecret conn user = BR.get conn "u:" user "secret"

incrementUserBalance :: PersistentConns -> B.ByteString -> BTC -> BTIO BTC
incrementUserBalance conn user = BR.incrementbtc conn "u:" user "balance"

incrementUnconfirmedBalance :: PersistentConns -> B.ByteString -> BTC -> BTIO BTC
incrementUnconfirmedBalance conn user = BR.incrementbtc conn "u:" user "balance_unconfirmed"


setUserAddress :: PersistentConns -> B.ByteString -> B.ByteString-> BTIO Bool
setUserAddress conn user = BR.set conn "u:" user "address"

incrementUserAddressRecieved :: PersistentConns -> B.ByteString -> BTC -> BTIO BTC
incrementUserAddressRecieved conn user = BR.incrementbtc conn "u:" user  "address_recieved"

setUserSecret :: PersistentConns -> B.ByteString -> B.ByteString -> BTIO Bool
setUserSecret conn user = BR.setnx conn "u:" user "secret"
