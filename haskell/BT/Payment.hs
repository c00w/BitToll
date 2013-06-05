{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}

module BT.Payment where

import Control.Monad.Loc

import qualified Data.ByteString as B
import BT.Redis
import BT.Types
import Network.Bitcoin (BTC)

getPaymentAmount :: PersistentConns -> B.ByteString -> BTIO BTC
getPaymentAmount conn paymentid = getbtc conn "p:" paymentid "amount"

getPaymentUser :: PersistentConns -> B.ByteString -> BTIO (Maybe B.ByteString)
getPaymentUser conn paymentid = get conn "p:" paymentid "user"

getPaymentDone :: PersistentConns -> B.ByteString -> BTIO Bool
getPaymentDone conn paymentid = do
    r <- get conn "p:" paymentid "done"
    case r of
        Nothing -> return False
        Just _ -> return True

setPaymentAmount :: PersistentConns -> B.ByteString -> BTC -> BTIO Bool
setPaymentAmount conn paymentid = setnxbtc conn "p:" paymentid "amount"

setPaymentUser :: PersistentConns -> B.ByteString -> B.ByteString -> BTIO Bool
setPaymentUser conn paymentid = setnx conn "p:" paymentid "user"

setPaymentDone :: PersistentConns -> B.ByteString -> BTIO Bool
setPaymentDone conn paymentid = setnx conn "p:" paymentid "done" "1"
