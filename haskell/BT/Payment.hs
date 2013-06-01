{-# LANGUAGE OverloadedStrings #-}
module BT.Payment where

import qualified Data.ByteString as B
import BT.Redis
import BT.Types
import Network.Bitcoin (BTC)

getPaymentAmount :: PersistentConns -> B.ByteString -> IO BTC
getPaymentAmount conn paymentid = getbtc conn "p:" paymentid "amount"

getPaymentUser :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getPaymentUser conn paymentid = get conn "p:" paymentid "user"

getPaymentDone :: PersistentConns -> B.ByteString -> IO (Bool)
getPaymentDone conn paymentid = do
    r <- get conn "p:" paymentid "done"
    case r of
        Nothing -> return False
        Just _ -> return True

setPaymentAmount :: PersistentConns -> B.ByteString -> BTC -> IO (Bool)
setPaymentAmount conn paymentid value = setnxbtc conn "p:" paymentid "amount" value

setPaymentUser :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
setPaymentUser conn paymentid value = setnx conn "p:" paymentid "user" value

setPaymentDone :: PersistentConns -> B.ByteString -> IO (Bool)
setPaymentDone conn paymentid = setnx conn "p:" paymentid "done" "1"
