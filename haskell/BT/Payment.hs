{-# LANGUAGE OverloadedStrings #-}
module BT.Payment where

import qualified Data.ByteString as B
import BT.Redis
import BT.Types
import Network.Bitcoin (BTC)

get_payment_amount :: PersistentConns -> B.ByteString -> IO (BTC)
get_payment_amount conn paymentid = getbtc conn "p:" paymentid "amount"

get_payment_user :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
get_payment_user conn paymentid = get conn "p:" paymentid "user"

get_payment_done :: PersistentConns -> B.ByteString -> IO (Bool)
get_payment_done conn paymentid = do
    r <- get conn "p:" paymentid "done"
    case r of
        Nothing -> return False
        Just _ -> return True

set_payment_amount :: PersistentConns -> B.ByteString -> BTC -> IO (Bool)
set_payment_amount conn paymentid value = setnxbtc conn "p:" paymentid "amount" value

set_payment_user :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_payment_user conn paymentid value = setnx conn "p:" paymentid "user" value

set_payment_done :: PersistentConns -> B.ByteString -> IO (Bool)
set_payment_done conn paymentid = setnx conn "p:" paymentid "done" "1"
