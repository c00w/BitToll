{-# LANGUAGE OverloadedStrings #-}
module BT.Payment where

import qualified Data.ByteString as B
import BT.Redis
import BT.Types

get_payment_amount :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
get_payment_amount conn paymentid = get conn "payment_" paymentid

get_payment_user :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
get_payment_user conn paymentid = get conn "payment_user_" paymentid

set_payment_amount :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_payment_amount conn paymentid value = setnx conn "payment_" paymentid value

set_payment_user :: PersistentConns -> B.ByteString -> B.ByteString -> IO (Bool)
set_payment_user conn paymentid value = setnx conn "payment_user_" paymentid value

set_payment_done :: PersistentConns -> B.ByteString -> IO (Bool)
set_payment_done conn paymentid = setnx conn "payment_user_" paymentid "1"
