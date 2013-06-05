{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}

module BT.EndPoints(register, deposit, getBalance, makePayment, createPayment, mine, sendBTC) where

import Control.Monad.Loc

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request, requestHeaders)
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (decodeLenient)
import Control.Monad (when, liftM)
import Control.Exception (throw)
import BT.Types
import BT.Util
import BT.JSON
import BT.User
import BT.Mining
import BT.ZMQ
import BT.Payment
import Data.Aeson (decode)
import Network.Bitcoin (HashData, BTC)
import Crypto.Hash.MD5 (hash)
import Data.Hex (hex)
import Data.Char (toLower)
import qualified Data.Map

usernameALShort :: PersistentConns -> Request -> BTIO (Data.Map.Map String String, B.ByteString)
usernameALShort conn info = do
    al <- getRequestMap info
    verifyMap conn al
    let username = BC.pack . getMaybe (UserException "Missing username") $ Data.Map.lookup "username" al
    return (al, username)

register :: Request -> PersistentConns-> BTIO [(String, String)]
register info conn = do
    user <- random256String
    salt <- random256String
    ok <- setUserSecret conn (BC.pack user) (BC.pack salt)

    if ok then return [("username"::String, user), ("secret", salt)]
      else register info conn

getBalance :: Request -> PersistentConns-> BTIO [(String, String)] 
getBalance info conn = do
    (_, username) <- usernameALShort conn info
    bitcoinid_wrap <- getUserAddress conn username
    case bitcoinid_wrap of
        Just bitcoinid -> updateStoredBalance bitcoinid username conn
        Nothing -> return ()

    resp <- getUserBalance conn username
    return [("balance", show resp)]

createPayment :: Request -> PersistentConns -> BTIO [(String, String)]
createPayment info conn = do
    al <- getRequestMap info
    verifyMap conn al
    let username = BC.pack$ getMaybe (UserException "Missing username") $ Data.Map.lookup "username" al
    let amount = getMaybe (UserException "Missing amount") $ Data.Map.lookup "amount" al
    paymentid <- random256String
    resp <- setPaymentAmount conn (BC.pack paymentid) (read amount :: BTC)
    if resp then do
            _ <- setPaymentUser conn (BC.pack paymentid) username
            return [("payment", paymentid)]
      else createPayment info conn

makePayment :: Request -> PersistentConns -> BTIO [(String, String)]
makePayment info conn = do
    (al, username) <- usernameALShort conn info
    let payment = BC.pack $ getMaybe (UserException "Missing payment") $ Data.Map.lookup "payment" al

    user_wrap <- getPaymentUser conn payment
    let user = getMaybe (RedisException "Failure getting payment user 225") user_wrap

    logMsg "Before lock"
    lockUser conn username
    logMsg "After lock"

    balance <- getUserBalance conn username

    req_amount <- getPaymentAmount conn payment

    logMsg "Got all info"

    when (balance - req_amount < 0) $ do
        unlockUser conn username
        throw (UserException "Insufficient Funds")

    when (username /= user) $ do
        _ <- incrementUserBalance conn username (-req_amount)
        _ <- incrementUserBalance conn user req_amount
        return ()

    _ <- setPaymentDone conn payment

    unlockUser conn username

    secret <- liftM ( getMaybe (RedisException "unknown user for secret")) $ getUserSecret conn user

    let code = BC.map toLower.hex.hash $ BC.append payment secret

    return [("code", BC.unpack code)]

deposit :: Request -> PersistentConns-> BTIO [(String, String)]
deposit info conn = do
    (_, username) <- usernameALShort conn info

    lockUser conn username

    addr <- getUserAddress conn username
    resp <- case addr of
        (Just a) -> return [("address",BC.unpack a)]
        _ -> do
            resp <- send conn "address"
            _ <- setUserAddress conn username resp
            return [("address", BC.unpack resp)]

    unlockUser conn username
    return resp

requestUsername :: Request -> B.ByteString
requestUsername req = head . BC.split ':' . decodeLenient . last. BC.split ' ' $ authstring
    where authstring = getMaybe (UserException "Missing username header") . lookup hAuthorization . requestHeaders $ req

mine :: Request -> PersistentConns -> BTIO BL.ByteString
mine info conn = do
    body <- getRequestBody info
    let request = getMaybe (UserException "Bad Format"). decode $ body :: MiningData
    let username = requestUsername info
    case length . getwork $ request of
        0 -> do
            logMsg "getwork length = 0"
            item <- sendmine conn "getwork"
            logMsg "done talking backend"
            let hashData = getMaybe (BackendException "Cannot convert result to hash") . decode . BL.fromStrict $ item :: HashData
            storeMerkleDiff conn hashData
            return $ jsonRPC (rpcid request) hashData
        1 -> do
            logMsg "getwork length = 1"
            let sub_hash = head . getwork $ request

            logMsg $ "recieved hash" ++ show sub_hash

            let merkle_root = extractMerkleRecieved sub_hash
            item <- sendmine conn (BC.pack ("recvwork"++ sub_hash))
            when (item == "true") $ do
                sdiff <- getMerkleDiff conn merkle_root
                let diff = getMaybe (RedisException "Error retrieving merklediff") sdiff
                payout <- getPayout conn diff
                lockUser conn username
                _ <- incrementUserBalance conn username payout
                _ <- incrementUnconfirmedBalance conn username payout
                share <- getCurrentMiningShare conn username
                _ <- incrementSharePayout conn share payout
                unlockUser conn username

            logMsg "done submitting work"

            logMsg $ "returning" ++ show item
            return $ jsonRPC (rpcid request) (item == "true")
        _ -> do
            logMsg "getwork length != 0"
            return "ERRORRRRRRR"


sendBTC :: Request -> PersistentConns -> BTIO [(String, String)]
sendBTC info conn = do
    (al, username) <- usernameALShort conn info
    let rawamount = BC.pack $ getMaybe (UserException "Missing amount") $ Data.Map.lookup "amount" al
    let address = BC.pack $ getMaybe (UserException "Missing address") $ Data.Map.lookup "address" al

    logMsg "Locking"
    lockUser conn username
    logMsg "Locked"

    balance <- getUserBalance conn username
    unconfirmed <- getUserBalance conn username

    logMsg "Got Balances"

    logMsg "forcing amount"
    let !amount = read. BC.unpack $ rawamount :: BTC

    logMsg "Handle cases"
    resp <- if amount > balance-unconfirmed then 
        (do
            logMsg "Can do it"
            logMsg "Set Balance"
            _ <- incrementUserBalance conn username (-amount)
            logMsg "send_money"
            let arg = BC.intercalate "|" [address, (BC.pack . show) amount]

            resp <- send conn (BC.append "sendto" arg) 

            logMsg "sent"
            return [("id", BC.unpack resp)])
        else return [("error", "Insufficient Balance")]

    logMsg "unlocking"

    unlockUser conn username
    logMsg "Unlocked"
    return resp
