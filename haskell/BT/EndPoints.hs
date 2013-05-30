{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module BT.EndPoints(register, deposit, getBalance, makePayment, createPayment, mine, sendBTC) where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Network.Wai (Request, requestHeaders)
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (decodeLenient)
import Control.Monad (when)
import Control.Exception (throw)
import BT.Types
import BT.Util
import BT.JSON
import BT.User
import BT.Mining
import BT.ZMQ
import BT.Payment
import BT.Log
import Control.Monad.IO.Class (liftIO)
import System.Timeout (timeout)
import Data.Aeson (decode)
import Network.Bitcoin (HashData, BTC)

register :: Request -> PersistentConns-> IO [(String, String)]
register info conn = do
    user <- random256String
    salt <- random256String
    ok <- set_user_secret conn (BC.pack user) (BC.pack salt)

    case ok of
        True -> return [("username"::String, user), ("secret", salt)]
        _ -> register info conn

getBalance :: Request -> PersistentConns-> IO [(String, String)] 
getBalance info conn = do
    requestal <- getRequestAL info
    verifyAL conn requestal
    let username = BC.pack $ getMaybe (UserException "Missing username field") $ lookup "username" requestal
    bitcoinid_wrap <- get_user_address conn username
    case bitcoinid_wrap of
        Just bitcoinid -> update_stored_balance bitcoinid username conn
        Nothing -> return ()

    resp <- get_user_balance conn username
    return [("balance", show resp)]

createPayment :: Request -> PersistentConns -> IO [(String, String)]
createPayment info conn = do
    al <- getRequestAL info
    verifyAL conn al
    let username = BC.pack$ getMaybe (UserException "Missing username") $ lookup "username" al
    let amount = getMaybe (UserException "Missing amount") $ lookup "amount" al
    paymentid <- random256String
    resp <- set_payment_amount conn (BC.pack paymentid) (read amount :: BTC)
    val <- case resp of
        True -> do
            _ <- set_payment_user conn (BC.pack paymentid) username
            return $ [("payment", paymentid)]
        False -> createPayment info conn
    return val

makePayment :: Request -> PersistentConns -> IO [(String, String)]
makePayment info conn = do
    al <- getRequestAL info
    verifyAL conn al
    let username = BC.pack $ getMaybe (UserException "Missing username") $ lookup "username" al
    let payment = BC.pack $ getMaybe (UserException "Missing payment") $ lookup "payment" al

    user_wrap <- get_payment_user conn payment
    let user = getMaybe (RedisException "Failure getting payment user 225") user_wrap

    logMsg "Before lock"
    lock_user conn username
    logMsg "After lock"

    balance <- get_user_balance conn username

    req_amount <- get_payment_amount conn payment

    logMsg "Got all info"

    when (balance - req_amount < 0) $ do
        unlock_user conn username
        throw (UserException "Insufficient Funds")

    when (username /= user) $ do
        _ <- increment_user_balance conn username (-req_amount)
        _ <- increment_user_balance conn user req_amount
        return ()

    _ <- set_payment_done conn payment

    unlock_user conn username

    return [("code", "hi")]

deposit :: Request -> PersistentConns-> IO [(String, String)]
deposit info conn = do
    al <- getRequestAL info
    verifyAL conn al
    let username = BC.pack $ getMaybe (UserException "Missing username") $ lookup "username" al

    lock_user conn username

    addr <- get_user_address conn username
    resp <- case addr of
        (Just a) -> return $ [("address",BC.unpack a)]
        _ -> do
            resp <- send conn "address"
            _ <- set_user_address conn username resp
            return $ [("address", BC.unpack resp)]

    unlock_user conn username
    return resp

requestUsername :: Request -> B.ByteString
requestUsername req = head . (BC.split ':') . decodeLenient . last. (BC.split ' ') $ authstring
    where authstring = getMaybe (UserException "Missing username header") . lookup hAuthorization . requestHeaders $ req

mine :: Request -> PersistentConns -> IO BL.ByteString
mine info conn = do
    body <- getRequestBody info
    let request = getMaybe (UserException "Bad Format"). decode $ body :: MiningData
    let username = requestUsername info
    case length . getwork $ request of
        0 -> do
            logMsg "getwork length = 0"
            resp <- timeout 1000000 $ sendmine conn "getwork"
            let item = getMaybe (BackendException "Cannot talk to p2pool server") resp
            logMsg "done talking backend"
            let hashData = ((getMaybe (BackendException "Cannot convert result to hash")) . (decode) . BL.fromStrict $ item) :: HashData
            storeMerkleDiff conn hashData
            return $ jsonRPC (rpcid request) hashData
        1 -> do
            let sub_hash = head . getwork $ request

            logMsg $ "recieved hash" ++ (show sub_hash)

            let merkle_root = extractMerkleRecieved (getMaybe (UserException "Invalid result") . (decode) . BLC.pack $ sub_hash)
            resp <- liftIO $ timeout 1000000 $ sendmine conn (BC.pack ("recvwork"++ sub_hash))
            let item = getMaybe (BackendException "Cannot talk to p2pool server") resp
            when (item == "true") $ do
                sdiff <- getMerkleDiff conn merkle_root
                let diff = getMaybe (RedisException "Error retrieving merklediff") sdiff
                payout <- getPayout conn diff
                lock_user conn username
                _ <- increment_user_balance conn username payout
                _ <- increment_unconfirmed_balance conn username payout
                share <- getCurrentMiningShare conn username
                _ <- incrementSharePayout conn share payout
                unlock_user conn username

            logMsg "done submitting work"

            logMsg $ "returning" ++ (show item)
            return $ jsonRPC (rpcid request) item
        _ -> do
            logMsg "getwork length != 0"
            return $ "ERRORRRRRRR"


sendBTC :: Request -> PersistentConns -> IO [(String, String)]
sendBTC info conn = do
    al <- getRequestAL info
    verifyAL conn al
    let username = BC.pack$ getMaybe (UserException "Missing username") $ lookup "username" al
    let rawamount = BC.pack $ getMaybe (UserException "Missing amount") $ lookup "amount" al
    let address = BC.pack $ getMaybe (UserException "Missing address") $ lookup "address" al

    logMsg "Locking"
    lock_user conn username
    logMsg "Locked"

    balance <- get_user_balance conn username
    unconfirmed <- get_user_balance conn username

    logMsg "Got Balances"

    logMsg "forcing amount"
    let !amount = read. BC.unpack $ rawamount :: BTC

    logMsg "Handle cases"
    resp <- case amount > balance-unconfirmed of
        True -> do
            logMsg "Can do it"
            logMsg "Set Balance"
            _ <- increment_user_balance conn username (-amount)
            logMsg "send_money"
            let arg = (BC.intercalate "|" [address, (BC.pack . show) amount])

            resp <- send conn (BC.append "sendto" arg) 

            logMsg "sent"
            return [("id", BC.unpack resp)]
        False -> return [("error", "Insufficient Balance")]

    logMsg "unlocking"

    unlock_user conn username
    logMsg "Unlocked"
    return resp
