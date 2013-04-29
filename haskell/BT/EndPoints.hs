{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module BT.EndPoints(register, deposit, getBalance, makePayment, createPayment, mine, sendBTC) where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Database.Redis(runRedis, setnx, get )
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
import Control.Monad.IO.Class (liftIO)
import System.Timeout (timeout)
import Data.Aeson (decode)
import Network.Bitcoin (HashData, BTC)

register :: Request -> PersistentConns-> IO [(String, String)]
register info conn = do
    user <- random256String
    salt <- random256String
    ok <- runRedis (redis conn) $ do
        setnx (BC.pack $"user_" ++ user) (BC.pack salt)

    case getRight (\s -> RedisException (show s)) ok of
        True -> return [("username"::String, user), ("secret", salt)]
        _ -> register info conn

getBalance :: Request -> PersistentConns-> IO [(String, String)] 
getBalance info conn = do
    requestal <- getRequestAL info
    verifyAL requestal
    let username = BC.pack $ getMaybe (UserException "Missing username field") $ lookup "username" requestal
    bitcoinid_wrap <- get_user_address conn username
    case bitcoinid_wrap of
        Just bitcoinid -> update_stored_balance bitcoinid username conn
        Nothing -> return ()

    resp <- get_user_balance conn username
    return [("balance", BC.unpack resp)]

createPayment :: Request -> PersistentConns -> IO [(String, String)]
createPayment info conn = do
    al <- getRequestAL info
    verifyAL al
    let username = BC.pack$ getMaybe (UserException "Missing username") $ lookup "username" al
    let amount = BC.pack $ getMaybe (UserException "Missing amount") $ lookup "amount" al
    paymentid <- random256String
    resp <- runRedis (redis conn) $ do
        ok <- setnx (B.append "payment_" (BC.pack paymentid)) amount
        case ok of
            (Right True) -> setnx (B.append "payment_user_" (BC.pack paymentid)) username
            a -> return a
    val <- case resp of
        (Right True) -> return $ [("payment",paymentid)]
        (Right False) -> createPayment info conn
        _ -> error []
    return val

makePayment :: Request -> PersistentConns -> IO [(String, String)]
makePayment info conn = do
    al <- getRequestAL info
    verifyAL al
    let username = BC.pack $ getMaybe (UserException "Missing username") $ lookup "username" al
    let payment = BC.pack $ getMaybe (UserException "Missing payment") $ lookup "payment" al

    user_wrap <- get_payment_user conn payment
    let user = getMaybe (RedisException "Failure getting payment user 225") user_wrap

    putStrLn "Before lock"
    lock_user conn username
    when (username /= user) $ lock_user conn user
    putStrLn "After lock"

    str_balance <- get_user_balance conn username
    let balance = (read. BC.unpack $ str_balance) :: BTC

    req_amount_wrap <- get_payment_amount conn payment
    let req_amount = (read .BC.unpack. getMaybe (UserException "No Such Payment") $ req_amount_wrap) :: BTC

    str_user_balance <- get_user_balance conn user
    let user_balance = read . BC.unpack $ str_user_balance :: BTC

    putStrLn "Got all info"

    when (balance - req_amount < 0) $ do
        unlock_user conn username
        unlock_user conn user
        throw (UserException "Insufficient Funds")

    when (username /= user) $ do
        _ <- set_user_balance conn username (BC.pack . show $ (balance-req_amount))
        _ <- set_user_balance conn user (BC.pack . show $ (user_balance+ req_amount))
        return ()

    _ <- set_payment_done conn payment

    unlock_user conn username
    when (username /= user) $ unlock_user conn user

    return [("code", "hi")]

deposit :: Request -> PersistentConns-> IO [(String, String)]
deposit info conn = do
    al <- getRequestAL info
    verifyAL al
    let username = BC.pack $ getMaybe (UserException "Missing username") $ lookup "username" al

    addr <- runRedis (redis conn) $ do
        get $ B.append "address_" username
    case addr of
        (Right (Just a)) -> return $ [("address",BC.unpack a)]
        _ -> do
            resp <- send conn "address"
            ok <- runRedis (redis conn) $ do
                setnx (B.append "address_" username)  resp
            case ok of
                Right True -> return $ [("address", BC.unpack resp)]
                _ -> return []

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
            putStrLn "getwork length = 0"
            resp <- timeout 30000000 $ sendmine conn "getwork"
            let item = getMaybe (BackendException "Cannot talk to p2pool server") resp
            putStrLn "done talking backend"
            let hashData = ((getMaybe (BackendException "Cannot convert result to hash")) . (decode) . BL.fromStrict $ item) :: HashData
            storeMerkleDiff conn hashData
            return $ jsonRPC (rpcid request) hashData
        1 -> do
            let sub_hash = head . getwork $ request
            let merkle_root = extractMerkleRecieved (getMaybe (UserException "Invalid result") . (decode) . BLC.pack $ sub_hash)
            resp <- liftIO $ timeout 30000000 $ sendmine conn (BC.pack ("recvwork"++ sub_hash))
            let item = getMaybe (BackendException "Cannot talk to p2pool server") resp
            when (item == "true") $ do
                sdiff <- getMerkleDiff conn merkle_root
                let diff = getMaybe (RedisException "Error retrieving merklediff") sdiff
                payout <- getPayout conn diff
                lock_user conn username
                strbalance <- get_user_balance conn username
                strubalance <- get_unconfirmed_balance conn username
                let balance = (read . BC.unpack $ strbalance :: BTC) + payout
                let ubalance = (read . BC.unpack $ strubalance :: BTC) + payout
                _ <- set_user_balance conn username (BC.pack . show $ balance)
                _ <- set_unconfirmed_balance conn username (BC.pack . show $ ubalance)
                unlock_user conn username

            putStrLn "done submitting work"
            return $ BL.fromStrict $ item
        _ -> do
            putStrLn "getwork length != 0"
            return $ "ERRORRRRRRR"


sendBTC :: Request -> PersistentConns -> IO [(String, String)]
sendBTC info conn = do
    al <- getRequestAL info
    verifyAL al
    let username = BC.pack$ getMaybe (UserException "Missing username") $ lookup "username" al
    let rawamount = BC.pack $ getMaybe (UserException "Missing amount") $ lookup "amount" al
    let address = BC.pack $ getMaybe (UserException "Missing address") $ lookup "address" al

    putStrLn "Locking"
    lock_user conn username
    putStrLn "Locked"

    raw_balance <- get_user_balance conn username
    raw_unconfirmed_balance <- get_user_balance conn username

    putStrLn "Got Balances"
    let balance = read . BC.unpack $ raw_balance :: BTC
    let unconfirmed = read . BC.unpack $ raw_unconfirmed_balance :: BTC

    putStrLn "forcing amount"
    let !amount = read. BC.unpack $ rawamount :: BTC

    putStrLn "Handle cases"
    resp <- case amount > balance-unconfirmed of
        True -> do
            putStrLn "Can do it"
            putStrLn "Set Balance"
            _ <- set_user_balance conn username (BC.pack . show $ balance-amount)
            putStrLn "send_money"
            let arg = (BC.intercalate "|" [address, (BC.pack . show) amount])

            resp <- send conn (BC.append "sendto" arg) 

            putStrLn "sent"
            return [("id", BC.unpack resp)]
        False -> return [("error", "Insufficient Balance")]

    putStrLn "unlocking"
    unlock_user conn username
    putStrLn "Unlocked"
    return resp
