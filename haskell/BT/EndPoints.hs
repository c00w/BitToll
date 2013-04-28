{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module BT.EndPoints(register, deposit, getBalance, makePayment, createPayment, mine, sendBTC) where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Database.Redis(runRedis, setnx, get, set, watch, multiExec, TxResult(TxSuccess))
import Network.Wai (Request, requestHeaders)
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (decodeLenient)
import BT.Types
import BT.Util
import BT.JSON
import BT.User
import BT.Mining
import BT.ZMQ
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
    case resp of
        Just a -> return [("balance", BC.unpack a)]
        Nothing -> return [("balance", "0")]

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

getRedisResult :: Either t (Maybe a) -> String -> a
getRedisResult a m = case a of
    (Right (Just b)) -> b
    _ -> error m

makePayment :: Request -> PersistentConns -> IO [(String, String)]
makePayment info conn = do
    al <- getRequestAL info
    verifyAL al
    let username = BC.pack $ getMaybe (UserException "Missing username") $ lookup "username" al
    let payment = BC.pack $ getMaybe (UserException "Missing payment") $ lookup "payment" al

    lock_user conn username

    balance_wrap <- get_user_balance conn username
    let balance = getMaybe (UserException "No Balance") balance_wrap

    resp <- runRedis (redis conn) $ do
        req_amount_wrap <- get $ B.append "payment_" payment
        let req_amount = getRedisResult req_amount_wrap "Failure Getting Request 217"

        let diff = satoshi_sub balance req_amount
        case satoshi_big diff "0" of
            False -> error "FU"
            _ -> return ()

        user_wrap <- get $ B.append "payment_user_" payment
        let user = getRedisResult user_wrap "Failure getting payment user 225"
        sw <- watch $ [B.append "balance_" user]
        checkWatch sw
        user_balance_wrap <- get $ B.append "balance_" user
        let user_balance = getRedisResult user_balance_wrap "Failure getting user_balance 229"
        case username== user of
            True -> multiExec $ do
                set (B.append "payment_done_" payment) (BC.pack "1")
            _ -> multiExec $ do
                _ <- set (B.append "balance_" username) diff
                _ <- set (B.append "balance_" user) $ satoshi_add user_balance req_amount
                set (B.append "payment_done_" payment) (BC.pack "1")

    unlock_user conn username

    case resp of 
        TxSuccess _ -> return [("code", "hi")]
        _ -> return [("Error", "Payment Failure")]

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
            resp <- liftIO $ timeout 30000000 $ sendmine conn (BC.pack ("recvwork"++ sub_hash))
            let item = getMaybe (BackendException "Cannot talk to p2pool server") resp
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
    let balance = case raw_balance of
                Just b -> read . BC.unpack $ b :: BTC
                Nothing -> 0.0   :: BTC

    let unconfirmed = case raw_unconfirmed_balance of 
                    Just b -> read . BC.unpack $ b :: BTC
                    Nothing -> 0.0   :: BTC

    putStrLn "forcing amount"
    let !amount = read. BC.unpack $ rawamount :: BTC

    putStrLn "Handle cases"
    resp <- case amount > balance-unconfirmed of
        True -> do
            putStrLn "Can do it"
            putStrLn "Set Balance"
            set_user_balance conn username (BC.pack . show $ balance-amount)
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
