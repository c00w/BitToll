{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, deposit, getBalance, makePayment, createPayment, mine) where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Database.Redis(runRedis, setnx, get, set, watch, multiExec, TxResult(TxSuccess))
import Network.Wai (Request)
import BT.Types
import BT.Util
import BT.JSON
import BT.User
import qualified System.ZMQ3 as ZMQ
import Data.Pool (withResource)

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
    let username = getMaybe (UserException "Missing username field") $ lookup "username" requestal
    bitcoinid_wrap <- runRedis (redis conn) $ do
        get $ B.append "address_" $ BC.pack username
    case bitcoinid_wrap of
        Right (Just bitcoinid) -> update_stored_balance bitcoinid (BC.pack username) conn
        _ -> return ()

    b_resp <- runRedis (redis conn) $ do
        get $ B.append "balance_" $ BC.pack username
    case b_resp of
        (Right (Just a)) -> return [("balance", BC.unpack a)]
        (Right Nothing )-> return [("balance", "0")]
        _ -> return []

createPayment :: Request -> PersistentConns -> IO [(String, String)]
createPayment info conn = do
    al <- getRequestAL info
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
    let username = BC.pack $ getMaybe (UserException "Missing username") $ lookup "username" al
    let payment = BC.pack $ getMaybe (UserException "Missing payment") $ lookup "payment" al
    resp <- runRedis (redis conn) $ do
        s <- watch $ [B.append "balance_" username]
        checkWatch s
        balance_wrap <- get $B.append "balance_" username
        let balance = getRedisResult balance_wrap "Failure Getting Balance 215"
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
    case resp of 
        TxSuccess _ -> return [("code", "hi")]
        _ -> return [("Error", "Payment Failure")]

deposit :: Request -> PersistentConns-> IO [(String, String)]
deposit info conn = do
    al <- getRequestAL info
    let username = BC.pack $ getMaybe (UserException "Missing username") $ lookup "username" al
    addr <- runRedis (redis conn) $ do
        get $ B.append "address_" username
    case addr of
        (Right (Just a)) -> return $ [("address",BC.unpack a)]
        _ -> do
            resp <- withResource (pool conn) (\s -> do
                ZMQ.send s [] "address"
                resp <- ZMQ.receive s
                return resp)
            ok <- runRedis (redis conn) $ do
                setnx (B.append "address_" username)  resp
            case ok of
                Right True -> return $ [("address", BC.unpack resp)]
                _ -> return []

mine :: Request -> PersistentConns -> IO [(String, String)]
mine info conn = do
    al <- getRequestAL info
    let username = BC.pack $ getMaybe (UserException "Missing username") $ lookup "username" al
    result <- runRedis (redis conn) $ do
        sw <- watch $ [B.append "balance_" username]
        checkWatch sw
        user_balance_wrap <- get $ B.append "balance_" username
        amount <- case user_balance_wrap of
            (Right (Just a)) -> return $ satoshi_add a ( BC.pack "0.1")
            (Right (Nothing)) -> return $ BC.pack "0.1"
            _ -> error "Failed to talk to box 271"
        multiExec $ do
            set (B.append "balance_" username) amount

    case result of
        TxSuccess _ -> return [("Success", "yay")]
        _ -> mine info conn
