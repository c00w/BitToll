{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, deposit, getBalance, makePayment, createPayment, mine) where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.List (sortBy)
import Database.Redis(Redis, runRedis, setnx, get, set, watch, multiExec, TxResult(TxSuccess))
import Network.Wai (Request, requestBody)
import Network.Bitcoin (BTC)
import Text.JSON
import BT.Types
import BT.Util
import qualified System.ZMQ3 as ZMQ
import Data.Pool (withResource)
import Data.Char (toUpper)
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.List (consume)
import Data.Monoid(mconcat)
import Control.Applicative
import Crypto.Hash.MD5 (hash)
import Data.Hex (hex)
import Control.Exception (throw)
import System.Timeout (timeout)

getRequestJSON :: Request -> IO (JSObject JSValue)
getRequestJSON req = getResult <$> decode <$> BC.unpack <$> mconcat <$> runResourceT (requestBody req $$ consume)

unjskey :: (String, JSValue) -> (String, String)
unjskey (a, JSString b) = (a, fromJSString b)
unjskey _ = error "Not a js value"

getRequestAL :: Request -> IO [(String, String)]
getRequestAL req = do
    json <- getRequestJSON req
    let jsal = fromJSObject json
    let al = map unjskey jsal
    let signWrap = lookup "sign" al
    let sign = getMaybe (UserException "no sign") signWrap
    let al_minus_sign = filter (\s -> not (fst s == "sign")) al
    if not $ validate_sign al_minus_sign sign
    then throw $ UserException "Invalid Sign"
    else return al

key_comp :: (String, String) -> (String, String) -> Ordering
key_comp a b = compare (fst a) (fst b)

validate_sign :: [(String, String)] -> String -> Bool
validate_sign sec_rec sign = (BC.pack $ map toUpper sign) == (hex $ hash $ BC.pack $ concat $ map snd sort_sec_rec)
    where sort_sec_rec = sortBy key_comp sec_rec

register :: Request -> PersistentConns-> IO [(String, String)]
register info conn = do
    user <- random256String
    salt <- random256String
    ok <- runRedis (redis conn) $ do
        setnx (BC.pack $"user_" ++ user) (BC.pack salt)

    case ok of
        Right True -> return [("username"::String, user), ("secret", salt)]
        _ -> register info conn

satoshi_big :: B.ByteString -> B.ByteString -> Bool 
satoshi_big a b = aBTC >= bBTC
    where
        aBTC = (read . BC.unpack) a :: BTC
        bBTC = (read . BC.unpack) b :: BTC

satoshi_sub :: B.ByteString -> B.ByteString -> B.ByteString
satoshi_sub a b = BC.pack . show $ aBTC - bBTC
    where
        aBTC = (read . BC.unpack) a :: BTC
        bBTC = (read . BC.unpack) b :: BTC

satoshi_add :: B.ByteString -> B.ByteString -> B.ByteString
satoshi_add a b = BC.pack . show $ aBTC + bBTC
    where
        aBTC = (read . BC.unpack) a :: BTC
        bBTC = (read . BC.unpack) b :: BTC

checkWatch :: (Either a b) -> Redis ()
checkWatch a = do 
    case a of
        Left _ -> return $ error "watch"
        _ -> return ()

checkInt :: (B.ByteString) -> Maybe B.ByteString
checkInt input = case BC.readInt input of
    (Just _) -> Just input
    _ -> Nothing

update_stored_balance :: B.ByteString -> B.ByteString -> PersistentConns -> IO ()
update_stored_balance bitcoinid userid conn = do
    runRedis (redis conn) $ do
        s <- watch $ [ B.append "address_recieved_" bitcoinid ]
        checkWatch s
        liftIO $ putStrLn "Updating balance"
        ractual_recv <- liftIO $ timeout 3000000 $ withResource (pool conn) (\s -> do
            liftIO $ ZMQ.send s [] $ B.append "recieved" bitcoinid
            resp <-liftIO $ ZMQ.receive s
            return resp)
        let actual_recv = getMaybe (BackendException "Cannot talk to bc server") ractual_recv

        stored_recv <- get $ B.append "address_recieved_" bitcoinid
        bw <- watch $ [ B.append "balance_" userid]
        checkWatch bw
        stored_balance <- get $ B.append "balance_" userid
        _ <- liftIO $ BC.putStrLn  actual_recv
        case (stored_recv, stored_balance) of
            (Right (Just stored), Right (Just st_balance)) -> do
                if stored == actual_recv
                then return ()
                else do
                    liftIO $ BC.putStrLn "Main path"
                    let diff = satoshi_sub actual_recv stored
                    cm <- multiExec $ do
                        _ <- set (B.append "address_recieved_" bitcoinid) stored
                        set (B.append "balance_" userid) (satoshi_add st_balance diff)
                    case cm of
                        TxSuccess _ -> return ()
                        _ -> liftIO $ update_stored_balance bitcoinid userid conn

            (Right (Nothing), Right (Just balance)) -> do
                liftIO $ BC.putStrLn "stored balance, no stored recv"
                cm <- multiExec $ do
                    _ <- set (B.append "address_recieved_" bitcoinid) actual_recv
                    set (B.append "balance_" userid) (satoshi_add actual_recv balance)
                case cm of
                        TxSuccess _ -> return ()
                        _ -> liftIO $ update_stored_balance bitcoinid userid conn

            (Right (Nothing), Right (Nothing)) -> do
                liftIO $ BC.putStrLn "No stored balance, no stored recieved"
                cm <- multiExec $ do
                    _ <- set (B.append "address_recieved_" bitcoinid) actual_recv
                    set (B.append "balance_" userid) actual_recv
                case cm of
                        TxSuccess _ -> return ()
                        _ -> liftIO $ update_stored_balance bitcoinid userid conn
            (_, _) -> do
                return ()
    return ()

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
