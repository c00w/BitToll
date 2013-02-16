{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, deposit, getBalance, makePayment, createPayment) where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.List (sortBy)
import Database.Redis(Redis, runRedis, setnx, get, set, watch, multiExec, TxResult(TxSuccess))
import Network.Wai (Request, requestBody)
import Numeric (showHex)
import Text.JSON
import BT.Global
import System.Random (randomIO)
import qualified System.ZMQ3 as ZMQ
import Data.Pool (withResource)
import Data.Word (Word64)
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.List (consume)
import Data.Monoid(mconcat)
import Control.Applicative
import Crypto.Hash.MD5 (hash)

randomNum :: IO Word64
randomNum = randomIO

randomString :: IO String
randomString = do
    a <- randomNum
    return $ showHex a ""

random256String :: IO String
random256String = do
    a <- randomString
    b <- randomString
    c <- randomString
    d <- randomString
    return $ a ++ b ++ c ++ d


getResult :: Result a -> a
getResult res = case res of
                    Ok val -> val
                    Error err -> error err

getMaybe :: Maybe a -> a
getMaybe may = case may of
    Just a -> a
    _ -> error "Not good."

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
    let sign = getMaybe $ signWrap
    let al_minus_sign = filter (\s -> not (fst s == "sign")) al
    if not $ validate_sign al_minus_sign sign
    then error "Invalid Sign"
    else return al

key_comp :: (String, String) -> (String, String) -> Ordering
key_comp a b = compare (fst a) (fst b)

validate_sign :: [(String, String)] -> String -> Bool
validate_sign sec_rec sign = BC.pack sign == (hash $ BC.pack $ concat $ map snd sort_sec_rec)
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
satoshi_big a b = case (BC.readInt a, BC.readInt b) of
    (Just (c, _), Just (d, _)) -> c > d
    _ -> error "satoshi_comp"

satoshi_sub :: B.ByteString -> B.ByteString -> B.ByteString
satoshi_sub a b = case (BC.readInt a, BC.readInt b) of
    (Just (c, _), Just (d, _)) -> BC.pack $ show $ c-d
    _ -> error "satoshi_comp"

satoshi_add :: B.ByteString -> B.ByteString -> B.ByteString
satoshi_add a b = case (BC.readInt a, BC.readInt b) of
    (Just (c, _), Just (d, _)) -> BC.pack $ show $ c+d
    _ -> error "satoshi_comp"

checkWatch :: (Either a b) -> Redis ()
checkWatch a = do 
    case a of
        Left _ -> return $ error "watch"
        _ -> return ()

update_stored_balance :: B.ByteString -> PersistentConns -> IO ()
update_stored_balance bitcoinid conn = do
    runRedis (redis conn) $ do
        actual_recv <- liftIO $ withResource (pool conn) (\s -> do
            liftIO $ ZMQ.send s [] $ B.append "recieved" bitcoinid
            resp <-liftIO $ ZMQ.receive s
            return resp)
        s <- watch $ [ B.append "address_recieved_" bitcoinid ]
        checkWatch s
        stored_recv <- get $ B.append "address_recieved_" bitcoinid
        bw <- watch $ [ B.append "balance_" bitcoinid ]
        checkWatch bw
        stored_balance <- get $ B.append "balance_" bitcoinid
        case (stored_recv, stored_balance) of
            (Right (Just stored), Right (Just st_balance)) -> do
                if stored == actual_recv
                then return ()
                else do
                    let diff = satoshi_sub actual_recv stored
                    cm <- multiExec $ do
                        _ <- set (B.append "address_recieved_" bitcoinid) stored
                        set (B.append "balance_" bitcoinid) (satoshi_add st_balance diff)
                    case cm of
                        TxSuccess _ -> return ()
                        _ -> liftIO $ update_stored_balance bitcoinid conn
            (Right (Just _), _) -> do
                cm <- multiExec $ do
                    _ <- set (B.append "address_recieved_" bitcoinid) actual_recv
                    set (B.append "balance_" bitcoinid) actual_recv
                case cm of
                        TxSuccess _ -> return ()
                        _ -> liftIO $ update_stored_balance bitcoinid conn

                return ()

            (_, _) -> return ()
    return ()

getBalance :: Request -> PersistentConns-> IO [(String, String)] 
getBalance info conn = do
    requestal <- getRequestAL info
    let username = getMaybe $ lookup "username" requestal
    bitcoinid_wrap <- runRedis (redis conn) $ do
        get $ B.append "address_" $ BC.pack username
    case bitcoinid_wrap of
        Right (Just bitcoinid) -> update_stored_balance bitcoinid conn
        _ -> return ()

    b_resp <- runRedis (redis conn) $ do
        get $ B.append "balance_" $ BC.pack username
    case b_resp of
        (Right (Just a)) -> return [("balance", BC.unpack a)]
        _ -> return []

createPayment :: Request -> PersistentConns -> IO [(String, String)]
createPayment info conn = do
    al <- getRequestAL info
    let username = BC.pack$ getMaybe $ lookup "username" al
    let amount = BC.pack $ getMaybe $ lookup "amount" al
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

getRedisResult :: Either t (Maybe a) -> a
getRedisResult a = case a of
    (Right (Just b)) -> b
    _ -> error "BREAK"

makePayment :: Request -> PersistentConns -> IO [(String, String)]
makePayment info conn = do
    al <- getRequestAL info
    let username = BC.pack $ getMaybe $ lookup "username" al
    let payment = BC.pack $ getMaybe $ lookup "payment" al
    resp <- runRedis (redis conn) $ do
        s <- watch $ [B.append "balance_" username]
        checkWatch s
        balance_wrap <- get $B.append "balance_" username
        let balance = getRedisResult balance_wrap
        req_amount_wrap <- get $ B.append "payment_" payment
        let req_amount = getRedisResult req_amount_wrap

        let diff = satoshi_sub balance req_amount
        case satoshi_big diff "0" of
            False -> error "FU"
            _ -> return ()

        user_wrap <- get $ B.append "payment_user_" payment
        let user = getRedisResult user_wrap
        sw <- watch $ [B.append "balance_" user]
        checkWatch sw
        user_balance_wrap <- get $ B.append "balance_" user
        let user_balance = getRedisResult user_balance_wrap
        multiExec $ do
            _ <- set (B.append "balance_" username) diff
            _ <- set (B.append "balance_" user) $ satoshi_add user_balance req_amount
            set (B.append "payment_done_" payment) (BC.pack "1")
    case resp of 
        TxSuccess _ -> return []
        _ -> return [("Error", "Payment Failure")]

deposit :: Request -> PersistentConns-> IO [(String, String)]
deposit info conn = do
    al <- getRequestAL info
    let username = BC.pack $ getMaybe $ lookup "username" al
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
