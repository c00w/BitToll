{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, deposit, balance, makePayment, createPayment) where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.List (sortBy)
import Database.Redis(runRedis, setnx, get, set, watch, multiExec)
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

getRequestJSON :: Request -> IO (JSObject JSValue)
getRequestJSON req = getResult <$> decode <$> BC.unpack <$> mconcat <$> runResourceT (requestBody req $$ consume)

getJSONStringVal :: JSObject JSValue -> String -> String
getJSONStringVal js key = getResult $ valFromObj key js


key_comp :: (String, String) -> (String, String) -> Ordering
key_comp a b = compare (fst a) (fst b)
-- concat vals of username, time in minutes(1 minute window), and salt then md5 hash
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
        Right True -> return [("username"::String, user), ("salt", salt)]
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

update_stored_balance :: B.ByteString -> PersistentConns -> IO ()
update_stored_balance bitcoinid conn = do
    runRedis (redis conn) $ do
        actual_recv <- liftIO $ withResource (pool conn) (\s -> do
            liftIO $ ZMQ.send s [] $ B.append "recieved" bitcoinid
            resp <-liftIO $ ZMQ.receive s
            return resp)
        watch $ [ B.append "address_recieved_" bitcoinid ]
        stored_recv <- get $ B.append "address_recieved_" bitcoinid
        watch $ [ B.append "balance_" bitcoinid ]
        stored_balance <- get $ B.append "balance_" bitcoinid
        case (stored_recv, stored_balance) of
            (Right (Just stored), Right (Just st_balance)) -> do
                if stored == actual_recv
                then return ()
                else do
                    let diff = satoshi_sub actual_recv stored
                    multiExec $ do
                        set (B.append "address_recieved_" bitcoinid) stored
                        set (B.append "balance_" bitcoinid) (satoshi_add st_balance diff)
                    return ()
            (Right stored, Left _) -> do
                multiExec $ do
                    set (B.append "address_recieved_" bitcoinid) actual_recv
                    set (B.append "balance_" bitcoinid) actual_recv
                return ()

            (_, _) -> return ()
    return ()

balance :: Request -> PersistentConns-> IO [(String, String)] 
balance info conn = do
    js <- getRequestJSON info 
    let username = getJSONStringVal js "username"
    bitcoinid <- runRedis (redis conn) $ do
        get $ B.append "address_" $ BC.pack username
    case bitcoinid of
        Right (Just id) -> update_stored_balance id conn
        _ -> return ()

    b_resp <- runRedis (redis conn) $ do
        get $ B.append "balance_" $ BC.pack username
    case b_resp of
        (Right (Just a)) -> return [("balance", BC.unpack a)]
        _ -> return []

createPayment :: Request -> PersistentConns -> IO [(String, String)]
createPayment info conn = do
    js <- getRequestJSON info
    let username = BC.pack $ getJSONStringVal js "username"
    let amount = BC.pack $ getJSONStringVal js "amount"
    paymentid <- random256String
    resp <- runRedis (redis conn) $ do
        setnx (B.append "payment_" (BC.pack paymentid)) (BC.pack (show amount))
        setnx (B.append "payment_user_" (BC.pack paymentid)) username
    val <- case resp of
        (Right True) -> return $ [("payment",paymentid)]
        (Right False) -> createPayment info conn
        _ -> error []
    return val

getRedisResult a = case a of
    (Right (Just b)) -> b
    _ -> error "BREAK"

makePayment :: Request -> PersistentConns -> IO [(String, String)]
makePayment info conn = do
    js <- getRequestJSON info
    let username = BC.pack $ getJSONStringVal js "username"
    let payment = BC.pack $ getJSONStringVal js "payment"
    runRedis (redis conn) $ do
        watch $ [B.append "balance_" username]
        balance_wrap <- get $B.append "balance_" username
        let balance = getRedisResult balance_wrap
        req_amount_wrap <- get $ B.append "payment_" payment
        let req_amount = getRedisResult req_amount_wrap

        let diff = satoshi_sub balance req_amount
        case satoshi_big diff "0" of
            False -> error "FU"
            _ -> return ""

        user_wrap <- get $ B.append "payment_user_" payment
        let user = getRedisResult user_wrap
        watch $ [B.append "balance_" user]
        user_balance_wrap <- get $ B.append "balance_" user
        let user_balance = getRedisResult user_balance_wrap
        multiExec $ do
            _ <- set (B.append "balance_" username) diff
            _ <- set (B.append "balance_" user) $ satoshi_add user_balance req_amount
            set (B.append "payment_done_" payment) (BC.pack "1")
    return []

deposit :: Request -> PersistentConns-> IO [(String, String)]
deposit info conn = do
    js <- getRequestJSON info
    let username = BC.pack $ getJSONStringVal js "username"
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
