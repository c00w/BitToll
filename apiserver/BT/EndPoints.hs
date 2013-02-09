{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, deposit, balance) where
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
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


data SecretRecipe = SecretRecipe {
    usernames :: String,
    time :: String,
    sign :: String,
    salt :: String
}

-- concat vals of username, time in minutes(1 minute window), and salt then md5 hash
validate_sign :: SecretRecipe -> Bool
validate_sign sec_rec = (BC.pack $ sign sec_rec) == (hash $ BC.pack $ concat $ [usernames sec_rec, minutes, salt sec_rec])
    where
        minutes = show $ floor $ (read $ time sec_rec) / 60

register :: Request -> PersistentConns-> IO BL.ByteString
register info conn = do
    user <- random256String
    salt <- random256String
    ok <- runRedis (redis conn) $ do
        setnx (BC.pack $"user_" ++ user) (BC.pack salt)

    case ok of
        Right True -> return $ pack $ encode $ toJSObject [("username"::String, user), ("salt", salt)]
        _ -> register info conn

satoshi_big :: B.ByteString -> B.ByteString -> Bool 
satoshi_big a b = case (BC.readInt a, BC.readInt b) of
    (Just (c, _), Just (d, _)) -> c > d

satoshi_sub :: B.ByteString -> B.ByteString -> B.ByteString
satoshi_sub a b = case (BC.readInt a, BC.readInt b) of
    (Just (c, _), Just (d, _)) -> BC.pack $ show $ c-d

satoshi_add :: B.ByteString -> B.ByteString -> B.ByteString
satoshi_add a b = case (BC.readInt a, BC.readInt b) of
    (Just (c, _), Just (d, _)) -> BC.pack $ show $ c+d

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

balance :: Request -> PersistentConns-> IO BL.ByteString
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
        (Right (Just a)) -> return $ BL.fromChunks [a]
        _ -> return "FU"

createPayment :: Request -> PersistentConns -> IO BL.ByteString
createPayment info conn = do
    js <- getRequestJSON info
    let username = BC.pack $ getJSONStringVal js "username"
    let amount = BC.pack $ getJSONStringVal js "amount"
    paymentid <- random256String
    resp <- runRedis (redis conn) $ do
        setnx (B.append "payment_" (BC.pack paymentid)) (BC.pack (show amount))
        setnx (B.append "payment_user_" (BC.pack paymentid)) username
    val <- case resp of
        (Right True) -> return $ BL.fromChunks [BC.pack paymentid]
        (Right False) -> createPayment info conn
        _ -> error "FU"
    return $ val

getRedisResult a = case a of
    (Right (Just b)) -> b
    _ -> error "BREAK"

makePayment :: Request -> PersistentConns -> IO BL.ByteString
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
    return ""

deposit :: Request -> PersistentConns-> IO BL.ByteString
deposit info conn = do
    js <- getRequestJSON info
    let username = BC.pack $ getJSONStringVal js "username"
    addr <- runRedis (redis conn) $ do
        get $ B.append "address_" username
    case addr of
        (Right (Just a)) -> return $ BL.fromChunks [a]
        _ -> do
            resp <- withResource (pool conn) (\s -> do
                ZMQ.send s [] "address"
                resp <- ZMQ.receive s
                return resp)
            ok <- runRedis (redis conn) $ do
                setnx (B.append "address_" username)  resp
            case ok of
                Right True -> return $ BL.fromChunks [resp]
                _ -> return ""
