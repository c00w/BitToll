{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints(register, deposit, balance) where
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Database.Redis(runRedis, setnx, get, set, watch, multiExec)
import Network.Wai (Request)
import Numeric (showHex)
import Text.JSON
import BT.Global
import System.Random (randomIO)
import qualified System.ZMQ3 as ZMQ
import Data.Pool (withResource)
import Data.Word (Word64)
import Control.Monad.IO.Class

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

register :: Request -> PersistentConns-> IO BL.ByteString
register info conn = do
    user <- random256String
    salt <- random256String
    ok <- runRedis (redis conn) $ do
        setnx (BC.pack $"user_" ++ user) (BC.pack salt)
    
    case ok of
        Right True -> return $ pack $ encode $ toJSObject [("username"::String, user), ("salt", salt)]
        _ -> register info conn

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
    let username = "bob"
    bitcoinid <- runRedis (redis conn) $ do
        get $ B.append "address_" username
    case bitcoinid of
        Right (Just id) -> update_stored_balance id conn
        _ -> return ()

    b_resp <- runRedis (redis conn) $ do
        get $ B.append "balance_" username
    case b_resp of
        (Right (Just a)) -> return $ BL.fromChunks [a]
        _ -> return "FU"

deposit :: Request -> PersistentConns-> IO BL.ByteString
deposit info conn = do
    let username = "bob"
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
                Right a -> return $ BL.fromChunks [resp]
                _ -> return ""
