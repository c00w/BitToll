{-# LANGUAGE OverloadedStrings #-}
module BT.Routing(route) where

import Data.Map
import BT.EndPoints
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Network.Wai (Request)
import BT.Types (PersistentConns)
import Text.JSON (toJSObject, encode)

router :: Map B.ByteString (Request -> PersistentConns -> IO [(String, String)])
router = Data.Map.fromList $ [
        ("/register", BT.EndPoints.register),
        ("/balance", BT.EndPoints.getBalance),
        ("/deposit", BT.EndPoints.deposit),
        ("/request", BT.EndPoints.createPayment),
        ("/mine", BT.EndPoints.mine),
        ("/pay", BT.EndPoints.makePayment)]

route :: B.ByteString -> Request -> PersistentConns -> IO BL.ByteString
route path info conns = case Data.Map.lookup path router of
                Nothing -> return "{\"error\":\"No Such Method\"}"
                Just a -> do
                    putStrLn $ "Handling" ++ (show path)
                    resp <- a info conns
                    putStrLn $ "Handled" ++ (show resp)
                    let enc_io = pack $ encode $ toJSObject resp
                    return enc_io
