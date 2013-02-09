{-# LANGUAGE OverloadedStrings #-}
module BT.Routing(route) where

import Data.Map
import BT.EndPoints
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Network.Wai (Request)
import BT.Global (PersistentConns)
import Text.JSON (toJSObject, encode)

router :: Map B.ByteString (Request -> PersistentConns -> IO [(String, String)])
router = Data.Map.fromList $ [
        ("/register", BT.EndPoints.register),
        ("/balance", BT.EndPoints.getBalance),
        ("/deposit", BT.EndPoints.deposit),
        ("/request", BT.EndPoints.createPayment),
        ("/pay", BT.EndPoints.makePayment)]

route :: B.ByteString -> Request -> PersistentConns -> IO BL.ByteString
route path info conns = case Data.Map.lookup path router of
                Nothing -> return "404"
                Just a -> do
                    resp <- a info conns
                    return $ pack $ encode $ toJSObject resp
