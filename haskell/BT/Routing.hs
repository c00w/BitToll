{-# LANGUAGE OverloadedStrings #-}

module BT.Routing(route) where



import Data.Map
import BT.EndPoints
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request)
import Data.Aeson.Encode (encode)
import BT.Types
import BT.Util (logMsg, logCount)

jstostring :: (Request -> PersistentConns -> IO [(String, String)]) -> Request -> PersistentConns -> IO BL.ByteString
jstostring obj r p= do
    resp <- obj r p
    return . encode . Data.Map.fromList $ resp ++ [("error_code", "0")]

router :: Map B.ByteString (Request -> PersistentConns -> IO BL.ByteString)
router = Data.Map.fromList [
        ("/register",   jstostring BT.EndPoints.register),
        ("/balance",    jstostring BT.EndPoints.getBalance),
        ("/withdraw",   jstostring BT.EndPoints.sendBTC),
        ("/deposit",    jstostring BT.EndPoints.deposit),
        ("/request",    jstostring BT.EndPoints.createPayment),
        ("/setalias",   jstostring BT.EndPoints.setAlias),
        ("/alias",      jstostring BT.EndPoints.getAlias),
        ("/mine",       BT.EndPoints.mine),
        ("/",           BT.EndPoints.mine),
        ("/pay",        jstostring BT.EndPoints.makePayment)]

route :: B.ByteString -> Request -> PersistentConns -> IO BL.ByteString
route path info conns = case Data.Map.lookup path router of
                Nothing -> do
                    logCount conns "apiserver" "request.error.endpoint" 1
                    return "{\"error\":\"No Such Method\",\"error_code\":\"1\"}"
                Just a -> do
                    logMsg $ "Handling" ++ show path
                    logCount conns "apiserver" (B.append "request.endpoint." path) 1
                    resp <- a info conns
                    logMsg $ "Handled" ++ show resp
                    return resp
