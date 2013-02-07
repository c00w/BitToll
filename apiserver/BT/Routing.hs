{-# LANGUAGE OverloadedStrings #-}
module BT.Routing(route) where

import Data.Map
import BT.EndPoints
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request)
import BT.Global (PersistentConns)

router = Data.Map.fromList $ [
        ("/register", BT.EndPoints.register),
        ("/balance", BT.EndPoints.balance),
        ("/deposit", BT.EndPoints.deposit)]

route :: B.ByteString -> Request -> PersistentConns -> IO BL.ByteString
route path info conns = case Data.Map.lookup path router of
                Nothing -> return "404"
                Just a -> a info conns
