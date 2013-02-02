{-# LANGUAGE OverloadedStrings #-}
module BT.Routing(route) where

import Data.Map
import BT.EndPoints
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request)
import Database.Redis (Connection)
import BT.Global

router = Data.Map.fromList $ [
        ("/register", BT.EndPoints.register),
        ("/deposit", BT.EndPoints.deposit)]

route :: B.ByteString -> Request -> PersistentConns -> IO BL.ByteString
route path info redis = case Data.Map.lookup path router of
                Nothing -> return "404"
                Just a -> a info redis
