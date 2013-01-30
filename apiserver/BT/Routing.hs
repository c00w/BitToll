{-# LANGUAGE OverloadedStrings #-}
module BT.Routing(route) where

import Data.Map
import BT.EndPoints
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request)
import Database.Redis (Connection)

router = Data.Map.fromList $ [
        ("", BT.EndPoints.helloworld),
        ("/", BT.EndPoints.helloworld),
        ("/balance", BT.EndPoints.helloworld),
        ("/register", BT.EndPoints.register)]

route :: B.ByteString -> Request -> Connection -> IO BL.ByteString
route path info redis = case Data.Map.lookup path router of
                Nothing -> return "404"
                Just a -> a info redis
