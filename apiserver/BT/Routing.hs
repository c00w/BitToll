{-# LANGUAGE OverloadedStrings #-}
module BT.Routing where

import Data.Map
import BT.EndPoints
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request)

router = Data.Map.fromList $ [
        ("", BT.EndPoints.helloworld),
        ("/", BT.EndPoints.helloworld),
        ("/balance", BT.EndPoints.helloworld),
        ("/register", BT.EndPoints.register)]

route :: B.ByteString -> Request -> IO BL.ByteString
route path info = case Data.Map.lookup path router of
                Nothing -> return "404"
                Just a -> a info
