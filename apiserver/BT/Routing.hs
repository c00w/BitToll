{-# LANGUAGE OverloadedStrings #-}
module BT.Routing where

import Data.Map
import BT.EndPoints
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

router = Data.Map.fromList $ [
        ("", BT.EndPoints.helloworld),
        ("/", BT.EndPoints.helloworld),
        ("/balance", BT.EndPoints.helloworld)]

route :: B.ByteString -> BL.ByteString
route path = case Data.Map.lookup path router of
                Nothing -> "404"
                Just a -> a
