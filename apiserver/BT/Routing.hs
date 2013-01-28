{-# LANGUAGE OverloadedStrings #-}
module BT.Routing where

import Data.Map
import BT.EndPoints

router = Data.Map.fromList $ [
        ("", BT.EndPoints.helloworld),
        ("/", BT.EndPoints.helloworld),
        ("/balance", BT.EndPoints.helloworld)]

route path = case Data.Map.lookup path router of
                Nothing -> "404"
                Just a -> a
