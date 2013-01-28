{-# LANGUAGE OverloadedStrings #-}
module BT.EndPoints where
import qualified Data.ByteString.Lazy as BL

helloworld = BL.fromChunks ["Hello", "World"]
