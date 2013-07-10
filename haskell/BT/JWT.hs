{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module BT.JWT (encode) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA (hmacSha256)
import qualified Data.Binary as Bin

data JWTRequest = JWTRequest {
    name :: String,
    descript :: String,
    price :: String,
    currencyCode :: String,
    sellerData :: String
} deriving Generic

instance ToJSON JWTRequest

data JWT = JWT {
    iss :: String,
    aud :: String,
    typ :: String,
    exp :: Int,
    iat :: Int,
    request :: JWTRequest
} deriving Generic

instance ToJSON JWT

header :: B.ByteString
header = B64.encode "{\"typ\":\"JWT\",\"alg\":\"HS256\"}"

encodeJWT :: JWT -> B.ByteString
encodeJWT = B64.encode . BL.toStrict . A.encode


encode :: B.ByteString -> JWT -> B.ByteString
encode key jwt = B.intercalate "." [
    header,
    encodeJWT jwt,
    BL.toStrict . Bin.encode . hmacSha256 (BL.fromStrict key) . BL.fromStrict . B.intercalate "." $ [header, encodeJWT jwt]
    ]

