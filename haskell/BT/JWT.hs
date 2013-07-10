{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module BT.JWT (encode, getJWT) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA (hmacSha256)
import qualified Data.Binary as Bin
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Prelude hiding (exp)

data JWTRequest = JWTRequest {
    name :: B.ByteString,
    description :: B.ByteString,
    price :: B.ByteString,
    currencyCode :: B.ByteString,
    sellerData :: B.ByteString
} deriving Generic

instance ToJSON JWTRequest

data JWT = JWT {
    iss :: B.ByteString,
    aud :: B.ByteString,
    typ :: B.ByteString,
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

time :: IO Int
time = getCurrentTime >>= return . floor. toRational .  utcTimeToPOSIXSeconds

getJWT :: B.ByteString -> B.ByteString -> B.ByteString -> IO B.ByteString
getJWT sellerID sellerSecret userAlias = do
    t <- time
    let s = encode sellerSecret JWT {
    iss = sellerID,
    aud = "Google",
    typ = "google/payments/inapp/item/v1",
    exp = t + 3600,
    iat = t,
    request = JWTRequest {
        name         = "5$ Worth Of Bitcoins on Bittoll Servers",
        description  = "5$ Worth of Bitcoins deposited on Bittoll Servers to be used in Bittoll transactions or withdrawn into the Bitcoin network",
        price        = "5.00",
        currencyCode = "USD",
        sellerData   = userAlias
        }
    }

    return s
