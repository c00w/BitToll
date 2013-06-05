{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}
module BT.JSON (getRequestMap, verifyMap) where 

import Control.Monad.Loc

import BT.Types
import BT.Util
import BT.User
import Control.Monad.Exception (throw)
import Crypto.Hash.MD5
import Control.Monad (unless, liftM)
import Data.Aeson (decode)
import Data.List (sortBy)
import Data.Hex (hex)
import Data.Char (toUpper)
import Data.Map (Map, toList)
import Network.Wai (Request)
import qualified Data.ByteString.Char8 as BC

getRequestMap :: Request -> BTIO (Map String String)
getRequestMap req = do
    body <- getRequestBody req
    let obj = decode body :: Maybe (Map String String)
    getMaybe (UserException "Invalid Json Object") obj

verifyMap :: PersistentConns -> Map String String -> BTIO ()
verifyMap conn mapd = do
    let al = toList mapd
    let signWrap = lookup "sign" al
    sign <- getMaybe (UserException "no sign") signWrap
    let al_minus_sign = filter (\s -> fst s /= "sign") al
    username <- liftM BC.pack . getMaybe (UserException "Missing username field") . lookup "username" $ al
    secretWrap <- getUserSecret conn username
    secret <- liftM BC.unpack . getMaybe (UserException "Invalid User") $ secretWrap
    unless (validateSign al_minus_sign sign secret) (throw $ UserException "Invalid Sign")

keyComp :: (String, String) -> (String, String) -> Ordering
keyComp a b = compare (fst a) (fst b)

validateSign :: [(String, String)] -> String -> String -> Bool
validateSign sec_rec sign secret = BC.pack (map toUpper sign) == (hex . hash . BC.pack . concat $ map snd sort_sec_rec ++ [secret])
    where sort_sec_rec = sortBy keyComp sec_rec


