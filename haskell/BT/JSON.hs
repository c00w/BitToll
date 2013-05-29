module BT.JSON (getRequestAL, verifyAL) where 
import Text.JSON
import BT.Types
import BT.Util
import BT.User
import Control.Applicative
import Control.Exception (throw)
import Crypto.Hash.MD5
import Data.List (sortBy)
import Data.Hex (hex)
import Data.Monoid(mconcat)
import Data.Conduit
import Data.Conduit.List (consume)
import Data.Char (toUpper)
import Network.Wai (Request, requestBody)
import qualified Data.ByteString.Char8 as BC

getRequestJSON :: Request -> IO (JSObject JSValue)
getRequestJSON req = getResult <$> decode <$> BC.unpack <$> mconcat     <$> runResourceT (requestBody req $$ consume)

unjskey :: (String, JSValue) -> (String, String)
unjskey (a, JSString b) = (a, fromJSString b)
unjskey _ = error "Not a js value"

getRequestAL :: Request -> IO [(String, String)]
getRequestAL req = do
    json <- getRequestJSON req
    let jsal = fromJSObject json
    let al = map unjskey jsal
    return al

verifyAL :: PersistentConns -> [(String, String)] -> IO ()
verifyAL conn al = do
    let signWrap = lookup "sign" al
    let sign = getMaybe (UserException "no sign") signWrap
    let al_minus_sign = filter (\s -> not (fst s == "sign")) al
    let username = BC.pack $ getMaybe (UserException "Missing username field") $ lookup "username" al
    secretWrap <- get_user_secret conn username
    let secret = BC.unpack $ getMaybe (UserException "Invalid User") secretWrap
    if not $ validate_sign al_minus_sign sign secret
    then throw $ UserException "Invalid Sign"
    else return ()

key_comp :: (String, String) -> (String, String) -> Ordering
key_comp a b = compare (fst a) (fst b)

validate_sign :: [(String, String)] -> String -> String -> Bool
validate_sign sec_rec sign secret = (BC.pack $ map toUpper sign) == (hex $ hash $ BC.pack $ concat $ (map snd sort_sec_rec) ++ [secret])
    where sort_sec_rec = sortBy key_comp sec_rec


