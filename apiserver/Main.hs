{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import BT.Routing

getPathCheck :: B.ByteString -> B.ByteString
getPathCheck path = let splitpath = C.split '/' path
                    in case length splitpath of
                        0 -> ""
                        1 -> ""
                        _ -> (splitpath !! 1)

application :: Application
application info = do
    let path = rawPathInfo info
    return $
        responseLBS status200 [("Content-Type", "text/plain")] $ BT.Routing.route path

main = run 3000 application
