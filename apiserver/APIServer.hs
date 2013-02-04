{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Char8 (split)
import Control.Monad.IO.Class (liftIO)
import BT.Routing
import BT.Global
import Database.Redis 

getPathCheck :: B.ByteString -> B.ByteString
getPathCheck path = let splitpath = split '/' path
                    in case length splitpath of
                        0 -> ""
                        1 -> ""
                        _ -> (splitpath !! 1)

application :: PersistentConns -> Application
application conns info = do
    let path = rawPathInfo info
    response <- liftIO $ BT.Routing.route path info conns
    return $
        responseLBS status200 [("Content-Type", "text/plain")] response

main = do
    handles <- makeCons
    run 3000 $ application handles
