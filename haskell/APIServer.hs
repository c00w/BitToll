{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DeriveDataTypeable     #-}

import Network.Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsHost, settingsPort)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock(getCurrentTime)
import BT.Routing
import BT.Global
import BT.Types
import BT.Log
import Prelude hiding (lookup)
import Data.Conduit.Network (HostPreference(Host))
import Control.Exception (catch)

exceptionHandler :: MyException -> IO LB.ByteString
exceptionHandler e = do
    logMsg (show e)
    case e of
        RedisException _ -> return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"
        BackendException _ -> return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"
        UserException a -> return . LBC.pack $ "{\"error\":\"" ++ a ++ "\",\"error_code\":\"1\"}"
        _ -> return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"

application :: PersistentConns -> Application
application conns info = do
    let path = rawPathInfo info
    response <- liftIO $ catch ( do
        start <- getCurrentTime
        resp <- BT.Routing.route path info conns
        logCount "apiserver" "requests" 1
        logTimer "apiserver" "request_time" start
        return resp
        ) exceptionHandler
    liftIO . logMsg . show $ response
    return $ responseLBS status200 [("Content-Type", "application/json")] response

main :: IO ()
main = do
    handles <- makeCons
    host <- getConfigP handles "api.host" :: IO String
    port <- getConfigP handles "api.port" :: IO Int
    let settings = defaultSettings {
        settingsHost = Host host,
        settingsPort = port
    }
    logMsg $ "Starting " ++ host ++ ":" ++ show port
    runSettings settings $ application handles
