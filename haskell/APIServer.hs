{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE BangPatterns           #-}

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
import BT.Util
import Prelude hiding (lookup)
import Data.Conduit.Network (HostPreference(Host))
import Control.Exception (fromException, catch, SomeException)

exceptionHandler :: PersistentConns -> SomeException -> IO LB.ByteString
exceptionHandler conns e = do
    logMsg (show e)
    case fromException e of
        Just (RedisException _) -> do
            logCount conns "apiserver" "request.errors" 1
            logCount conns "apiserver" "request.error.redis" 1
            return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"
        Just (BackendException _) -> do
            logCount conns "apiserver" "request.errors" 1
            logCount conns "apiserver" "request.error.backend" 1
            return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"
        Just (UserException a) -> do
            logCount conns "apiserver" "request.error.user" 1
            return . LBC.pack $ "{\"error\":\"" ++ a ++ "\",\"error_code\":\"1\"}"
        _ -> do
            logCount conns "apiserver" "request.errors" 1
            logCount conns "apiserver" "request.error.unknown" 1
            return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"

application :: PersistentConns -> Application
application conns info = do
    let path = rawPathInfo info
    !response <- liftIO $ catch ( do
        start <- getCurrentTime
        logCount conns "apiserver" "requests" 1
        resp <- BT.Routing.route path info conns
        logTimer conns "apiserver" "request_time" start
        return resp
        ) (exceptionHandler conns)
    body <- liftIO $ getRequestBody info
    liftIO . logMsg . show $ body
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
