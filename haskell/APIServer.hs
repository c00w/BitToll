{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Network.Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsHost, settingsPort)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Control.Monad.IO.Class (liftIO)
import BT.Routing
import BT.Global
import BT.Types
import BT.Log
import Control.Exception (catch)
import System.IO(hPutStrLn, stdout)
import System.Timeout (timeout)
import Prelude hiding (lookup)
import Data.Conduit.Network (HostPreference(Host))

exceptionHandler :: MyException -> IO (Maybe LB.ByteString)
exceptionHandler e = do
    hPutStrLn stdout $ show e
    case e of
        RedisException _ -> return $ Just "{\"error\":\"Server Error\"}"
        BackendException _ -> return $ Just "{\"error\":\"Server Error\"}"
        UserException a -> return $ Just $ LBC.pack $ "{\"error\":\"" ++ a ++ "\"}"
        _ -> return $ Just "{\"error\":\"Server Error\"}"

application :: PersistentConns -> Application
application conns info = do
    let path = rawPathInfo info
    responsew <- liftIO $ catch (timeout 30000000 ( BT.Routing.route path info conns )) exceptionHandler
    liftIO $ logMsg $ show responsew
    case responsew of
        Just response -> return $
            responseLBS status200 [("Content-Type", "text/plain")] response
        Nothing -> do
            liftIO $ logMsg "Api call timed out"
            return $ responseLBS status200 [] "{\"error\":\"Server Error\"}"

main :: IO ()
main = do
    handles <- makeCons
    host <- getConfigP handles "api.host" :: IO String
    port <- getConfigP handles "api.port" :: IO Int
    let settings = defaultSettings {
        settingsHost = Host host,
        settingsPort = port
    }
    logMsg $ "Starting " ++ host ++ ":" ++ (show port)
    runSettings settings $ application handles
