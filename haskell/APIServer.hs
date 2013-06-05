{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc   #-}
import Control.Monad.Loc

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
import System.Timeout (timeout)
import Prelude hiding (lookup)
import Data.Conduit.Network (HostPreference(Host))
import Control.Monad.Exception (EMT, catchWithSrcLoc, runEMT, showExceptionWithTrace)
import Control.Monad.Exception.Base (NoExceptions)

exceptionHandler :: [String] -> MyException -> EMT NoExceptions IO LB.ByteString
exceptionHandler loc e = do
    liftIO $ logMsg (showExceptionWithTrace loc e)
    case e of
        RedisException _ -> return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"
        BackendException _ -> return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"
        UserException a -> return . LBC.pack $ "{\"error\":\"" ++ a ++ "\",\"error_code\":\"1\"}"
        _ -> return "{\"error\":\"Server Error\",\"error_code\":\"2\"}"

application :: PersistentConns -> Application
application conns info = do
    let path = rawPathInfo info
    responsew <- liftIO . timeout 30000000 . runEMT $ catchWithSrcLoc ( BT.Routing.route path info conns ) exceptionHandler
    liftIO $ logMsg $ show responsew
    case responsew of
        Just response -> return $
            responseLBS status200 [("Content-Type", "application/json")] response
        Nothing -> do
            liftIO $ logMsg "Api call timed out"
            return $ responseLBS status200 [] "{\"error\":\"Server Error\",\"error_code\":\"2\"}"

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
