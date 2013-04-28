{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Control.Monad.IO.Class (liftIO)
import BT.Routing
import BT.Global
import BT.Types
import Control.Exception (catch)
import System.IO(hPutStrLn, stdout)
import System.Timeout (timeout)

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
    liftIO $ hPutStrLn stdout $ show responsew
    case responsew of
        Just response -> return $
            responseLBS status200 [("Content-Type", "text/plain")] response
        Nothing -> return $ responseLBS status200 [] "{\"error\":\"Server Error\"}"

main :: IO ()
main = do
    handles <- makeCons
    putStrLn "Starting"
    run 3000 $ application handles
