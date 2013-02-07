{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as LB
import Control.Monad.IO.Class (liftIO)
import BT.Routing
import BT.Global
import Control.Exception (catch, SomeException)
import System.IO(hPutStr, stderr)

exceptionHandler :: SomeException -> IO LB.ByteString
exceptionHandler e = do
    hPutStr stderr $ show e
    return "Server Error"

application :: PersistentConns -> Application
application conns info = do
    let path = rawPathInfo info
    response <- liftIO $ catch ( BT.Routing.route path info conns ) exceptionHandler
    return $
        responseLBS status200 [("Content-Type", "text/plain")] response

main :: IO ()
main = do
    handles <- makeCons
    run 3000 $ application handles
