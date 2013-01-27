{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

application _ = return $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main = run 3000 application
