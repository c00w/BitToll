{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import BT.Routing

getPathCheck :: B.ByteString -> Maybe B.ByteString
getPathCheck path = let splitpath = C.split '/' path
                    in case length splitpath of
                        0 -> Nothing
                        1 -> Just ""
                        _ -> Just (splitpath !! 1)

application :: Application
application info = do
    let path = rawPathInfo info
        responsepath = case getPathCheck path of
                            Nothing -> "Nope"
                            Just a-> a
    return $
        responseLBS status200 [("Content-Type", "text/plain")] (BL.fromChunks [responsepath, path])

main = run 3000 application
