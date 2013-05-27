{-# LANGUAGE OverloadedStrings #-}
import BT.Global
import BT.Mining
import BT.User

import qualified Data.ByteString.Char8 as BC

import System.Environment (getArgs)
import Control.Monad (liftM)

main :: IO ()
main = do
    conn <- makeCons
    username <- (liftM (BC.pack . head)) getArgs
    BC.putStrLn username

    share <- getCurrentMiningShare conn username
    _ <- increment_user_balance conn username 1.0
    _ <- increment_unconfirmed_balance conn username 1.0
    _ <- incrementSharePayout conn share 1.0
    return ()
