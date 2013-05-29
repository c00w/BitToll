module BT.Config where

import Data.Configurator (lookup, Worth(Required), load)
import Data.Configurator.Types (Configured, Config)

import Data.Text (pack)

import BT.Util
import BT.Types

makeConfig :: IO (Config)
makeConfig = load [Required "/etc/bittoll/bittoll.conf"]

getConfig :: Configured a => Config -> String -> IO a
getConfig c k = do
    t <- Data.Configurator.lookup c (pack k)
    return $ getMaybe (BackendException (k ++ " lookup failed")) t

