{-# LANGUAGE DeriveDataTypeable #-}
module BT.Config where

import Data.Configurator (lookup, Worth(Required), load)
import Data.Configurator.Types (Configured, Config)
import Data.Text (pack)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import Control.Exception

data ConfigException = ConfigException String
    deriving (Show, Typeable)

instance Exception ConfigException

makeConfig :: IO Config
makeConfig = load [Required "/etc/bittoll/bittoll.conf"]

getConfig :: Configured a => Config -> String -> IO a
getConfig c k = do
    t <- Data.Configurator.lookup c (pack k)
    return $ fromMaybe (throw $ ConfigException k) t
