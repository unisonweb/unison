{-# Language OverloadedStrings #-}

module Unison.Config where

import qualified Data.Configurator as Config
import Data.Configurator.Types (Config)
import qualified Unison.Util.Logger as L
import System.IO (Handle, stderr, stdout)

-- | Load the config
load :: IO Config
load = Config.load [Config.Required "config", Config.Optional "$(HOME)/.unisonconfig"]

-- | Obtain a Logger decorator that sets the default logging level
logger :: IO (L.Logger -> L.Logger)
logger = do
  config <- load
  level <- Config.require config "logging"
  pure $ L.at level

loggerTo :: Handle -> IO L.Logger
loggerTo h = do
  t <- logger
  t <$> L.atomic (L.toHandle h)

loggerStandardOut, loggerStandardError :: IO L.Logger
(loggerStandardOut, loggerStandardError) = (loggerTo stdout, loggerTo stderr)
