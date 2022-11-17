{-# LANGUAGE RecordWildCards #-}

module Unison.CommandLine.Server.Types where

import Control.Monad.Except
import Data.Aeson
import qualified Data.Aeson as Aeson
import Servant.Server
import Unison.Cli.Monad
import Unison.Codebase.Editor.Output (CommandResponse)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Server.Orphans ()

data RunCommandRequest = RunCommandRequest
  { command :: String,
    args :: [String],
    namespace :: Path.Absolute
  }

instance FromJSON RunCommandRequest where
  parseJSON = Aeson.withObject "Command" $ \obj -> do
    command <- obj Aeson..: "command"
    args <- obj Aeson..: "arguments"
    pathStr <- obj Aeson..: "namespace"
    namespace <- case Path.parsePath' pathStr of
      Left err -> fail err
      Right (Path.RelativePath' {}) -> fail "Expected absolute path for 'namespace'"
      Right (Path.AbsolutePath' p) -> pure p
    pure $ RunCommandRequest {..}

data RunCommandResponse = RunCommandResponse
  { response :: CommandResponse
  }

instance ToJSON RunCommandResponse where
  toJSON (RunCommandResponse r) = either toJSON toJSON r

type CommandLineServer = ExceptT ServerError Cli

liftCli :: Cli a -> CommandLineServer a
liftCli = lift
