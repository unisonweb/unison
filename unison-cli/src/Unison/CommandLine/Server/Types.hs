{-# LANGUAGE RecordWildCards #-}

module Unison.CommandLine.Server.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson as Aeson
import Servant.Server
import qualified Servant.Server as Servant
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..))
import Unison.Codebase.Editor.Output (CommandResponse)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Server.Orphans ()
import UnliftIO

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
  toJSON (RunCommandResponse r) = maybe Aeson.Null (either toJSON toJSON) r

data Env = Env
  { rootVar :: TMVar (Branch IO),
    cliEnv :: Cli.Env
  }

type CommandLineServer = ReaderT Env Servant.Handler

liftCli :: Cli.Cli a -> CommandLineServer (Either CommandResponse a)
liftCli cli = do
  Env {cliEnv, rootVar} <- ask
  lastSavedRoot <- liftIO $ Codebase.getRootCausalHash (Cli.codebase cliEnv)
  let ls = Cli.loopState0 lastSavedRoot rootVar Path.absoluteEmpty
  liftIO (Cli.runCli cliEnv ls cli) >>= \case
    (rt, _ls) -> case rt of
      Cli.Success a -> pure (Right a)
      Cli.Continue response -> pure (Left response)
      Cli.HaltRepl -> throwError err500
