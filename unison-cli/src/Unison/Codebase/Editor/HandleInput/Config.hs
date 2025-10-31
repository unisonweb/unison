module Unison.Codebase.Editor.HandleInput.Config (handleConfigSet, handleConfigGet) where

import U.Codebase.Config (ConfigKey)
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Prelude

handleConfigSet :: ConfigKey -> Text -> Cli ()
handleConfigSet key value = do
  Cli.runTransaction $ Q.setConfigValue key value

handleConfigGet :: ConfigKey -> Cli ()
handleConfigGet key = do
  mayValue <- Cli.runTransaction $ Q.getConfigValue key
  Cli.respond $ ConfigValueGet key mayValue
