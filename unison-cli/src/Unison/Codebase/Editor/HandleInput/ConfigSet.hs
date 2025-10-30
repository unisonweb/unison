module Unison.Codebase.Editor.HandleInput.ConfigSet (handleConfigSet) where

import U.Codebase.Config (ConfigKey)
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Prelude

handleConfigSet :: ConfigKey -> Text -> Cli ()
handleConfigSet key value = do
  Cli.runTransaction $ Q.setConfigValue key value
