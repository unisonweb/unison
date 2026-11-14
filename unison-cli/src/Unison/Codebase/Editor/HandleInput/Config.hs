module Unison.Codebase.Editor.HandleInput.Config (handleConfigSet, handleConfigGet) where

import U.Codebase.Config (ConfigKey)
import U.Codebase.Config qualified as Config
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Prelude

handleConfigSet :: ConfigKey -> Text -> Cli ()
handleConfigSet key value = do
  case key of
    Config.CommandAliasesKey command -> do
      -- Don't let one alias name two different commands.
      otherCommands <- Cli.runTransaction Q.getCommandAliases
      let taken =
            [ (alias, otherCommand)
            | (otherCommand, otherAliases) <- otherCommands,
              otherCommand /= command,
              alias <- Config.parseCommandAliases value,
              alias `elem` Config.parseCommandAliases otherAliases
            ]
      when (not (null taken)) do
        Cli.returnEarly (CommandAliasesAlreadyTaken taken)
    Config.AuthorNameKey -> pure ()
  Cli.runTransaction $ Q.setConfigValue key value

handleConfigGet :: ConfigKey -> Cli ()
handleConfigGet key = do
  mayValue <- Cli.runTransaction $ Q.getConfigValue key
  Cli.respond $ ConfigValueGet key mayValue
