module Unison.Codebase.Editor.HandleInput.LSPDebug (debugLspNameCompletion) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase.Editor.Output (Output (DisplayDebugLSPNameCompletions))
import Unison.LSP.Completion qualified as Completion
import Unison.Prelude

debugLspNameCompletion :: Text -> Cli ()
debugLspNameCompletion prefix = do
  names <- Cli.currentNames
  let ct = Completion.namesToCompletionTree names
  let (_, matches) = Completion.completionsForQuery ct prefix
  Cli.respond $ DisplayDebugLSPNameCompletions matches
