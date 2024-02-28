module Unison.Codebase.Editor.HandleInput.DebugHashValidate (debugHashValidate) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase.Editor.Output (Output (HashValidationResult))
import Unison.Hash32 (Hash32)
import Unison.Sync.Common (expectEntity)
import Unison.Sync.EntityValidation qualified as EV

debugHashValidate :: Hash32 -> Cli ()
debugHashValidate hash = do
  entity <- Cli.runTransaction $ expectEntity hash
  Cli.respond . HashValidationResult $ EV.validateEntity hash entity
