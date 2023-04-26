-- | @release.draft@ input handler
module Unison.Codebase.Editor.HandleInput.ReleaseDraft
  ( handleReleaseDraft,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Project (Semver)

-- | Handle a @release.draft@ command.
handleReleaseDraft :: Semver -> Cli ()
handleReleaseDraft _semver = do
  pure ()
