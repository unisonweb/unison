module Unison.Codebase.Editor.HandleInput.Upgrade
  ( handleUpgrade,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.NameSegment (NameSegment)

handleUpgrade :: NameSegment -> NameSegment -> Cli ()
handleUpgrade _ _ = pure ()
