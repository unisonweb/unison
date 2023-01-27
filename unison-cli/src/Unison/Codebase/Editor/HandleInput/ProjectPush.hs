-- | @project.push@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectPush
  ( projectPush,
  )
where

import Unison.Cli.Monad (Cli)

-- | Push a project branch.
projectPush :: Cli ()
projectPush = do
  pure ()
