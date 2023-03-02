-- | @project.clone@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectClone
  ( projectClone,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Project (ProjectName)

-- | Clone a remote project.
projectClone :: ProjectName -> Cli ()
projectClone _name = do
  pure ()
