-- | @project.create@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectCreate
  ( projectCreate,
  )
where

import Unison.Project (ProjectName)
import Unison.Cli.Monad (Cli)

projectCreate :: ProjectName -> Cli ()
projectCreate _projectName = do
  pure ()
