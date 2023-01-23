-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Unison.Project (ProjectName)
import Unison.Cli.Monad (Cli)

projectSwitch :: ProjectName -> Cli ()
projectSwitch _projectName = do
  pure ()
