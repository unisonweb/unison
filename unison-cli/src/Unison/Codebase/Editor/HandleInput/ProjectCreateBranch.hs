-- | @project.create-branch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectCreateBranch
  ( projectCreateBranch,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Project (ProjectBranchName)

projectCreateBranch :: ProjectBranchName -> Cli ()
projectCreateBranch _name = do
  pure ()
