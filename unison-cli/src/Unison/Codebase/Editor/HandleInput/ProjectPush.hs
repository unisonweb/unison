-- | @project.push@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectPush
  ( projectPush,
  )
where

import Control.Lens ((^.))
import Unison.Cli.Monad (Cli)
import Unison.Cli.ProjectUtils (getCurrentProjectBranch)
import Unison.Prelude
import Unison.Project (ProjectAndBranch, ProjectBranchName, ProjectName)
import Witch (unsafeFrom)

-- | Push a project branch.
projectPush :: Maybe (ProjectAndBranch ProjectName (Maybe ProjectBranchName)) -> Cli ()
projectPush maybeProjectAndBranch = do
  (projectId, currentBranchId) <- getCurrentProjectBranch & onNothingM (error "not on branch")

  -- Resolve where to push:
  --   if (project/branch names provided)
  --     if (ids in remote_project / remote_project_branch tables)
  --       use those
  --     else
  --       ask Share
  --   else if (default push location exists),
  --     if (its remote branch id is non-null)
  --       use that
  --     else
  --       if (this branch name exists in that project)
  --         use that
  --       else
  --         create a branch with this name
  --   else
  --     ask Share for my username
  --     if (I'm not logged in)
  --       fail -- don't know where to push
  --     else
  --

  projectAndBranch <-
    case maybeProjectAndBranch of
      Just projectAndBranch -> pure projectAndBranch
      Nothing -> error "must provide project and branch"

  let projectName = projectAndBranch ^. #project
  let branchName = fromMaybe (unsafeFrom @Text "main") (projectAndBranch ^. #branch)

  pure ()
