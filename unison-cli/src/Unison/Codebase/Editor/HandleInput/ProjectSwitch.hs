-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens ((^.))
import Data.These (These (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (..), ProjectBranchName, ProjectName)

-- | Switch to an existing project or project branch, with a flexible syntax that does not require prefixing branch
-- names with forward slashes (though doing so makes the command unambiguous).
--
-- When the argument is ambiguous, when outside of a project, "switch foo" means switch to project "foo", not branch
-- "foo" (since there is no current project). And when inside a project, "switch foo" means one of:
--
--   1. Switch to project "foo", since there isn't a branch "foo" in the current project
--   2. Switch to branch "foo", since there isn't a project "foo"
--   3. Complain, because there's both a project "foo" and a branch "foo" in the current project
projectSwitch :: ProjectAndBranchNames -> Cli ()
projectSwitch = \case
  ProjectAndBranchNames'Ambiguous projectName branchName ->
    ProjectUtils.getCurrentProjectBranch >>= \case
      Nothing -> switchToProjectAndBranchByTheseNames (This projectName)
      Just (ProjectAndBranch currentProject _currentBranch) -> do
        (projectExists, branchExists) <-
          Cli.runTransaction do
            (,)
              <$> Queries.projectExistsByName projectName
              <*> Queries.projectBranchExistsByName (currentProject ^. #projectId) branchName
        case (projectExists, branchExists) of
          (False, False) -> Cli.respond (Output.LocalProjectNorProjectBranchExist projectName branchName)
          (False, True) -> switchToProjectAndBranchByTheseNames (These (currentProject ^. #name) branchName)
          (True, False) -> switchToProjectAndBranchByTheseNames (This projectName)
          (True, True) -> Cli.respond (Output.BothLocalProjectAndProjectBranchExist projectName branchName)
  ProjectAndBranchNames'Unambiguous projectAndBranchNames0 ->
    switchToProjectAndBranchByTheseNames projectAndBranchNames0

switchToProjectAndBranchByTheseNames :: These ProjectName ProjectBranchName -> Cli ()
switchToProjectAndBranchByTheseNames projectAndBranchNames0 = do
  projectAndBranchNames@(ProjectAndBranch projectName branchName) <- ProjectUtils.hydrateNames projectAndBranchNames0
  branch <-
    Cli.runTransaction (Queries.loadProjectBranchByNames projectName branchName) & onNothingM do
      Cli.returnEarly (Output.LocalProjectBranchDoesntExist projectAndBranchNames)
  Cli.cd (ProjectUtils.projectBranchPath (ProjectAndBranch (branch ^. #projectId) (branch ^. #branchId)))
