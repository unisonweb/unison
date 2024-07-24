-- | @switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Data.These (These (..))
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (..), ProjectBranchName, ProjectName)
import Witch (unsafeFrom)

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
projectSwitch projectNames = do
  case projectNames of
    ProjectAndBranchNames'Ambiguous projectName branchName -> do
      ProjectAndBranch currentProject _currentBranch <- Cli.getCurrentProjectAndBranch
      (projectExists, branchExists) <-
        Cli.runTransaction do
          (,)
            <$> Queries.projectExistsByName projectName
            <*> Queries.projectBranchExistsByName currentProject.projectId branchName
      case (projectExists, branchExists) of
        (False, False) -> Cli.respond (Output.LocalProjectNorProjectBranchExist projectName branchName)
        (False, True) -> switchToProjectAndBranchByTheseNames (These currentProject.name branchName)
        (True, False) -> switchToProjectAndBranchByTheseNames (This projectName)
        (True, True) ->
          Cli.respondNumbered $
            Output.AmbiguousSwitch
              projectName
              (ProjectAndBranch currentProject.name branchName)
    ProjectAndBranchNames'Unambiguous projectAndBranchNames0 ->
      switchToProjectAndBranchByTheseNames projectAndBranchNames0

switchToProjectAndBranchByTheseNames :: These ProjectName ProjectBranchName -> Cli ()
switchToProjectAndBranchByTheseNames projectAndBranchNames0 = do
  branch <- case projectAndBranchNames0 of
    This projectName ->
      Cli.runTransactionWithRollback \rollback -> do
        project <-
          Queries.loadProjectByName projectName & onNothingM do
            rollback (Output.LocalProjectDoesntExist projectName)
        Queries.loadMostRecentBranch (project ^. #projectId) >>= \case
          Nothing -> do
            let branchName = unsafeFrom @Text "main"
            branch <-
              Queries.loadProjectBranchByName project.projectId branchName & onNothingM do
                rollback (Output.LocalProjectBranchDoesntExist (ProjectAndBranch projectName branchName))
            pure branch
          Just branchId -> Queries.expectProjectBranch project.projectId branchId
    _ -> do
      projectAndBranchNames@(ProjectAndBranch projectName branchName) <- ProjectUtils.hydrateNames projectAndBranchNames0
      Cli.runTransactionWithRollback \rollback -> do
        branch <-
          Queries.loadProjectBranchByNames projectName branchName & onNothingM do
            rollback (Output.LocalProjectBranchDoesntExist projectAndBranchNames)
        pure branch
  Cli.switchProject (ProjectAndBranch (branch ^. #projectId) (branch ^. #branchId))
