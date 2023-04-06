-- | @branch@ input handler
module Unison.Codebase.Editor.HandleInput.Branch
  ( handleBranch,
  )
where

import Control.Lens (view, (^.))
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (getCurrentBranch, updateAt)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite

-- | Create a new project branch from an existing project branch or namespace.
handleBranch :: These ProjectName ProjectBranchName -> Cli ()
handleBranch projectAndBranchNames0 = do
  projectAndBranchNames@(ProjectAndBranch projectName newBranchName) <- ProjectUtils.hydrateNames projectAndBranchNames0

  maybeCurrentProject <- ProjectUtils.getCurrentProjectBranch

  (projectId, newBranchId) <-
    Cli.runEitherTransaction do
      Queries.loadProjectByName projectName >>= \case
        Nothing -> pure (Left (Output.LocalProjectBranchDoesntExist projectAndBranchNames))
        Just project -> do
          let projectId = project ^. #projectId
          Queries.projectBranchExistsByName projectId newBranchName >>= \case
            True -> pure (Left (Output.ProjectAndBranchNameAlreadyExists projectAndBranchNames))
            False ->
              -- Here, we are forking to `foo/bar`, where project `foo` does exist, and it does not have a branch named
              -- `bar`, so the fork will succeed.
              --
              -- If we are currently on a different branch of `foo`, we record the current branch as the parent of
              -- `bar`.
              fmap Right do
                newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
                let parentBranch = do
                      ProjectAndBranch currentProject currentBranch <- maybeCurrentProject
                      guard (projectId == currentProject ^. #projectId)
                      Just currentBranch
                Queries.insertProjectBranch
                  Sqlite.ProjectBranch
                    { projectId,
                      branchId = newBranchId,
                      name = newBranchName,
                      parentBranchId = view #branchId <$> parentBranch
                    }
                pure (projectId, newBranchId)

  let path = ProjectUtils.projectBranchPath (ProjectAndBranch projectId newBranchId)
  currentNamespaceObject <- Cli.getCurrentBranch
  let description = "branch.fork " <> into @Text (These projectName newBranchName)
  _ <- Cli.updateAt description path (const currentNamespaceObject)
  Cli.respond (Output.CreatedProjectBranch Nothing newBranchName)
  Cli.cd path
