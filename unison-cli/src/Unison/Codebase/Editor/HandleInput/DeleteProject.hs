-- | @delete.project@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteProject
  ( handleDeleteProject,
  )
where

import Control.Lens (view, (^.))
import Data.Function (on)
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectName)

-- | Delete a project branch.
--
-- Currently, deleting a branch means deleting its `project_branch` row, then deleting its contents from the namespace.
-- Its children branches, if any, are reparented to their grandparent, if any. You may delete the only branch in a
-- project.
handleDeleteProject :: ProjectName -> Cli ()
handleDeleteProject projectName = do
  maybeCurrentBranch <- ProjectUtils.getCurrentProjectBranch

  deletedProject <-
    Cli.runEitherTransaction do
      Queries.loadProjectByName projectName >>= \case
        Nothing -> pure (Left (Output.LocalProjectDoesntExist projectName))
        Just project -> do
          Queries.deleteProject (project ^. #projectId)
          pure (Right project)

  let projectId = deletedProject ^. #projectId

  Cli.updateAt
    ("delete.project " <> into @Text projectName)
    (ProjectUtils.projectPath projectId)
    (const Branch.empty)

  -- If the user is on the project that they're deleting, we cd to the root path
  whenJust maybeCurrentBranch \(ProjectAndBranch currentProject _currentBranch, _restPath) ->
    when (on (==) (view #projectId) deletedProject currentProject) do
      Cli.cd (Path.Absolute Path.empty)
