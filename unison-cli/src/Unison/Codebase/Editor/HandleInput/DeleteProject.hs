-- | @delete.project@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteProject
  ( handleDeleteProject,
  )
where

import Control.Lens (view, (^.))
import Data.Function (on)
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectName)

-- | Delete a project
handleDeleteProject :: ProjectName -> Cli ()
handleDeleteProject projectName = do
  maybeCurrentBranch <- ProjectUtils.getCurrentProjectBranch

  deletedProject <-
    Cli.runTransactionWithRollback \rollback -> do
      project <-
        Queries.loadProjectByName projectName & onNothingM do
          rollback (Output.LocalProjectDoesntExist projectName)
      Queries.deleteProject (project ^. #projectId)
      pure project

  let projectId = deletedProject ^. #projectId

  Cli.updateAt
    ("delete.project " <> into @Text projectName)
    (ProjectUtils.projectPath projectId)
    (const Branch.empty)

  -- If the user is on the project that they're deleting, we cd to the root path
  whenJust maybeCurrentBranch \(ProjectAndBranch currentProject _currentBranch, _restPath) ->
    when (on (==) (view #projectId) deletedProject currentProject) do
      Cli.cd (Path.Absolute Path.empty)
