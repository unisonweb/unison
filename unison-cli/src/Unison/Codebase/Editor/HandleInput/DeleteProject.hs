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
import Unison.Codebase.Editor.HandleInput.ProjectCreate (projectCreate)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Prelude
import Unison.Project (ProjectName)

-- | Delete a project
handleDeleteProject :: ProjectName -> Cli ()
handleDeleteProject projectName = do
  ProjectPath currentProject _ _ <- Cli.getCurrentProjectPath

  deletedProject <-
    Cli.runTransactionWithRollback \rollback -> do
      project <-
        Queries.loadProjectByName projectName & onNothingM do
          rollback (Output.LocalProjectDoesntExist projectName)
      Queries.deleteProject (project ^. #projectId)
      pure project

  -- If the user is on the project that they're deleting, we create a new project to switch
  -- to.
  when (((==) `on` (view #projectId)) deletedProject currentProject) do
    nextLoc <- projectCreate False Nothing
    Cli.switchProject nextLoc
