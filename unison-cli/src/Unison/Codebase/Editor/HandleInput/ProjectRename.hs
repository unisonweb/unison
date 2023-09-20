-- | @project.rename@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectRename
  ( handleProjectRename,
  )
where

import Control.Lens ((^.))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectName)

handleProjectRename :: ProjectName -> Cli ()
handleProjectRename newName = do
  project <- ProjectUtils.expectCurrentProject
  let oldName = project ^. #name
  when (oldName /= newName) do
    Cli.runTransactionWithRollback \rollback -> do
      Queries.loadProjectByName newName >>= \case
        Just _ -> rollback (Output.ProjectNameAlreadyExists newName)
        Nothing -> Queries.renameProject (project ^. #projectId) newName
  Cli.respond (Output.RenamedProject oldName newName)
