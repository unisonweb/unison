-- | @project.rename@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectRename
  ( handleProjectRename,
  )
where

import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectName)

handleProjectRename :: ProjectName -> Cli ()
handleProjectRename newName = do
  ProjectAndBranch project _branch <- Cli.getCurrentProjectAndBranch
  let oldName = project.name
  when (oldName /= newName) do
    Cli.runTransactionWithRollback \rollback -> do
      Queries.loadProjectByName newName >>= \case
        Just _ -> rollback (Output.ProjectNameAlreadyExists newName)
        Nothing -> Queries.renameProject project.projectId newName
  Cli.respond (Output.RenamedProject oldName newName)
