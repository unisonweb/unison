module Unison.Codebase.Editor.HandleInput.Cancel
  ( handleCancel,
  )
where

import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.HandleInput.DeleteBranch (handleDeleteBranch2)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))

handleCancel :: Cli ()
handleCancel = do
  current <- Cli.getCurrentProjectPath

  when (not (current.branch.isUpdate || current.branch.isUpgrade || current.branch.isMerge)) do
    Cli.returnEarly (Output.Literal "There's no merge, update, or upgrade in progress.")

  handleDeleteBranch2 (ProjectAndBranch current.project current.branch)
