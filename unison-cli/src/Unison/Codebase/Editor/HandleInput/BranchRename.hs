-- | @branch.rename@ input handler
module Unison.Codebase.Editor.HandleInput.BranchRename
  ( handleBranchRename,
  )
where

import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectBranchNameKind (..), classifyProjectBranchName)

handleBranchRename :: ProjectBranchName -> Cli ()
handleBranchRename newBranchName = do
  PP.ProjectPath project branch _path <- Cli.getCurrentProjectPath

  case classifyProjectBranchName newBranchName of
    ProjectBranchNameKind'Contributor {} -> pure ()
    ProjectBranchNameKind'DraftRelease {} -> pure ()
    ProjectBranchNameKind'NothingSpecial {} -> pure ()
    ProjectBranchNameKind'Release {} -> Cli.returnEarly (Output.CantRenameBranchTo newBranchName)

  let projectName = project ^. #name
  let oldBranchName = branch ^. #name
  when (oldBranchName /= newBranchName) do
    Cli.runTransactionWithRollback \rollback -> do
      Queries.loadProjectBranchByName (project ^. #projectId) newBranchName >>= \case
        Just _ -> rollback (Output.ProjectAndBranchNameAlreadyExists (ProjectAndBranch projectName newBranchName))
        Nothing -> Queries.renameProjectBranch (project ^. #projectId) (branch ^. #branchId) newBranchName
  Cli.respond (Output.RenamedProjectBranch projectName oldBranchName newBranchName)
