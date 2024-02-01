-- | @branch.rename@ input handler
module Unison.Codebase.Editor.HandleInput.BranchRename
  ( handleBranchRename,
  )
where

import Control.Lens ((^.))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectBranchNameKind (..), classifyProjectBranchName)

handleBranchRename :: ProjectBranchName -> Cli ()
handleBranchRename newBranchName = do
  (ProjectAndBranch project branch, _path) <- ProjectUtils.expectCurrentProjectBranch

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
