-- | @branch@ input handler
module Unison.Codebase.Editor.HandleInput.Branch
  ( handleBranch,
    createBranchFromParent,
    createBranchFromNamespace,
  )
where

import Control.Monad.Reader
import Data.UUID.V4 qualified as UUID
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.ProjectBranch qualified as ProjectBranch
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectBranchNameKind (..), ProjectName, classifyProjectBranchName)
import Unison.Sqlite qualified as Sqlite

-- | Create a new project branch from an existing project branch or namespace.
handleBranch :: Input.BranchSourceI -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleBranch sourceI projectAndBranchNames@(ProjectAndBranch mayProjectName newBranchName) = do
  -- You can only create release branches with `branch.clone`
  --
  -- We do allow creating draft release branches with `branch`, but you'll get different output if you use
  -- `release.draft`
  case classifyProjectBranchName newBranchName of
    ProjectBranchNameKind'Contributor _user _name -> pure ()
    ProjectBranchNameKind'DraftRelease _ver -> pure ()
    ProjectBranchNameKind'Release ver ->
      Cli.returnEarly (Output.CannotCreateReleaseBranchWithBranchCommand newBranchName ver)
    ProjectBranchNameKind'NothingSpecial -> pure ()

  currentProjectName <- Cli.getCurrentProjectPath <&> view (#project . #name)
  let projectName = (fromMaybe currentProjectName mayProjectName)
  destProject <- do
    Cli.runTransactionWithRollback
      \rollback -> do
        Queries.loadProjectByName projectName & onNothingM do
          -- We can't make the *first* branch of a project with `branch`; the project has to already exist.
          rollback (Output.LocalProjectBranchDoesntExist (ProjectAndBranch projectName newBranchName))

  -- Compute what we should create the branch from.
  maySrcProjectAndBranch <-
    case sourceI of
      Input.BranchSourceI'CurrentContext -> Just . view PP.projectAndBranch_ <$> Cli.getCurrentProjectPath
      Input.BranchSourceI'Empty -> pure Nothing
      Input.BranchSourceI'UnresolvedProjectBranch unresolvedProjectBranch -> do
        pp <- Cli.getCurrentProjectPath
        Just <$> ProjectUtils.resolveProjectBranch (pp ^. #project) (unresolvedProjectBranch & #branch %~ Just)

  _ <- createBranchFromParent (view #branch <$> maySrcProjectAndBranch) destProject newBranchName

  Cli.respond $
    Output.CreatedProjectBranch
      ( case maySrcProjectAndBranch of
          Just sourceBranch ->
            if sourceBranch ^. #project . #projectId == destProject ^. #projectId
              then Output.CreatedProjectBranchFrom'ParentBranch (sourceBranch ^. #branch . #name)
              else Output.CreatedProjectBranchFrom'OtherBranch sourceBranch
          Nothing -> Output.CreatedProjectBranchFrom'Nothingness
      )
      (projectAndBranchNames & #project .~ projectName)

-- | @createBranchFromParent createFrom project branch description@:
--
--   1. Creates a new branch row for @branch@ in project @project@ (failing if @branch@ already exists in @project@).
--   3. Switches to the new branch.
--
-- This bit of functionality is factored out from the main 'handleBranch' handler because it is also called by the
-- @release.draft@ command, which essentially just creates a branch, but with some different output for the user.
--
-- Returns the branch id of the newly-created branch.
createBranchFromParent ::
  -- If no parent branch is provided, make an empty branch.
  Maybe Sqlite.ProjectBranch ->
  Sqlite.Project ->
  ProjectBranchName ->
  Cli ProjectBranchId
createBranchFromParent mayParentBranch project newBranchName = do
  let projectId = project ^. #projectId
  newBranchId <-
    Cli.runTransactionWithRollback \rollback -> do
      Queries.projectBranchExistsByName projectId newBranchName >>= \case
        True -> rollback (Output.ProjectAndBranchNameAlreadyExists (ProjectAndBranch (project ^. #name) newBranchName))
        False -> do
          -- Here, we are forking to `foo/bar`, where project `foo` does exist, and it does not have a branch named
          -- `bar`, so the fork will succeed.
          newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          newBranchCausalHashId <-
            (ProjectBranch.causalHashId <$> mayParentBranch) `whenNothing` do
              (_, causalHashId) <- Codebase.emptyCausalHash
              pure causalHashId
          Queries.insertProjectBranch
            Sqlite.ProjectBranch
              { projectId,
                branchId = newBranchId,
                name = newBranchName,
                parentBranchId = ProjectBranch.branchId <$> mayParentBranch,
                causalHashId = newBranchCausalHashId
              }
          pure newBranchId

  Cli.switchProject (ProjectAndBranch projectId newBranchId)
  pure newBranchId

createBranchFromNamespace :: Sqlite.Project -> Sqlite.Transaction ProjectBranchName -> Branch IO -> Cli ProjectBranchId
createBranchFromNamespace project getBranchName branch = do
  let projectId = project ^. #projectId
  Cli.Env {codebase} <- ask
  let causalHash = Branch.headHash branch
  liftIO $ Codebase.putBranch codebase branch
  newBranchId <-
    Cli.runTransactionWithRollback \rollback -> do
      branchName <- getBranchName
      Queries.projectBranchExistsByName projectId branchName >>= \case
        True -> rollback (Output.ProjectAndBranchNameAlreadyExists (ProjectAndBranch (project ^. #name) branchName))
        False -> do
          newProjectBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          newBranchCausalHashId <- Q.expectCausalHashIdByCausalHash causalHash
          Queries.insertProjectBranch
            Sqlite.ProjectBranch
              { projectId,
                branchId = newProjectBranchId,
                name = branchName,
                parentBranchId = Nothing,
                causalHashId = newBranchCausalHashId
              }
          pure newProjectBranchId

  Cli.switchProject (ProjectAndBranch projectId newBranchId)
  pure newBranchId
