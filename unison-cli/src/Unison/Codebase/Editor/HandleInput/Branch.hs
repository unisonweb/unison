-- | @branch@ input handler
module Unison.Codebase.Editor.HandleInput.Branch
  ( handleBranch,
    CreateFrom (..),
    doCreateBranch,
  )
where

import Control.Lens ((^.))
import Data.These (These (..))
import Data.UUID.V4 qualified as UUID
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli (getBranchAt, getCurrentPath, updateAt)
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Branch qualified as Branch (empty)
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectBranchNameKind (..), ProjectName, classifyProjectBranchName)
import Unison.Sqlite qualified as Sqlite

data CreateFrom
  = CreateFrom'Branch (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
  | CreateFrom'LooseCode Path.Absolute
  | CreateFrom'Nothingness

-- | Create a new project branch from an existing project branch or namespace.
handleBranch :: Input.BranchSourceI -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleBranch sourceI projectAndBranchNames0 = do
  projectAndBranchNames@(ProjectAndBranch projectName newBranchName) <-
    case projectAndBranchNames0 of
      ProjectAndBranch Nothing branchName -> ProjectUtils.hydrateNames (That branchName)
      ProjectAndBranch (Just projectName) branchName -> pure (ProjectAndBranch projectName branchName)

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

  -- Compute what we should create the branch from.
  createFrom <-
    case sourceI of
      Input.BranchSourceI'CurrentContext ->
        ProjectUtils.getCurrentProjectBranch >>= \case
          Nothing -> CreateFrom'LooseCode <$> Cli.getCurrentPath
          Just (currentBranch, _restPath) -> pure (CreateFrom'Branch currentBranch)
      Input.BranchSourceI'Empty -> pure CreateFrom'Nothingness
      Input.BranchSourceI'LooseCodeOrProject (This sourcePath) -> do
        currentPath <- Cli.getCurrentPath
        pure (CreateFrom'LooseCode (Path.resolve currentPath sourcePath))
      Input.BranchSourceI'LooseCodeOrProject (That sourceBranch) ->
        fmap CreateFrom'Branch do
          ProjectUtils.expectProjectAndBranchByTheseNames
            case sourceBranch of
              ProjectAndBranch Nothing b -> That b
              ProjectAndBranch (Just p) b -> These p b
      -- For now, treat ambiguous parses as branch names, as this seems (far) more common than trying to create a
      -- branch from a relative one-segment namespace.
      --
      -- Future work: be smarter; for example, if there is such a relative namespace, but no such branch, maybe they
      -- really meant create a branch from that namespace.
      Input.BranchSourceI'LooseCodeOrProject (These _sourcePath sourceBranch) ->
        fmap CreateFrom'Branch do
          ProjectUtils.expectProjectAndBranchByTheseNames
            case sourceBranch of
              ProjectAndBranch Nothing b -> That b
              ProjectAndBranch (Just p) b -> These p b

  project <-
    Cli.runTransactionWithRollback \rollback -> do
      Queries.loadProjectByName projectName & onNothingM do
        -- We can't make the *first* branch of a project with `branch`; the project has to already exist.
        rollback (Output.LocalProjectBranchDoesntExist projectAndBranchNames)

  doCreateBranch createFrom project newBranchName ("branch " <> into @Text projectAndBranchNames)

  Cli.respond $
    Output.CreatedProjectBranch
      ( case createFrom of
          CreateFrom'Branch sourceBranch ->
            if sourceBranch ^. #project . #projectId == project ^. #projectId
              then Output.CreatedProjectBranchFrom'ParentBranch (sourceBranch ^. #branch . #name)
              else Output.CreatedProjectBranchFrom'OtherBranch sourceBranch
          CreateFrom'LooseCode path -> Output.CreatedProjectBranchFrom'LooseCode path
          CreateFrom'Nothingness -> Output.CreatedProjectBranchFrom'Nothingness
      )
      projectAndBranchNames

-- | @doCreateBranch createFrom project branch description@:
--
--   1. Creates a new branch row for @branch@ in project @project@ (failing if @branch@ already exists in @project@)
--   2. Puts the branch contents from @createFrom@ in the root namespace., using @description@ for the reflog.
--   3. cds to the new branch in the root namespace.
--
-- This bit of functionality is factored out from the main 'handleBranch' handler because it is also called by the
-- @release.draft@ command, which essentially just creates a branch, but with some different output for the user.
doCreateBranch :: CreateFrom -> Sqlite.Project -> ProjectBranchName -> Text -> Cli ()
doCreateBranch createFrom project newBranchName description = do
  let projectId = project ^. #projectId
  newBranchId <-
    Cli.runTransactionWithRollback \rollback -> do
      Queries.projectBranchExistsByName projectId newBranchName >>= \case
        True -> rollback (Output.ProjectAndBranchNameAlreadyExists (ProjectAndBranch (project ^. #name) newBranchName))
        False -> do
          -- Here, we are forking to `foo/bar`, where project `foo` does exist, and it does not have a branch named
          -- `bar`, so the fork will succeed.
          newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch
            Sqlite.ProjectBranch
              { projectId,
                branchId = newBranchId,
                name = newBranchName,
                parentBranchId =
                  -- If we creating the branch from another branch in the same project, mark its parent
                  case createFrom of
                    CreateFrom'Branch (ProjectAndBranch _ sourceBranch)
                      | (sourceBranch ^. #projectId) == projectId -> Just (sourceBranch ^. #branchId)
                    _ -> Nothing
              }
          Queries.setMostRecentBranch projectId newBranchId
          pure newBranchId

  let newBranchPath = ProjectUtils.projectBranchPath (ProjectAndBranch projectId newBranchId)
  sourceNamespaceObject <-
    case createFrom of
      CreateFrom'Branch (ProjectAndBranch _ sourceBranch) -> do
        let sourceProjectId = sourceBranch ^. #projectId
        let sourceBranchId = sourceBranch ^. #branchId
        Cli.getBranchAt (ProjectUtils.projectBranchPath (ProjectAndBranch sourceProjectId sourceBranchId))
      CreateFrom'LooseCode sourcePath -> Cli.getBranchAt sourcePath
      CreateFrom'Nothingness -> pure Branch.empty
  _ <- Cli.updateAt description newBranchPath (const sourceNamespaceObject)
  Cli.cd newBranchPath
