-- | @branch@ input handler
module Unison.Codebase.Editor.HandleInput.Branch
  ( handleBranch,
  )
where

import Control.Lens ((^.))
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.Project as Sqlite
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (getBranchAt, getCurrentPath, updateAt)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Branch as Branch (empty)
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite

data CreateFrom
  = CreateFrom'Branch (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
  | CreateFrom'LooseCode Path.Absolute
  | CreateFrom'Nothingness

-- | Create a new project branch from an existing project branch or namespace.
handleBranch :: Maybe Input.LooseCodeOrProject -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleBranch maybeSource projectAndBranchNames0 = do
  projectAndBranchNames@(ProjectAndBranch projectName newBranchName) <-
    case projectAndBranchNames0 of
      ProjectAndBranch Nothing branchName -> ProjectUtils.hydrateNames (That branchName)
      ProjectAndBranch (Just projectName) branchName -> pure (ProjectAndBranch projectName branchName)

  -- Compute what we should create the branch from.
  createFrom <-
    case maybeSource of
      Nothing ->
        ProjectUtils.getCurrentProjectBranch >>= \case
          Nothing -> CreateFrom'LooseCode <$> Cli.getCurrentPath
          Just currentBranch -> pure (CreateFrom'Branch currentBranch)
      Just (This sourcePath) -> do
        currentPath <- Cli.getCurrentPath
        pure (CreateFrom'LooseCode (Path.resolve currentPath sourcePath))
      Just (That sourceBranch) ->
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
      Just (These _sourcePath sourceBranch) ->
        fmap CreateFrom'Branch do
          ProjectUtils.expectProjectAndBranchByTheseNames
            case sourceBranch of
              ProjectAndBranch Nothing b -> That b
              ProjectAndBranch (Just p) b -> These p b

  (projectId, newBranchId) <-
    Cli.runEitherTransaction do
      Queries.loadProjectByName projectName >>= \case
        -- We can't make the *first* branch of a project with `branch`; the project has to already exist.
        Nothing -> pure (Left (Output.LocalProjectBranchDoesntExist projectAndBranchNames))
        Just project -> do
          let projectId = project ^. #projectId
          Queries.projectBranchExistsByName projectId newBranchName >>= \case
            True -> pure (Left (Output.ProjectAndBranchNameAlreadyExists projectAndBranchNames))
            False ->
              -- Here, we are forking to `foo/bar`, where project `foo` does exist, and it does not have a branch named
              -- `bar`, so the fork will succeed.
              fmap Right do
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
                pure (projectId, newBranchId)

  let newBranchPath = ProjectUtils.projectBranchPath (ProjectAndBranch projectId newBranchId)
  sourceNamespaceObject <-
    case createFrom of
      CreateFrom'Branch (ProjectAndBranch _ sourceBranch) -> do
        let sourceProjectId = sourceBranch ^. #projectId
        let sourceBranchId = sourceBranch ^. #branchId
        Cli.getBranchAt (ProjectUtils.projectBranchPath (ProjectAndBranch sourceProjectId sourceBranchId))
      CreateFrom'LooseCode sourcePath -> Cli.getBranchAt sourcePath
      CreateFrom'Nothingness -> pure Branch.empty
  let description = "branch " <> into @Text (These projectName newBranchName)
  _ <- Cli.updateAt description newBranchPath (const sourceNamespaceObject)
  Cli.respond $
    Output.CreatedProjectBranch
      ( case createFrom of
          CreateFrom'Branch sourceBranch ->
            if sourceBranch ^. #project . #projectId == projectId
              then Output.CreatedProjectBranchFrom'ParentBranch (sourceBranch ^. #branch . #name)
              else Output.CreatedProjectBranchFrom'OtherBranch sourceBranch
          CreateFrom'LooseCode path -> Output.CreatedProjectBranchFrom'LooseCode path
          CreateFrom'Nothingness -> Output.CreatedProjectBranchFrom'Nothingness
      )
      projectAndBranchNames
  Cli.cd newBranchPath
