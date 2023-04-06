-- | @branch@ input handler
module Unison.Codebase.Editor.HandleInput.Branch
  ( handleBranch,
  )
where

import Control.Lens (view, (^.))
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (getCurrentBranch, getCurrentPath, updateAt)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite

data CreatedFrom
  = CreatedFrom'LooseCode
  | CreatedFrom'Nothingness
  | CreatedFrom'OtherBranch (ProjectAndBranch ProjectName ProjectBranchName)
  | CreatedFrom'ParentBranch Sqlite.ProjectBranch

-- | Create a new project branch from an existing project branch or namespace.
handleBranch :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleBranch projectAndBranchNames0 = do
  projectAndBranchNames@(ProjectAndBranch projectName newBranchName) <-
    case projectAndBranchNames0 of
      ProjectAndBranch Nothing branchName -> ProjectUtils.hydrateNames (That branchName)
      ProjectAndBranch (Just projectName) branchName -> pure (ProjectAndBranch projectName branchName)

  maybeCurrentProjectBranch <- ProjectUtils.getCurrentProjectBranch

  (projectId, newBranchId, createdFrom) <-
    Cli.runEitherTransaction do
      Queries.loadProjectByName projectName >>= \case
        Nothing -> pure (Left (Output.LocalProjectBranchDoesntExist projectAndBranchNames))
        Just project -> do
          let projectId = project ^. #projectId
          Queries.projectBranchExistsByName projectId newBranchName >>= \case
            True -> pure (Left (Output.ProjectAndBranchNameAlreadyExists projectAndBranchNames))
            False ->
              -- Here, we are forking to `foo/bar`, where project `foo` does exist, and it does not have a branch named
              -- `bar`, so the fork will succeed.
              --
              -- If we are currently on a different branch of `foo`, we record the current branch as the parent of
              -- `bar`.
              fmap Right do
                newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
                createdFrom <-
                  case maybeCurrentProjectBranch of
                    Nothing -> pure CreatedFrom'LooseCode
                    Just (ProjectAndBranch currentProject currentBranch) ->
                      pure
                        if projectId == currentProject ^. #projectId
                          then CreatedFrom'ParentBranch currentBranch
                          else
                            CreatedFrom'OtherBranch
                              (ProjectAndBranch (currentProject ^. #name) (currentBranch ^. #name))
                Queries.insertProjectBranch
                  Sqlite.ProjectBranch
                    { projectId,
                      branchId = newBranchId,
                      name = newBranchName,
                      parentBranchId =
                        case createdFrom of
                          CreatedFrom'ParentBranch parentBranch -> Just (view #branchId parentBranch)
                          CreatedFrom'LooseCode {} -> Nothing
                          CreatedFrom'Nothingness -> Nothing
                          CreatedFrom'OtherBranch {} -> Nothing
                    }
                pure (projectId, newBranchId, createdFrom)

  let path = ProjectUtils.projectBranchPath (ProjectAndBranch projectId newBranchId)
  currentNamespaceObject <- Cli.getCurrentBranch
  let description = "branch " <> into @Text (These projectName newBranchName)
  _ <- Cli.updateAt description path (const currentNamespaceObject)
  createdFrom1 <-
    case createdFrom of
      CreatedFrom'LooseCode -> Output.CreatedProjectBranchFrom'LooseCode <$> Cli.getCurrentPath
      CreatedFrom'Nothingness -> pure Output.CreatedProjectBranchFrom'Nothingness
      CreatedFrom'OtherBranch otherBranch -> pure (Output.CreatedProjectBranchFrom'OtherBranch otherBranch)
      CreatedFrom'ParentBranch parentBranch ->
        pure (Output.CreatedProjectBranchFrom'ParentBranch (parentBranch ^. #name))
  Cli.respond (Output.CreatedProjectBranch createdFrom1 newBranchName)
  Cli.cd path
