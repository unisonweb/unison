-- | @project.clone@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectClone
  ( projectClone,
  )
where

import Control.Lens ((^.))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..), RemoteProjectId (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import Unison.Cli.ProjectUtils (loggeth)
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectName)
import qualified Unison.Share.API.Projects as Share.API
import Witch (unsafeFrom)

-- | Clone a remote project.
projectClone :: ProjectName -> Cli ()
projectClone projectName = do
  -- Quick local check before hitting share to determine whether this project already exists.
  Cli.runEitherTransaction do
    Queries.projectExistsByName (into @Text projectName) <&> \case
      False -> Right ()
      True -> Left (Output.ProjectNameAlreadyExists projectName)

  -- Get the "main" branch of the given project.
  remoteProjectBranch <- do
    project <-
      Share.getProjectByName projectName >>= \case
        Share.API.GetProjectResponseNotFound notFound -> do
          loggeth ["remote project doesn't exist: ", tShow notFound]
          Cli.returnEarlyWithoutOutput
        Share.API.GetProjectResponseUnauthorized unauthorized -> do
          loggeth ["unauthorized: ", tShow unauthorized]
          Cli.returnEarlyWithoutOutput
        Share.API.GetProjectResponseSuccess project -> pure project
    let remoteProjectId = RemoteProjectId (project ^. #projectId)
    let remoteBranchName = unsafeFrom @Text "main"
    Share.getProjectBranchByName (ProjectAndBranch remoteProjectId remoteBranchName) >>= \case
      Share.API.GetProjectBranchResponseNotFound notFound -> do
        loggeth ["remote branch 'main' doesn't exist: ", tShow notFound]
        Cli.returnEarlyWithoutOutput
      Share.API.GetProjectBranchResponseUnauthorized unauthorized -> do
        loggeth ["unauthorized: ", tShow unauthorized]
        Cli.returnEarlyWithoutOutput
      Share.API.GetProjectBranchResponseSuccess projectBranch -> pure projectBranch

  -- Pull the remote branch's contents
  -- TODO

  -- Create the local project and branch
  localProjectId <- liftIO (ProjectId <$> UUID.nextRandom)
  localBranchId <- liftIO (ProjectBranchId <$> UUID.nextRandom)
  Cli.runEitherTransaction do
    Queries.projectExistsByName (into @Text projectName) >>= \case
      False -> do
        Queries.insertProject localProjectId (into @Text projectName)
        Queries.insertProjectBranch localProjectId localBranchId "main"
        pure (Right ())
      True -> pure (Left (Output.ProjectNameAlreadyExists projectName))

  -- Manipulate the root namespace and cd

  -- let path = projectBranchPath ProjectAndBranch {project = projectId, branch = branchId}
  -- Cli.stepAt "project.create" (Path.unabsolute path, const mainBranchContents)
  -- Cli.cd path
  
  wundefined
