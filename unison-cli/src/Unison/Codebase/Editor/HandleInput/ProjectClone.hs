-- | @project.clone@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectClone
  ( projectClone,
  )
where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..), RemoteProjectId (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (loggeth, projectBranchPath)
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.HandleInput.Pull as HandleInput.Pull
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectName, projectNameUserSlug)
import qualified Unison.Share.API.Projects as Share.API
import qualified Unison.Share.Sync as Share (downloadEntities)
import Unison.Sync.Common (hash32ToCausalHash)
import qualified Unison.Sync.Types as Share (RepoName (..), hashJWTHash)
import Witch (unsafeFrom)

-- | Clone a remote project.
projectClone :: ProjectName -> Cli ()
projectClone projectName = do
  -- Assert that this project name has a user slug
  projectUserSlug <-
    case projectNameUserSlug projectName of
      Just slug -> pure slug
      Nothing -> do
        loggeth ["can't clone project without user slug"]
        Cli.returnEarlyWithoutOutput

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
      Share.API.GetProjectBranchResponseBranchNotFound notFound -> do
        loggeth ["remote branch 'main' doesn't exist: ", tShow notFound]
        Cli.returnEarlyWithoutOutput
      Share.API.GetProjectBranchResponseProjectNotFound notFound -> do
        loggeth ["project doesn't exist: ", tShow notFound]
        Cli.returnEarlyWithoutOutput
      Share.API.GetProjectBranchResponseUnauthorized unauthorized -> do
        loggeth ["unauthorized: ", tShow unauthorized]
        Cli.returnEarlyWithoutOutput
      Share.API.GetProjectBranchResponseSuccess projectBranch -> pure projectBranch

  -- FIXME remote project branch should have HashJWT
  let remoteBranchHeadJwt = wundefined (remoteProjectBranch ^. #branchHead)
  let remoteBranchHead = Share.hashJWTHash remoteBranchHeadJwt

  -- Pull the remote branch's contents
  Cli.with HandleInput.Pull.withEntitiesDownloadedProgressCallback \downloadedCallback -> do
    let download =
          Share.downloadEntities
            Share.hardCodedBaseUrl
            (Share.RepoName projectUserSlug)
            remoteBranchHeadJwt
            downloadedCallback
    download >>= \case
      Left err -> do
        loggeth ["download entities error: ", tShow err]
        Cli.returnEarlyWithoutOutput
      Right () -> pure ()

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
  Cli.Env {codebase} <- ask
  theBranch <- liftIO (Codebase.expectBranchForHash codebase (hash32ToCausalHash remoteBranchHead))
  let path = projectBranchPath (ProjectAndBranch localProjectId localBranchId)
  Cli.stepAt "project.clone" (Path.unabsolute path, const (Branch.head theBranch))
  Cli.cd path
