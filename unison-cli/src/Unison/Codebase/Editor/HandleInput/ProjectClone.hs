-- | @clone@-related input handlers
module Unison.Codebase.Editor.HandleInput.ProjectClone
  ( branchClone,
    projectClone,
  )
where

import Control.Lens (over, (^.))
import Control.Monad.Reader (ask)
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..))
import qualified U.Codebase.Sqlite.Project as Sqlite (Project)
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (projectBranchPath)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.HandleInput.Pull as HandleInput.Pull
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName, projectNameUserSlug)
import qualified Unison.Share.API.Hash as Share.API
import qualified Unison.Share.Sync as Share (downloadEntities)
import qualified Unison.Share.Sync.Types as Share
import qualified Unison.Sqlite as Sqlite
import Unison.Sync.Common (hash32ToCausalHash)
import qualified Unison.Sync.Types as Share
import Witch (unsafeFrom)

-- | Clone a remote branch from the remote project associated with the current branch.
--
-- If there is no associated remote branch, fall back on its nearest ancestor with an associated remote branch, and if
-- there are none, finally fall back on a name comparison (e.g. trying to clone branch "foo" while in local project
-- named "@bar/baz" will look for branch "@bar/baz/foo" on the server)
branchClone :: ProjectBranchName -> Cli ()
branchClone remoteBranchName = do
  -- TODO: allow user to override this with second argument
  let localBranchName = remoteBranchName

  ProjectAndBranch currentProject currentBranch <- ProjectUtils.expectCurrentProjectBranch
  let localProjectBranch = ProjectAndBranch (currentProject ^. #name) localBranchName

  -- The current branch or one of its ancestors may be associated with a remote project already. If that's true,
  -- then try cloning from *that* project. Otherwise, fall back on cloning the remote project whose *name* matches
  -- the local project.
  maybeRemoteProjectInfo <-
    Cli.runEitherTransaction do
      Queries.projectBranchExistsByName (currentProject ^. #projectId) localBranchName >>= \case
        -- We can't clone into local `foo` if local `foo` already exists.
        True -> pure (Left (Output.ProjectAndBranchNameAlreadyExists localProjectBranch))
        -- The current branch or one of its ancestors may be associated with a remote project already. If that's true,
        -- then try cloning from *that* project.
        False -> do
          let currentProjectId = currentProject ^. #projectId
          let currentBranchId = currentBranch ^. #branchId
          Queries.loadRemoteProjectBranch currentProjectId Share.hardCodedUri currentBranchId >>= \case
            -- Neither the current branch nor any of its ancestors are associated with a remote project. Fall back on
            -- cloning the remote project whose *name* matches the local project.
            Nothing -> pure (Right Nothing)
            -- The current branch (or one of its ancestors) is associated with a remote project. Try cloning from that.
            Just (remoteProjectId, _maybeRemoteBranchId) -> do
              remoteProjectName <- Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri
              pure (Right (Just (remoteProjectId, remoteProjectName)))

  remoteProjectBranch <-
    case maybeRemoteProjectInfo of
      Nothing ->
        ProjectUtils.expectRemoteProjectBranchByNames
          (ProjectAndBranch (localProjectBranch ^. #project) remoteBranchName)
      Just remoteProjectInfo ->
        ProjectUtils.expectRemoteProjectBranchByName (ProjectAndBranch remoteProjectInfo remoteBranchName)

  cloneInto "branch.clone" localProjectBranch remoteProjectBranch

-- | Clone a remote project or remote project branch.
projectClone :: ProjectAndBranch ProjectName (Maybe ProjectBranchName) -> Cli ()
projectClone remoteProjectAndBranch0 = do
  let remoteProjectAndBranch = remoteProjectAndBranch0 & over #branch (fromMaybe (unsafeFrom @Text "main"))
  let remoteProjectName = remoteProjectAndBranch ^. #project
  let remoteBranchName = remoteProjectAndBranch ^. #branch
  -- TODO: allow user to override these with second argument
  let localProjectName = remoteProjectName
  let localBranchName = remoteBranchName
  let localProjectBranch = ProjectAndBranch localProjectName localBranchName

  -- Assert that this project name has a user slug
  projectNameUserSlug remoteProjectName & onNothing do
    Cli.returnEarly (Output.ProjectNameRequiresUserSlug remoteProjectName)

  -- Quick local check before hitting share
  void (Cli.runEitherTransaction (assertLocalProjectBranchDoesntExist localProjectBranch))

  -- Get the branch of the given project.
  remoteProjectBranch <- ProjectUtils.expectRemoteProjectBranchByNames remoteProjectAndBranch

  cloneInto "project.clone" localProjectBranch remoteProjectBranch

-- `cloneInto command local remote` clones `remote` into `local`, which is believed to not exist yet, but may (because
-- it takes some time to pull the remote). The `command` argument is used in the reflog to indicate whether this was a
-- `project.clone` or `branch.clone`.
cloneInto :: Text -> ProjectAndBranch ProjectName ProjectBranchName -> Share.RemoteProjectBranch -> Cli ()
cloneInto command localProjectBranch remoteProjectBranch = do
  let remoteProjectName = remoteProjectBranch ^. #projectName
  let remoteBranchName = remoteProjectBranch ^. #branchName

  -- Pull the remote branch's contents
  let remoteBranchHeadJwt = remoteProjectBranch ^. #branchHead
  (result, numDownloaded) <-
    Cli.with HandleInput.Pull.withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
      result <-
        Share.downloadEntities
          Share.hardCodedBaseUrl
          (Share.RepoInfo (into @Text (ProjectAndBranch remoteProjectName remoteBranchName)))
          remoteBranchHeadJwt
          downloadedCallback
      numDownloaded <- liftIO getNumDownloaded
      pure (result, numDownloaded)
  case result of
    Left err0 ->
      (Cli.returnEarly . Output.ShareError) case err0 of
        Share.SyncError err -> Output.ShareErrorDownloadEntities err
        Share.TransportError err -> Output.ShareErrorTransport err
    Right () -> Cli.respond (Output.DownloadedEntities numDownloaded)

  localProjectAndBranch <-
    Cli.runEitherTransaction do
      -- Repeat the check from before, because (although it's highly unlikely) we could have a name conflict after
      -- downloading the remote branch
      assertLocalProjectBranchDoesntExist localProjectBranch >>= \case
        Left err -> pure (Left err)
        Right maybeLocalProject -> do
          -- Create the local project (if necessary), and create the local branch
          localProjectId <-
            case maybeLocalProject of
              Nothing -> do
                localProjectId <- Sqlite.unsafeIO (ProjectId <$> UUID.nextRandom)
                Queries.insertProject localProjectId (localProjectBranch ^. #project)
                pure localProjectId
              Just localProject -> pure (localProject ^. #projectId)
          localBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch
            Sqlite.ProjectBranch
              { projectId = localProjectId,
                branchId = localBranchId,
                name = localProjectBranch ^. #branch,
                parentBranchId = Nothing
              }
          Queries.insertBranchRemoteMapping
            localProjectId
            localBranchId
            (remoteProjectBranch ^. #projectId)
            Share.hardCodedUri
            (remoteProjectBranch ^. #branchId)
          pure (Right (ProjectAndBranch localProjectId localBranchId))

  -- Manipulate the root namespace and cd
  Cli.Env {codebase} <- ask
  let branchHead = hash32ToCausalHash (Share.API.hashJWTHash remoteBranchHeadJwt)
  theBranch <- liftIO (Codebase.expectBranchForHash codebase branchHead)
  let path = projectBranchPath localProjectAndBranch
  Cli.stepAt
    (command <> " " <> into @Text (ProjectAndBranch remoteProjectName remoteBranchName))
    (Path.unabsolute path, const (Branch.head theBranch))
  Cli.cd path

-- Assert that a local project+branch with this name doesn't already exist. If it does exist, we can't clone over it.
assertLocalProjectBranchDoesntExist ::
  ProjectAndBranch ProjectName ProjectBranchName ->
  Sqlite.Transaction (Either Output.Output (Maybe Sqlite.Project))
assertLocalProjectBranchDoesntExist projectBranch =
  Queries.loadProjectByName (projectBranch ^. #project) >>= \case
    Nothing -> pure (Right Nothing)
    Just project ->
      Queries.projectBranchExistsByName (project ^. #projectId) (projectBranch ^. #branch) <&> \case
        False -> Right (Just project)
        True -> Left (Output.ProjectAndBranchNameAlreadyExists projectBranch)
