-- | @push@ input handler
module Unison.Codebase.Editor.HandleInput.Push where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Set.NonEmpty as Set.NonEmpty
import Data.Text as Text
import qualified System.Console.Regions as Console.Regions
import U.Codebase.HashTags (CausalHash (..))
import qualified U.Codebase.Sqlite.Operations as Operations
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, loggeth, projectBranchPath)
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Cli.UnisonConfigUtils as UnisonConfigUtils
import Unison.Codebase (PushGitBranchOpts (..))
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.HandleInput.AuthLogin (ensureAuthenticatedWithCodeserver)
import qualified Unison.Codebase.Editor.HandleInput.AuthLogin as AuthLogin
import Unison.Codebase.Editor.Input (GistInput (..), PushRemoteBranchInput (..), PushSource (..), PushSourceTarget (..), PushTarget (..))
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.Output.PushPull (PushPull (Push))
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadRemoteNamespace (..),
    ShareUserHandle (..),
    WriteGitRemotePath (..),
    WriteRemotePath (..),
    WriteShareRemotePath (..),
    writeToReadGit,
  )
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import qualified Unison.Codebase.PushBehavior as PushBehavior
import qualified Unison.Codebase.ShortCausalHash as SCH
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.Type (GitPushBehavior (..))
import qualified Unison.Hash as Hash
import Unison.Hash32 (Hash32)
import qualified Unison.Hash32 as Hash32
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectBranchName,
    ProjectName,
    prependUserSlugToProjectBranchName,
    prependUserSlugToProjectName,
    projectBranchNameUserSlug,
    projectNameUserSlug,
  )
import qualified Unison.Share.API.Projects as Share.API
import qualified Unison.Share.Codeserver as Codeserver
import qualified Unison.Share.Sync as Share
import qualified Unison.Share.Sync.Types as Share
import Unison.Share.Types (codeserverBaseURL)
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sync.Types as Share
import Witch (unsafeFrom)

-- | Handle a @gist@ command.
handleGist :: GistInput -> Cli ()
handleGist (GistInput repo) = do
  Cli.Env {codebase} <- ask
  sourceBranch <- Cli.getCurrentBranch
  let opts =
        PushGitBranchOpts
          { behavior = GitPushBehaviorGist,
            syncMode = SyncMode.ShortCircuit
          }
  result <-
    Cli.ioE (Codebase.pushGitBranch codebase repo opts (\_remoteRoot -> pure (Right sourceBranch))) \err ->
      Cli.returnEarly (Output.GitError err)
  _branch <- result & onLeft Cli.returnEarly
  schLength <- Cli.runTransaction Codebase.branchHashLength
  Cli.respond $
    GistCreated $
      ReadRemoteNamespaceGit
        ReadGitRemoteNamespace
          { repo = writeToReadGit repo,
            sch = Just (SCH.fromHash schLength (Branch.headHash sourceBranch)),
            path = Path.empty
          }

-- | Handle a @push@ command.
handlePushRemoteBranch :: PushRemoteBranchInput -> Cli ()
handlePushRemoteBranch PushRemoteBranchInput {sourceTarget, pushBehavior, syncMode} = do
  case sourceTarget of
    -- push <implicit> to <implicit>
    PushSourceTarget0 ->
      getCurrentProjectBranch >>= \case
        Nothing -> do
          localPath <- Cli.getCurrentPath
          remotePath <- UnisonConfigUtils.resolveConfiguredUrl Push Path.currentPath
          pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode
        Just localProjectAndBranch -> pushProjectBranchToProjectBranch localProjectAndBranch Nothing
    -- push <implicit> to .some.path
    PushSourceTarget1 (PathyTarget remotePath) -> do
      localPath <- Cli.getCurrentPath
      pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode
    -- push <implicit> to @some/project
    PushSourceTarget1 (ProjyTarget remoteProjectAndBranch0) ->
      getCurrentProjectBranch >>= \case
        Nothing -> do
          localPath <- Cli.getCurrentPath
          remoteProjectAndBranch <- resolveProjectAndBranchMaybeNamesToNames remoteProjectAndBranch0
          pushLooseCodeToProjectBranch localPath remoteProjectAndBranch
        Just localProjectAndBranch ->
          pushProjectBranchToProjectBranch localProjectAndBranch (Just remoteProjectAndBranch0)
    -- push .some.path to .some.path
    PushSourceTarget2 (PathySource localPath0) (PathyTarget remotePath) -> do
      localPath <- Cli.resolvePath' localPath0
      pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode
    -- push .some.path to @some/project
    PushSourceTarget2 (PathySource localPath0) (ProjyTarget remoteProjectAndBranch0) -> do
      localPath <- Cli.resolvePath' localPath0
      remoteProjectAndBranch <- resolveProjectAndBranchMaybeNamesToNames remoteProjectAndBranch0
      pushLooseCodeToProjectBranch localPath remoteProjectAndBranch
    -- push @some/project to .some.path
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (PathyTarget remotePath) -> do
      localProjectAndBranch <- resolveProjectAndBranchMaybeNamesToIds localProjectAndBranch0
      pushLooseCodeToLooseCode (projectBranchPath localProjectAndBranch) remotePath pushBehavior syncMode
    -- push @some/project to @some/project
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (ProjyTarget remoteProjectAndBranch) -> do
      localProjectAndBranch <- resolveProjectAndBranchMaybeNamesToIds localProjectAndBranch0
      pushProjectBranchToProjectBranch localProjectAndBranch (Just remoteProjectAndBranch)

resolveProjectAndBranchMaybeNamesToIds ::
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli (ProjectAndBranch Queries.ProjectId Queries.BranchId)
resolveProjectAndBranchMaybeNamesToIds = \case
  ProjectAndBranch Nothing Nothing ->
    getCurrentProjectBranch >>= \case
      Nothing -> do
        loggeth ["not on a project branch yo"]
        Cli.returnEarlyWithoutOutput
      Just projectAndBranch -> pure projectAndBranch
  ProjectAndBranch Nothing (Just branchName) ->
    getCurrentProjectBranch >>= \case
      Nothing -> do
        loggeth ["not on a project branch yo"]
        Cli.returnEarlyWithoutOutput
      Just (ProjectAndBranch projectId _branchId) ->
        Cli.runTransaction (Queries.loadProjectBranchByName projectId (into @Text branchName)) >>= \case
          Nothing -> do
            loggeth ["no branch in project ", tShow projectId, " with name ", into @Text branchName]
            Cli.returnEarlyWithoutOutput
          Just branch -> pure (ProjectAndBranch projectId (branch ^. #branchId))
  ProjectAndBranch (Just projectName) maybeBranchName -> do
    let branchName = fromMaybe (unsafeFrom @Text "main") maybeBranchName
    maybeProjectAndBranch <-
      Cli.runTransaction do
        runMaybeT do
          project <- MaybeT (Queries.loadProjectByName (into @Text projectName))
          let projectId = project ^. #projectId
          branch <- MaybeT (Queries.loadProjectBranchByName projectId (into @Text branchName))
          pure (ProjectAndBranch projectId (branch ^. #branchId))
    case maybeProjectAndBranch of
      Nothing -> do
        loggeth [into @Text (ProjectAndBranch (Just projectName) (Just branchName)), " not found!"]
        Cli.returnEarlyWithoutOutput
      Just projectAndBranch -> pure projectAndBranch

resolveProjectAndBranchMaybeNamesToNames ::
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli (ProjectAndBranch ProjectName ProjectBranchName)
resolveProjectAndBranchMaybeNamesToNames = \case
  ProjectAndBranch Nothing Nothing ->
    getCurrentProjectBranch >>= \case
      Nothing -> do
        loggeth ["not on a project branch"]
        Cli.returnEarlyWithoutOutput
      Just (ProjectAndBranch projectId branchId) ->
        Cli.runTransaction do
          project <- Queries.expectProject projectId
          branch <- Queries.expectProjectBranch projectId branchId
          pure (ProjectAndBranch (unsafeFrom @Text (project ^. #name)) (unsafeFrom @Text (branch ^. #name)))
  ProjectAndBranch Nothing (Just branchName) -> do
    getCurrentProjectBranch >>= \case
      Nothing -> do
        loggeth ["not on a project branch"]
        Cli.returnEarlyWithoutOutput
      Just (ProjectAndBranch projectId _branchId) ->
        Cli.runTransaction do
          project <- Queries.expectProject projectId
          pure (ProjectAndBranch (unsafeFrom @Text (project ^. #name)) branchName)
  ProjectAndBranch (Just projectName) Nothing -> pure (ProjectAndBranch projectName (unsafeFrom @Text "main"))
  ProjectAndBranch (Just projectName) (Just branchName) -> pure (ProjectAndBranch projectName branchName)

-- | Push a local namespace ("loose code") to a remote namespace ("loose code").
pushLooseCodeToLooseCode ::
  Path.Absolute ->
  WriteRemotePath ->
  PushBehavior ->
  SyncMode ->
  Cli ()
pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode = do
  case remotePath of
    WriteRemotePathGit gitRemotePath -> pushLooseCodeToGitLooseCode localPath gitRemotePath pushBehavior syncMode
    WriteRemotePathShare shareRemotePath -> pushLooseCodeToShareLooseCode localPath shareRemotePath pushBehavior

-- | Push a local namespace ("loose code") to a Git-hosted remote namespace ("loose code").
pushLooseCodeToGitLooseCode ::
  Path.Absolute ->
  WriteGitRemotePath ->
  PushBehavior ->
  SyncMode ->
  Cli ()
pushLooseCodeToGitLooseCode localPath gitRemotePath pushBehavior syncMode = do
  sourceBranch <- Cli.getBranchAt localPath
  let withRemoteRoot :: Branch IO -> Either Output (Branch IO)
      withRemoteRoot remoteRoot = do
        let -- We don't merge `sourceBranch` with `remoteBranch`, we just replace it. This push will be rejected if
            -- this rewinds time or misses any new updates in the remote branch that aren't in `sourceBranch`
            -- already.
            f remoteBranch = if shouldPushTo pushBehavior remoteBranch then Just sourceBranch else Nothing
        case Branch.modifyAtM (gitRemotePath ^. #path) f remoteRoot of
          Nothing -> Left (RefusedToPush pushBehavior (WriteRemotePathGit gitRemotePath))
          Just newRemoteRoot -> Right newRemoteRoot
  let opts =
        PushGitBranchOpts
          { behavior =
              case pushBehavior of
                PushBehavior.ForcePush -> GitPushBehaviorForce
                PushBehavior.RequireEmpty -> GitPushBehaviorFf
                PushBehavior.RequireNonEmpty -> GitPushBehaviorFf,
            syncMode
          }
  Cli.Env {codebase} <- ask
  let push =
        Codebase.pushGitBranch
          codebase
          (gitRemotePath ^. #repo)
          opts
          (\remoteRoot -> pure (withRemoteRoot remoteRoot))
  result <-
    liftIO push & onLeftM \err ->
      Cli.returnEarly (Output.GitError err)
  _branch <- result & onLeft Cli.returnEarly
  Cli.respond Success
  where
    -- Per `pushBehavior`, we are either:
    --
    --   (1) force-pushing, in which case the remote branch state doesn't matter
    --   (2) updating an empty branch, which fails if the branch isn't empty (`push.create`)
    --   (3) updating a non-empty branch, which fails if the branch is empty (`push`)
    shouldPushTo :: PushBehavior -> Branch m -> Bool
    shouldPushTo pushBehavior remoteBranch =
      case pushBehavior of
        PushBehavior.ForcePush -> True
        PushBehavior.RequireEmpty -> Branch.isEmpty0 (Branch.head remoteBranch)
        PushBehavior.RequireNonEmpty -> not (Branch.isEmpty0 (Branch.head remoteBranch))

-- | Push a local namespace ("loose code") to a Share-hosted remote namespace ("loose code").
pushLooseCodeToShareLooseCode :: Path.Absolute -> WriteShareRemotePath -> PushBehavior -> Cli ()
pushLooseCodeToShareLooseCode localPath remote@WriteShareRemotePath {server, repo, path = remotePath} behavior = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  let sharePath = Share.Path (shareUserHandleToText repo Nel.:| pathToSegments remotePath)
  ensureAuthenticatedWithCodeserver codeserver

  localCausalHash <-
    Cli.runTransaction (Ops.loadCausalHashAtPath (pathToSegments (Path.unabsolute localPath))) & onNothingM do
      Cli.returnEarly (EmptyPush . Path.absoluteToPath' $ localPath)

  let checkAndSetPush :: Maybe Hash32 -> Cli ()
      checkAndSetPush remoteHash =
        when (Just (Hash32.fromHash (unCausalHash localCausalHash)) /= remoteHash) do
          let push =
                Cli.with withEntitiesUploadedProgressCallback \uploadedCallback -> do
                  Share.checkAndSetPush
                    baseURL
                    sharePath
                    remoteHash
                    localCausalHash
                    uploadedCallback
          push & onLeftM (pushError ShareErrorCheckAndSetPush)

  case behavior of
    PushBehavior.ForcePush -> do
      maybeHashJwt <-
        Share.getCausalHashByPath baseURL sharePath & onLeftM \err0 ->
          (Cli.returnEarly . Output.ShareError) case err0 of
            Share.SyncError err -> ShareErrorGetCausalHashByPath err
            Share.TransportError err -> ShareErrorTransport err
      checkAndSetPush (Share.hashJWTHash <$> maybeHashJwt)
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireEmpty -> do
      checkAndSetPush Nothing
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireNonEmpty -> do
      let push :: Cli (Either (Share.SyncError Share.FastForwardPushError) ())
          push =
            Cli.with withEntitiesUploadedProgressCallback \uploadedCallback ->
              Share.fastForwardPush
                baseURL
                sharePath
                localCausalHash
                uploadedCallback
      push & onLeftM (pushError ShareErrorFastForwardPush)
      Cli.respond (ViewOnShare remote)
  where
    pathToSegments :: Path -> [Text]
    pathToSegments =
      coerce Path.toList

    pushError :: (a -> Output.ShareError) -> Share.SyncError a -> Cli b
    pushError f err0 = do
      Cli.returnEarly case err0 of
        Share.SyncError err -> Output.ShareError (f err)
        Share.TransportError err -> Output.ShareError (ShareErrorTransport err)

-- | Push a local namespace ("loose code") to a remote project branch.
pushLooseCodeToProjectBranch ::
  Path.Absolute ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli ()
pushLooseCodeToProjectBranch localPath remoteProjectAndBranch = wundefined

-- | Push a local project branch to a remote project branch. If the remote project branch is left unspecified, we either
-- use a pre-existing mapping for the local branch, or else infer what remote branch to push to (possibly creating it).
pushProjectBranchToProjectBranch ::
  ProjectAndBranch Queries.ProjectId Queries.BranchId ->
  Maybe (ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName)) ->
  Cli ()
pushProjectBranchToProjectBranch localProjectAndBranchIds maybeRemoteProjectAndBranchNames = do
  -- Load local project and branch from database
  localProjectAndBranch <-
    Cli.runTransaction (expectProjectAndBranch localProjectAndBranchIds)

  localBranchCausalHash <- do
    let path = projectBranchPath localProjectAndBranchIds
    let segments = coerce @[NameSegment] @[Text] (Path.toList (Path.unabsolute path))
    Cli.runTransaction (Operations.loadCausalHashAtPath segments) & onNothingM do
      Cli.returnEarly (EmptyPush (Path.absoluteToPath' path))
  let localBranchCausalHash32 = Hash32.fromHash (unCausalHash localBranchCausalHash)

  -- Get two pieces of information that are computed in various ways depending on whether the user has specified a
  -- target or not, the state of the database, etc:
  --
  --   repoName
  --     The "repo name" to upload to in the upload-entities request. For a contributor branch, for example, this will
  --     be the username of the contributor. This action might short-circuit, as in the case when
  --
  --   afterUpload
  --     An action invoked after successfully uploading branch contents.
  (repoName, afterUpload) <-
    case maybeRemoteProjectAndBranchNames of
      Nothing ->
        Cli.runTransaction oinkResolveRemoteIds >>= \case
          Nothing -> bazinga1 localProjectAndBranch localBranchCausalHash32
          Just (ProjectAndBranch remoteProjectId maybeRemoteBranchId) ->
            case maybeRemoteBranchId of
              Nothing -> bazinga2 (localProjectAndBranch ^. #branch) localBranchCausalHash32 remoteProjectId
              Just remoteBranchId -> bazinga3 (ProjectAndBranch remoteProjectId remoteBranchId)
      Just remoteProjectAndBranchNames -> bazinga4 localProjectAndBranch remoteProjectAndBranchNames

  oinkUpload (Share.RepoName repoName) localBranchCausalHash32
  afterUpload

bazinga1 :: ProjectAndBranch Queries.Project Queries.Branch -> Hash32 -> Cli (Text, Cli ())
bazinga1 (ProjectAndBranch localProject localBranch) localBranchCausalHash32 = do
  myUserHandle <- oinkGetLoggedInUser

  -- Derive the remote project name from the user's handle and the local project name.
  --
  -- Example 1 - user "bob" has local project "foo"
  --   remoteProjectName = "@bob/foo"
  --
  -- Example 2 - user "bob" has local project "@unison/base"
  --   remoteProjectName = "@unison/base"
  let remoteProjectName =
        let localProjectName = unsafeFrom @Text (localProject ^. #name)
         in prependUserSlugToProjectName myUserHandle localProjectName

  let (remoteBranchUserSlug, remoteBranchName) =
        deriveRemoteBranchName myUserHandle (unsafeFrom @Text (localBranch ^. #name))

  let afterUpload = do
        remoteProject <- oinkCreateRemoteProject (into @Text remoteProjectName)
        remoteBranch <-
          oinkCreateRemoteBranch
            Share.API.CreateProjectBranchRequest
              { projectId = remoteProject ^. #projectId,
                branchName = into @Text remoteBranchName,
                branchCausalHash = localBranchCausalHash32,
                branchMergeTarget = Nothing
              }
        pure ()

  Share.getProjectByName (into @Text remoteProjectName) >>= \case
    Share.API.GetProjectResponseNotFound -> pure ()
    Share.API.GetProjectResponseSuccess remoteProject -> do
      Share.getProjectBranchByName (remoteProject ^. #projectId) (into @Text remoteBranchName) >>= \case
        Share.API.GetProjectBranchResponseNotFound -> pure ()
        Share.API.GetProjectBranchResponseSuccess remoteBranch ->
          -- TODO don't proceed with push if local hash not ahead of
          wundefined

  pure (remoteBranchUserSlug, afterUpload)

bazinga2 :: Queries.Branch -> Hash32 -> Text -> Cli (Text, Cli ())
bazinga2 localBranch localBranchCausalHash32 remoteProjectId = do
  myUserHandle <- oinkGetLoggedInUser

  let (remoteBranchUserSlug, remoteBranchName) =
        deriveRemoteBranchName myUserHandle (unsafeFrom @Text (localBranch ^. #name))

  let afterUpload = do
        remoteBranch <-
          oinkCreateRemoteBranch
            Share.API.CreateProjectBranchRequest
              { projectId = remoteProjectId,
                branchName = into @Text remoteBranchName,
                branchCausalHash = localBranchCausalHash32,
                branchMergeTarget = wundefined
              }
        pure ()

  Share.getProjectById remoteProjectId >>= \case
    Share.API.GetProjectResponseNotFound -> pure ()
    Share.API.GetProjectResponseSuccess remoteProject -> do
      Share.getProjectBranchByName (remoteProject ^. #projectId) (into @Text remoteBranchName) >>= \case
        Share.API.GetProjectBranchResponseNotFound -> pure ()
        Share.API.GetProjectBranchResponseSuccess remoteBranch ->
          -- TODO don't proceed with push if local hash not ahead of
          wundefined

  pure (remoteBranchUserSlug, afterUpload)

bazinga3 :: ProjectAndBranch Text Text -> Cli (Text, Cli ())
bazinga3 (ProjectAndBranch remoteProjectId remoteBranchId) = do
  loggeth ["getProjectBranchById ", remoteProjectId, " ", remoteBranchId]
  remoteBranch <-
    Share.getProjectBranchById remoteProjectId remoteBranchId >>= \case
      Share.API.GetProjectBranchResponseNotFound -> do
        loggeth ["GetProjectBranchResponseNotFound"]
        Cli.returnEarlyWithoutOutput
      Share.API.GetProjectBranchResponseSuccess remoteBranch -> pure remoteBranch

  -- We need to compute the "repo name" of the branch that we want to push to, because that's what the Share upload
  -- API requires (currently).
  --
  -- We do this in a (slightly) racy way: ask share what the branch's project/branch name is, then go right back to
  -- share with an upload request. So if a project or branch is renamed in between these calls, this will fail.
  --
  -- A couple example repo names derived from the project/branch names:
  --
  --   "@unison/base" / "@arya/topic" => "arya", because the branch "@arya/topic" has a user component
  --
  --   "@unison/base" / "main"        => "unison", because the branch "main" doesn't have a user component
  --
  --   "something"    / "weird"       => oh no, we probably have to fail here, because even though we tried to design an
  --                                     API that allows any ol' project and branch name, we don't really know *where*
  --                                     to upload (i.e. the repo name of) a project that doesn't have a user component
  repoName <-
    case tryInto @ProjectBranchName (remoteBranch ^. #branchName) of
      -- This shouldn't happen often - Share gave us a branch name that we don't consider valid?
      Left _ -> wundefined
      Right remoteBranchName ->
        case projectBranchNameUserSlug remoteBranchName of
          Nothing ->
            case tryInto @ProjectName (remoteBranch ^. #projectName) of
              -- This shouldn't happen often - Share gave us a project name that we don't consider valid?
              Left _ -> wundefined
              Right remoteProjectName ->
                case projectNameUserSlug remoteProjectName of
                  -- eep, neither project nor branch name have a user slug, so it doesn't seem like we know what to use
                  -- for the repo name. just bail.
                  Nothing -> wundefined
                  Just userSlug -> pure userSlug
          Just userSlug -> pure userSlug

  -- FIXME check if remote branch exists

  -- Nothing to do in "after upload" callback: because project branch already exist.
  pure (repoName, pure ())

bazinga4 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli (Text, Cli ())
bazinga4 localProjectAndBranch remoteProjectAndBranchNames = do
  wundefined

-- Derive the remote branch user slug and remote branch name from the user's handle and the local branch name.
--
-- Example 1 - user "bob" has local branch "topic"
--   remoteBranchUserSlug = "bob"
--   remoteBranchName     = "@bob/topic"
--
-- Example 2 - user "bob" has local branch "@runar/topic"
--   remoteBranchUserSlug = "runar"
--   remoteBranchName     = "@runar/topic"
deriveRemoteBranchName :: Text -> ProjectBranchName -> (Text, ProjectBranchName)
deriveRemoteBranchName myUserHandle branchName =
  case projectBranchNameUserSlug branchName of
    Nothing -> (myUserHandle, prependUserSlugToProjectBranchName myUserHandle branchName)
    Just userSlug -> (userSlug, branchName)

expectProjectAndBranch ::
  ProjectAndBranch Queries.ProjectId Queries.BranchId ->
  Sqlite.Transaction (ProjectAndBranch Queries.Project Queries.Branch)
expectProjectAndBranch (ProjectAndBranch projectId branchId) = do
  project <- Queries.expectProject projectId
  branch <- Queries.expectProjectBranch projectId branchId
  pure (ProjectAndBranch project branch)

oinkUpload :: Share.RepoName -> Hash32 -> Cli Hash32
oinkUpload repoName causalHash32 = do
  loggeth ["uploading entities"]
  Cli.with withEntitiesUploadedProgressCallback \uploadedCallback -> do
    let upload =
          Share.uploadEntities
            (codeserverBaseURL Codeserver.defaultCodeserver)
            repoName
            (Set.NonEmpty.singleton causalHash32)
            uploadedCallback
    upload & onLeftM \err -> do
      loggeth ["upload entities error"]
      loggeth [tShow err]
      Cli.returnEarlyWithoutOutput
  pure causalHash32

oinkCreateRemoteProject projectName = do
  let request = Share.API.CreateProjectRequest {projectName}
  loggeth ["Making create-project request for project"]
  loggeth [tShow request]
  Share.createProject request >>= \case
    Share.API.CreateProjectResponseBadRequest -> do
      loggeth ["Share says: bad request"]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectResponseUnauthorized -> do
      loggeth ["Share says: unauthorized"]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectResponseSuccess remoteProject -> do
      loggeth ["Share says: success!"]
      loggeth [tShow remoteProject]
      loggeth ["TODO insert remote_project"]
      pure remoteProject

oinkCreateRemoteBranch request = do
  loggeth ["creating remote branch"]
  loggeth [tShow request]
  Share.createProjectBranch request >>= \case
    Share.API.CreateProjectBranchResponseBadRequest -> do
      loggeth ["Share says: bad request"]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectBranchResponseUnauthorized -> do
      loggeth ["Share says: unauthorized"]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectBranchResponseSuccess remoteBranch -> do
      loggeth ["Share says: success!"]
      loggeth [tShow remoteBranch]
      loggeth ["TODO insert remote_project_branch"]
      loggeth ["TODO insert remote_project"]
      loggeth ["TODO insert project_branch_remote_mapping"]
      pure remoteBranch

oinkResolveRemoteIds :: Sqlite.Transaction (Maybe (ProjectAndBranch Text (Maybe Text)))
oinkResolveRemoteIds = undefined

oinkGetLoggedInUser :: Cli Text
oinkGetLoggedInUser = do
  loggeth ["Getting current logged-in user on Share"]
  AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  myUserHandle <- wundefined
  loggeth ["Got current logged-in user on Share: ", myUserHandle]
  pure myUserHandle

-- Provide the given action a callback that displays to the terminal.
withEntitiesUploadedProgressCallback :: ((Int -> IO ()) -> IO a) -> IO a
withEntitiesUploadedProgressCallback action = do
  entitiesUploadedVar <- newTVarIO 0
  Console.Regions.displayConsoleRegions do
    Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
      Console.Regions.setConsoleRegion region do
        entitiesUploaded <- readTVar entitiesUploadedVar
        pure $
          "\n  Uploaded "
            <> tShow entitiesUploaded
            <> " entities...\n\n"
      result <- action (\n -> atomically (modifyTVar' entitiesUploadedVar (+ n)))
      entitiesUploaded <- readTVarIO entitiesUploadedVar
      Console.Regions.finishConsoleRegion region $
        "\n  Uploaded " <> tShow entitiesUploaded <> " entities."
      pure result
