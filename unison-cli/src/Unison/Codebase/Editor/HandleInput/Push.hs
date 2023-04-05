-- | @push@ input handler
module Unison.Codebase.Editor.HandleInput.Push
  ( handleGist,
    handlePushRemoteBranch,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens (over, view, (.~), (^.), _1, _2)
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Set.NonEmpty as Set.NonEmpty
import Data.Text as Text
import Data.These (These (..))
import Data.Void (absurd)
import qualified System.Console.Regions as Console.Regions
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.Operations as Operations
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Project as Sqlite (Project)
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite (ProjectBranch)
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Cli.UnisonConfigUtils as UnisonConfigUtils
import Unison.Codebase (PushGitBranchOpts (..))
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.HandleInput.AuthLogin (ensureAuthenticatedWithCodeserver)
import qualified Unison.Codebase.Editor.HandleInput.AuthLogin as AuthLogin
import Unison.Codebase.Editor.Input
  ( GistInput (..),
    PushRemoteBranchInput (..),
    PushSource (..),
    PushSourceTarget (..),
  )
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.Output.PushPull (PushPull (Push))
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadRemoteNamespace (..),
    ShareUserHandle (..),
    WriteGitRemoteNamespace (..),
    WriteRemoteNamespace (..),
    WriteShareRemoteNamespace (..),
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
import qualified Unison.Share.API.Hash as Share.API
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
      ProjectUtils.getCurrentProjectBranch >>= \case
        Nothing -> do
          localPath <- Cli.getCurrentPath
          UnisonConfigUtils.resolveConfiguredUrl Push Path.currentPath >>= \case
            WriteRemoteNamespaceGit namespace -> pushLooseCodeToGitLooseCode localPath namespace pushBehavior syncMode
            WriteRemoteNamespaceShare namespace -> pushLooseCodeToShareLooseCode localPath namespace pushBehavior
            WriteRemoteProjectBranch v -> absurd v
        Just localProjectAndBranch -> pushProjectBranchToProjectBranch localProjectAndBranch Nothing
    -- push <implicit> to .some.path (git)
    PushSourceTarget1 (WriteRemoteNamespaceGit namespace) -> do
      localPath <- Cli.getCurrentPath
      pushLooseCodeToGitLooseCode localPath namespace pushBehavior syncMode
    -- push <implicit> to .some.path (share)
    PushSourceTarget1 (WriteRemoteNamespaceShare namespace) -> do
      localPath <- Cli.getCurrentPath
      pushLooseCodeToShareLooseCode localPath namespace pushBehavior
    -- push <implicit> to @some/project
    PushSourceTarget1 (WriteRemoteProjectBranch remoteProjectAndBranch0) ->
      ProjectUtils.getCurrentProjectBranch >>= \case
        Nothing -> do
          localPath <- Cli.getCurrentPath
          remoteProjectAndBranch <- ProjectUtils.hydrateNames remoteProjectAndBranch0
          pushLooseCodeToProjectBranch localPath remoteProjectAndBranch
        Just localProjectAndBranch ->
          pushProjectBranchToProjectBranch localProjectAndBranch (Just remoteProjectAndBranch0)
    -- push .some.path to .some.path (git)
    PushSourceTarget2 (PathySource localPath0) (WriteRemoteNamespaceGit namespace) -> do
      localPath <- Cli.resolvePath' localPath0
      pushLooseCodeToGitLooseCode localPath namespace pushBehavior syncMode
    -- push .some.path to .some.path (share)
    PushSourceTarget2 (PathySource localPath0) (WriteRemoteNamespaceShare namespace) -> do
      localPath <- Cli.resolvePath' localPath0
      pushLooseCodeToShareLooseCode localPath namespace pushBehavior
    -- push .some.path to @some/project
    PushSourceTarget2 (PathySource localPath0) (WriteRemoteProjectBranch remoteProjectAndBranch0) -> do
      localPath <- Cli.resolvePath' localPath0
      remoteProjectAndBranch <- ProjectUtils.hydrateNames remoteProjectAndBranch0
      pushLooseCodeToProjectBranch localPath remoteProjectAndBranch
    -- push @some/project to .some.path (git)
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (WriteRemoteNamespaceGit namespace) -> do
      ProjectAndBranch project branch <- ProjectUtils.expectProjectAndBranchByTheseNames localProjectAndBranch0
      pushLooseCodeToGitLooseCode
        (ProjectUtils.projectBranchPath (ProjectAndBranch (project ^. #projectId) (branch ^. #branchId)))
        namespace
        pushBehavior
        syncMode
    -- push @some/project to .some.path (share)
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (WriteRemoteNamespaceShare namespace) -> do
      ProjectAndBranch project branch <- ProjectUtils.expectProjectAndBranchByTheseNames localProjectAndBranch0
      pushLooseCodeToShareLooseCode
        (ProjectUtils.projectBranchPath (ProjectAndBranch (project ^. #projectId) (branch ^. #branchId)))
        namespace
        pushBehavior
    -- push @some/project to @some/project
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (WriteRemoteProjectBranch remoteProjectAndBranch) -> do
      localProjectAndBranch <- ProjectUtils.expectProjectAndBranchByTheseNames localProjectAndBranch0
      pushProjectBranchToProjectBranch localProjectAndBranch (Just remoteProjectAndBranch)

-- Push a local namespace ("loose code") to a Git-hosted remote namespace ("loose code").
pushLooseCodeToGitLooseCode :: Path.Absolute -> WriteGitRemoteNamespace -> PushBehavior -> SyncMode -> Cli ()
pushLooseCodeToGitLooseCode localPath gitRemotePath pushBehavior syncMode = do
  sourceBranch <- Cli.getBranchAt localPath
  let withRemoteRoot :: Branch IO -> Either Output (Branch IO)
      withRemoteRoot remoteRoot = do
        let -- We don't merge `sourceBranch` with `remoteBranch`, we just replace it. This push will be rejected if
            -- this rewinds time or misses any new updates in the remote branch that aren't in `sourceBranch`
            -- already.
            f remoteBranch = if shouldPushTo pushBehavior remoteBranch then Just sourceBranch else Nothing
        case Branch.modifyAtM (gitRemotePath ^. #path) f remoteRoot of
          Nothing -> Left (RefusedToPush pushBehavior (WriteRemoteNamespaceGit gitRemotePath))
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

-- Push a local namespace ("loose code") to a Share-hosted remote namespace ("loose code").
pushLooseCodeToShareLooseCode :: Path.Absolute -> WriteShareRemoteNamespace -> PushBehavior -> Cli ()
pushLooseCodeToShareLooseCode localPath remote@WriteShareRemoteNamespace {server, repo, path = remotePath} behavior = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  let sharePath = Share.Path (shareUserHandleToText repo Nel.:| pathToSegments remotePath)
  ensureAuthenticatedWithCodeserver codeserver

  localCausalHash <-
    Cli.runTransaction (Ops.loadCausalHashAtPath (pathToSegments (Path.unabsolute localPath))) & onNothingM do
      Cli.returnEarly (EmptyLooseCodePush (Path.absoluteToPath' localPath))

  let checkAndSetPush :: Maybe Hash32 -> Cli (Maybe Int)
      checkAndSetPush remoteHash =
        if Just (Hash32.fromHash (unCausalHash localCausalHash)) == remoteHash
          then pure Nothing
          else do
            let push =
                  Cli.with withEntitiesUploadedProgressCallback \(uploadedCallback, getNumUploaded) -> do
                    result <-
                      Share.checkAndSetPush
                        baseURL
                        sharePath
                        remoteHash
                        localCausalHash
                        uploadedCallback
                    numUploaded <- liftIO getNumUploaded
                    pure (result, numUploaded)
            push >>= \case
              (Left err, _) -> pushError ShareErrorCheckAndSetPush err
              (Right (), numUploaded) -> pure (Just numUploaded)

  case behavior of
    PushBehavior.ForcePush -> do
      maybeHashJwt <-
        Share.getCausalHashByPath baseURL sharePath & onLeftM \err0 ->
          (Cli.returnEarly . Output.ShareError) case err0 of
            Share.SyncError err -> ShareErrorGetCausalHashByPath err
            Share.TransportError err -> ShareErrorTransport err
      maybeNumUploaded <- checkAndSetPush (Share.API.hashJWTHash <$> maybeHashJwt)
      whenJust maybeNumUploaded (Cli.respond . Output.UploadedEntities)
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireEmpty -> do
      maybeNumUploaded <- checkAndSetPush Nothing
      whenJust maybeNumUploaded (Cli.respond . Output.UploadedEntities)
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireNonEmpty -> do
      let push :: Cli (Either (Share.SyncError Share.FastForwardPushError) (), Int)
          push =
            Cli.with withEntitiesUploadedProgressCallback \(uploadedCallback, getNumUploaded) -> do
              result <-
                Share.fastForwardPush
                  baseURL
                  sharePath
                  localCausalHash
                  uploadedCallback
              numUploaded <- liftIO getNumUploaded
              pure (result, numUploaded)
      push >>= \case
        (Left err, _) -> pushError ShareErrorFastForwardPush err
        (Right (), numUploaded) -> do
          Cli.respond (UploadedEntities numUploaded)
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

-- Push a local namespace ("loose code") to a remote project branch.
pushLooseCodeToProjectBranch :: Path.Absolute -> ProjectAndBranch ProjectName ProjectBranchName -> Cli ()
pushLooseCodeToProjectBranch localPath remoteProjectAndBranch = do
  localBranchHead <-
    Cli.runEitherTransaction do
      loadCausalHashToPush localPath <&> \case
        Nothing -> Left (EmptyLooseCodePush (Path.absoluteToPath' localPath))
        Just hash -> Right hash

  uploadPlan <- pushToProjectBranch0 PushingLooseCode localBranchHead remoteProjectAndBranch
  executeUploadPlan uploadPlan

-- | Push a local project branch to a remote project branch. If the remote project branch is left unspecified, we either
-- use a pre-existing mapping for the local branch, or else infer what remote branch to push to (possibly creating it).
pushProjectBranchToProjectBranch ::
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Maybe (These ProjectName ProjectBranchName) ->
  Cli ()
pushProjectBranchToProjectBranch localProjectAndBranch maybeRemoteProjectAndBranchNames = do
  let localProjectAndBranchIds = localProjectAndBranch & over #project (view #projectId) & over #branch (view #branchId)
  let localProjectAndBranchNames = localProjectAndBranch & over #project (view #name) & over #branch (view #name)

  -- Load local project and branch from database and get the causal hash to push
  (localProjectAndBranch, localBranchHead) <-
    Cli.runEitherTransaction do
      loadCausalHashToPush (ProjectUtils.projectBranchPath localProjectAndBranchIds) >>= \case
        Nothing -> pure (Left (EmptyProjectBranchPush localProjectAndBranchNames))
        Just hash -> do
          localProjectAndBranch <- expectProjectAndBranch localProjectAndBranchIds
          pure (Right (localProjectAndBranch, hash))

  uploadPlan <-
    case maybeRemoteProjectAndBranchNames of
      Nothing -> bazinga50 localProjectAndBranch localBranchHead Nothing
      Just (This remoteProjectName) ->
        bazinga10 localProjectAndBranch localBranchHead (ProjectAndBranch (Just remoteProjectName) Nothing)
      Just (That remoteBranchName) -> bazinga50 localProjectAndBranch localBranchHead (Just remoteBranchName)
      Just (These remoteProjectName remoteBranchName) ->
        pushToProjectBranch0
          (PushingProjectBranch localProjectAndBranch)
          localBranchHead
          (ProjectAndBranch remoteProjectName remoteBranchName)

  executeUploadPlan uploadPlan

-- "push" or "push /foo", remote mapping unknown
bazinga50 :: ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> Hash32 -> Maybe ProjectBranchName -> Cli UploadPlan
bazinga50 localProjectAndBranch localBranchHead maybeRemoteBranchName = do
  let loadRemoteProjectInfo ::
        Sqlite.Transaction
          ( Maybe
              ( RemoteProjectId,
                ProjectName,
                Maybe (RemoteProjectBranchId, ProjectBranchName)
              )
          )
      loadRemoteProjectInfo =
        Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId >>= \case
          Nothing -> pure Nothing
          Just (remoteProjectId, maybeRemoteBranchId) -> do
            remoteProjectName <- Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri
            maybeRemoteBranchInfo <-
              for maybeRemoteBranchId \remoteBranchId -> do
                remoteBranchName <-
                  Queries.expectRemoteProjectBranchName Share.hardCodedUri remoteProjectId remoteBranchId
                pure (remoteBranchId, remoteBranchName)
            pure (Just (remoteProjectId, remoteProjectName, maybeRemoteBranchInfo))

  Cli.runTransaction loadRemoteProjectInfo >>= \case
    Nothing -> bazinga10 localProjectAndBranch localBranchHead (ProjectAndBranch Nothing maybeRemoteBranchName)
    Just (remoteProjectId, remoteProjectName, maybeRemoteBranchInfo) ->
      case maybeRemoteBranchName of
        Nothing -> do
          case maybeRemoteBranchInfo of
            -- "push" with remote mapping for project from ancestor branch
            Nothing -> do
              myUserHandle <- view #handle <$> AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
              let localBranchName = localProjectAndBranch ^. #branch . #name
              let remoteBranchName = deriveRemoteBranchName myUserHandle remoteProjectName localBranchName
              pushToProjectBranch1
                localProjectAndBranch
                localBranchHead
                (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
            -- "push" with remote mapping for branch
            Just (remoteBranchId, remoteBranchName) -> do
              let remoteProjectBranchDoesntExist =
                    Cli.returnEarly $
                      Output.RemoteProjectBranchDoesntExist
                        Share.hardCodedUri
                        (ProjectAndBranch remoteProjectName remoteBranchName)
              Share.getProjectBranchById (ProjectAndBranch remoteProjectId remoteBranchId) >>= \case
                Share.GetProjectBranchResponseBranchNotFound -> remoteProjectBranchDoesntExist
                Share.GetProjectBranchResponseProjectNotFound -> remoteProjectBranchDoesntExist
                Share.GetProjectBranchResponseSuccess remoteBranch -> do
                  afterUploadAction <-
                    makeFastForwardAfterUploadAction
                      (PushingProjectBranch localProjectAndBranch)
                      localBranchHead
                      remoteBranch
                  pure
                    UploadPlan
                      { remoteBranch = ProjectAndBranch (remoteBranch ^. #projectName) (remoteBranch ^. #branchName),
                        causalHash = localBranchHead,
                        afterUploadAction
                      }
        -- "push /foo" with remote mapping for project from ancestor branch
        Just remoteBranchName ->
          pushToProjectBranch1
            localProjectAndBranch
            localBranchHead
            (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
  where
    localProjectId = localProjectAndBranch ^. #project . #projectId
    localBranchId = localProjectAndBranch ^. #branch . #branchId

-- "push", "push foo", or "push /foo" ignoring remote mapping (if any)
bazinga10 ::
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Hash32 ->
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli UploadPlan
bazinga10 localProjectAndBranch localBranchHead remoteProjectAndBranchMaybes = do
  myUserHandle <- view #handle <$> AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  let localProjectName = localProjectAndBranch ^. #project . #name
  let localBranchName = localProjectAndBranch ^. #branch . #name
  let remoteProjectName =
        case remoteProjectAndBranchMaybes ^. #project of
          Nothing -> prependUserSlugToProjectName myUserHandle localProjectName
          Just remoteProjectName1 -> remoteProjectName1
  let remoteBranchName =
        case remoteProjectAndBranchMaybes ^. #branch of
          Nothing -> deriveRemoteBranchName myUserHandle remoteProjectName localBranchName
          Just remoteBranchName1 -> remoteBranchName1
  let remoteProjectAndBranch = ProjectAndBranch remoteProjectName remoteBranchName
  pushToProjectBranch0 (PushingProjectBranch localProjectAndBranch) localBranchHead remoteProjectAndBranch

-- If left unspecified (and we don't yet have a remote mapping), we derive the remote branch name from the user's
-- handle, remote project name, and local branch name as follows:
--
--   * If the local branch name already has a user slug prefix, then we leave it alone.
--   * Otherwise, if the remote project name's user prefix matches the user's handle (i.e. they are pushing to their own
--     project) *and* the local branch name is "main", then we leave it alone.
--   * Otherwise, we prepend the user's handle to the local branch name.
--
-- This way, users (who let us infer remote branch names) tend to make topic branches, even when contributing to their
-- own project (e.g. pushing a local branch "foo" to my own project "@runar/lens" will create a remote branch called
-- "@runar/foo", not "foo"), because ephemeral topic branches are far more common than long-lived branches.
--
-- If a user wants to create a long-lived branch alongside their "main" branch (say "oldstuff"), they'll just have to
-- name "oldstuff" explicitly when pushing (the first time).
--
-- And "main" is an exception to the rule that we prefix your local branch name with your user handle. That way, you
-- won't end up with a *topic branch* called "@arya/main" when pushing a local branch called "main". Of course,
-- special-casing "main" in this way is only temporary, before we have a first-class notion of a default branch.
deriveRemoteBranchName :: Text -> ProjectName -> ProjectBranchName -> ProjectBranchName
deriveRemoteBranchName userHandle remoteProjectName localBranchName =
  case projectBranchNameUserSlug localBranchName of
    Just _ -> localBranchName -- already "@user/branch"; don't mess with it
    Nothing ->
      case projectNameUserSlug remoteProjectName of
        -- I'm "arya" pushing local branch "main" to "@arya/lens", so don't call it "@arya/main"
        Just projectUserSlug
          | projectUserSlug == userHandle && localBranchName == unsafeFrom @Text "main" ->
              localBranchName
        -- Nothing is a weird unlikely case: project doesn't begin with a user slug? server will likely reject
        _ -> prependUserSlugToProjectBranchName userHandle localBranchName

-- What are we pushing, a project branch or loose code?
data WhatAreWePushing
  = PushingProjectBranch (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
  | PushingLooseCode

-- we have the remote project and branch names, but we don't know whether either already exist
pushToProjectBranch0 :: WhatAreWePushing -> Hash32 -> ProjectAndBranch ProjectName ProjectBranchName -> Cli UploadPlan
pushToProjectBranch0 pushing localBranchHead remoteProjectAndBranch = do
  let remoteProjectName = remoteProjectAndBranch ^. #project

  -- Assert that this project name has a user slug before bothering to hit Share
  _ <-
    projectNameUserSlug remoteProjectName & onNothing do
      Cli.returnEarly (Output.ProjectNameRequiresUserSlug remoteProjectName)

  Share.getProjectByName remoteProjectName >>= \case
    Nothing -> do
      remoteProject <-
        Share.createProject remoteProjectName & onNothingM do
          Cli.returnEarly (Output.RemoteProjectDoesntExist Share.hardCodedUri remoteProjectName)
      pure
        UploadPlan
          { remoteBranch = remoteProjectAndBranch,
            causalHash = localBranchHead,
            afterUploadAction =
              createBranchAfterUploadAction
                pushing
                True -- just created the project
                localBranchHead
                (over #project (remoteProject ^. #projectId,) remoteProjectAndBranch)
          }
    Just remoteProject -> do
      let remoteProjectId = remoteProject ^. #projectId
      Share.getProjectBranchByName (remoteProjectAndBranch & #project .~ remoteProjectId) >>= \case
        Share.GetProjectBranchResponseBranchNotFound -> do
          pure
            UploadPlan
              { remoteBranch = remoteProjectAndBranch,
                causalHash = localBranchHead,
                afterUploadAction =
                  createBranchAfterUploadAction
                    pushing
                    False -- didn't just create the project
                    localBranchHead
                    (over #project (remoteProjectId,) remoteProjectAndBranch)
              }
        Share.GetProjectBranchResponseProjectNotFound ->
          Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri remoteProjectAndBranch)
        Share.GetProjectBranchResponseSuccess remoteBranch -> do
          afterUploadAction <- makeFastForwardAfterUploadAction pushing localBranchHead remoteBranch
          pure
            UploadPlan
              { remoteBranch = remoteProjectAndBranch,
                causalHash = localBranchHead,
                afterUploadAction
              }

-- "push /foo" with a remote mapping for the project (either from this branch or one of our ancestors)
-- but we don't know whether the remote branch exists
pushToProjectBranch1 ::
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Hash32 ->
  ProjectAndBranch (RemoteProjectId, ProjectName) ProjectBranchName ->
  Cli UploadPlan
pushToProjectBranch1 localProjectAndBranch localBranchHead remoteProjectAndBranch = do
  Share.getProjectBranchByName (over #project fst remoteProjectAndBranch) >>= \case
    Share.GetProjectBranchResponseBranchNotFound -> do
      pure
        UploadPlan
          { remoteBranch = over #project snd remoteProjectAndBranch,
            causalHash = localBranchHead,
            afterUploadAction =
              createBranchAfterUploadAction
                (PushingProjectBranch localProjectAndBranch)
                False -- didn't just create the project
                localBranchHead
                remoteProjectAndBranch
          }
    Share.GetProjectBranchResponseProjectNotFound -> remoteProjectBranchDoesntExist
    Share.GetProjectBranchResponseSuccess remoteBranch -> do
      afterUploadAction <-
        makeFastForwardAfterUploadAction (PushingProjectBranch localProjectAndBranch) localBranchHead remoteBranch
      pure
        UploadPlan
          { remoteBranch = over #project snd remoteProjectAndBranch,
            causalHash = localBranchHead,
            afterUploadAction
          }
  where
    remoteProjectBranchDoesntExist :: Cli void
    remoteProjectBranchDoesntExist =
      Cli.returnEarly $
        Output.RemoteProjectBranchDoesntExist
          Share.hardCodedUri
          (over #project snd remoteProjectAndBranch)

------------------------------------------------------------------------------------------------------------------------
-- Upload plan

-- A plan for uploading to a remote branch and doing something afterwards.
data UploadPlan = UploadPlan
  { -- The remote branch we are uploading entities for.
    remoteBranch :: ProjectAndBranch ProjectName ProjectBranchName,
    -- The causal hash to upload.
    causalHash :: Hash32,
    -- The action to call after a successful upload.
    afterUploadAction :: AfterUploadAction
  }

-- Execute an upload plan.
executeUploadPlan :: UploadPlan -> Cli ()
executeUploadPlan UploadPlan {remoteBranch, causalHash, afterUploadAction} = do
  numUploaded <-
    Cli.with withEntitiesUploadedProgressCallback \(uploadedCallback, getNumUploaded) -> do
      let upload =
            Share.uploadEntities
              (codeserverBaseURL Codeserver.defaultCodeserver)
              -- On the wire, the remote branch is encoded as e.g.
              --   { "repo_info": "@unison/base/@arya/topic", ... }
              (Share.RepoInfo (into @Text (These (remoteBranch ^. #project) (remoteBranch ^. #branch))))
              (Set.NonEmpty.singleton causalHash)
              uploadedCallback
      upload & onLeftM \err0 -> do
        (Cli.returnEarly . Output.ShareError) case err0 of
          Share.SyncError err -> ShareErrorUploadEntities err
          Share.TransportError err -> ShareErrorTransport err
      liftIO getNumUploaded
  Cli.respond (Output.UploadedEntities numUploaded)
  afterUploadAction

------------------------------------------------------------------------------------------------------------------------
-- After upload actions
--
-- Depending on the state of the local and remote projects, we may need to do one of a few different things after
-- uploading entities:
--
--   - Create a remote project, then create a remote branch
--   - Create a remote branch
--   - Fast-forward a remote branch
--   - Force-push a remote branch (not here yet)

-- An action to call after a successful upload.
type AfterUploadAction = Cli ()

-- An after-upload action that creates a remote branch.
--
-- Precondition: the remote project exists, but the remote branch doesn't.
createBranchAfterUploadAction ::
  WhatAreWePushing ->
  Bool ->
  Hash32 ->
  ProjectAndBranch (RemoteProjectId, ProjectName) ProjectBranchName ->
  AfterUploadAction
createBranchAfterUploadAction pushing justCreatedProject localBranchHead remoteProjectAndBranch = do
  let remoteProjectId = remoteProjectAndBranch ^. #project . _1
  let remoteBranchName = remoteProjectAndBranch ^. #branch
  branchMergeTarget <-
    runMaybeT do
      ProjectAndBranch localProject localBranch <-
        case pushing of
          PushingProjectBranch localProjectAndBranch -> pure localProjectAndBranch
          PushingLooseCode -> mzero
      (mergeTargetProjectId, mergeTargetBranchId) <-
        MaybeT $
          Cli.runTransaction do
            Queries.loadDefaultMergeTargetForLocalProjectBranch
              (localProject ^. #projectId)
              Share.hardCodedUri
              (localBranch ^. #branchId)
      pure $
        Share.API.ProjectBranchIds
          (unRemoteProjectId mergeTargetProjectId)
          (unRemoteProjectBranchId mergeTargetBranchId)
  let createProjectBranchRequest =
        Share.API.CreateProjectBranchRequest
          { projectId = unRemoteProjectId remoteProjectId,
            branchName = into @Text remoteBranchName,
            branchCausalHash = localBranchHead,
            branchMergeTarget
          }
  remoteBranch <-
    Share.createProjectBranch createProjectBranchRequest & onNothingM do
      Cli.returnEarly $
        Output.RemoteProjectDoesntExist Share.hardCodedUri (remoteProjectAndBranch ^. #project . _2)
  Cli.respond
    if justCreatedProject
      then Output.CreatedRemoteProject Share.hardCodedUri (over #project snd remoteProjectAndBranch)
      else Output.CreatedRemoteProjectBranch Share.hardCodedUri (over #project snd remoteProjectAndBranch)
  case pushing of
    PushingLooseCode -> pure ()
    PushingProjectBranch (ProjectAndBranch localProject localBranch) ->
      Cli.runTransaction do
        -- If the local branch has no associated remote then we
        -- associate this newly created branch.
        Queries.ensureBranchRemoteMapping
          (localProject ^. #projectId)
          (localBranch ^. #branchId)
          (remoteBranch ^. #projectId)
          Share.hardCodedUri
          (remoteBranch ^. #branchId)

-- We intend to fast-forward a remote branch.
--
-- There are two last checks to do that may cause this action to short-circuit:
--
--   1. If the remote branch head is equal to the hash we intend to fast-forward it to, then there's nothing to upload.
--   2. If the remote branch head is ahead of the hash we intend to fast-forward it to, then we will refuse to push
--      (until we implement some syntax for a force-push).
makeFastForwardAfterUploadAction ::
  WhatAreWePushing ->
  Hash32 ->
  Share.RemoteProjectBranch ->
  Cli AfterUploadAction
makeFastForwardAfterUploadAction pushing localBranchHead remoteBranch = do
  let remoteProjectAndBranchNames = ProjectAndBranch (remoteBranch ^. #projectName) (remoteBranch ^. #branchName)

  when (localBranchHead == Share.API.hashJWTHash (remoteBranch ^. #branchHead)) do
    Cli.returnEarly (RemoteProjectBranchIsUpToDate Share.hardCodedUri remoteProjectAndBranchNames)

  whenM (Cli.runTransaction (wouldNotBeFastForward localBranchHead remoteBranchHead)) do
    Cli.returnEarly (RemoteProjectBranchHeadMismatch Share.hardCodedUri remoteProjectAndBranchNames)

  pure do
    let request =
          Share.API.SetProjectBranchHeadRequest
            { projectId = unRemoteProjectId (remoteBranch ^. #projectId),
              branchId = unRemoteProjectBranchId (remoteBranch ^. #branchId),
              branchOldCausalHash = Just remoteBranchHead,
              branchNewCausalHash = localBranchHead
            }
    Share.setProjectBranchHead request >>= \case
      Share.SetProjectBranchHeadResponseExpectedCausalHashMismatch _expected _actual ->
        Cli.returnEarly (RemoteProjectBranchHeadMismatch Share.hardCodedUri remoteProjectAndBranchNames)
      Share.SetProjectBranchHeadResponseNotFound -> do
        Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri remoteProjectAndBranchNames)
      Share.SetProjectBranchHeadResponseSuccess -> do
        case pushing of
          PushingLooseCode -> pure ()
          PushingProjectBranch (ProjectAndBranch localProject localBranch) -> do
            Cli.runTransaction do
              Queries.ensureBranchRemoteMapping
                (localProject ^. #projectId)
                (localBranch ^. #branchId)
                (remoteBranch ^. #projectId)
                Share.hardCodedUri
                (remoteBranch ^. #branchId)
  where
    remoteBranchHead =
      Share.API.hashJWTHash (remoteBranch ^. #branchHead)

-- Provide the given action a callback that displays to the terminal.
withEntitiesUploadedProgressCallback :: ((Int -> IO (), IO Int) -> IO a) -> IO a
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
      action ((\n -> atomically (modifyTVar' entitiesUploadedVar (+ n))), readTVarIO entitiesUploadedVar)

------------------------------------------------------------------------------------------------------------------------
-- Misc. sqlite queries

-- Resolve project/branch ids to project/branch records.
expectProjectAndBranch ::
  ProjectAndBranch ProjectId ProjectBranchId ->
  Sqlite.Transaction (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
expectProjectAndBranch (ProjectAndBranch projectId branchId) =
  ProjectAndBranch
    <$> Queries.expectProject projectId
    <*> Queries.expectProjectBranch projectId branchId

-- Get the causal hash to push at the given path. Return Nothing if there's no history.
loadCausalHashToPush :: Path.Absolute -> Sqlite.Transaction (Maybe Hash32)
loadCausalHashToPush path =
  Operations.loadCausalHashAtPath segments <&> \case
    Nothing -> Nothing
    Just (CausalHash hash) -> Just (Hash32.fromHash hash)
  where
    segments = coerce @[NameSegment] @[Text] (Path.toList (Path.unabsolute path))

-- Were we to try to advance `remoteBranchHead` to `localBranchHead`, would it *not* be a fast-forward?
wouldNotBeFastForward :: Hash32 -> Hash32 -> Sqlite.Transaction Bool
wouldNotBeFastForward localBranchHead remoteBranchHead = do
  maybeHashIds <-
    runMaybeT $
      (,)
        <$> MaybeT (Queries.loadCausalHashIdByCausalHash (CausalHash (Hash32.toHash localBranchHead)))
        <*> MaybeT (Queries.loadCausalHashIdByCausalHash (CausalHash (Hash32.toHash remoteBranchHead)))
  case maybeHashIds of
    Nothing -> pure True
    Just (localBranchHead1, remoteBranchHead1) -> not <$> Queries.before remoteBranchHead1 localBranchHead1
