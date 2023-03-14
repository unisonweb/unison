-- | @push@ input handler
module Unison.Codebase.Editor.HandleInput.Push
  ( handleGist,
    handlePushRemoteBranch,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens (over, to, (.~), (^.), _1)
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Set.NonEmpty as Set.NonEmpty
import Data.Text as Text
import Data.These (These (..))
import qualified System.Console.Regions as Console.Regions
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.Operations as Operations
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.ProjectUtils
  ( expectBranchName,
    expectProjectName,
    getCurrentProjectBranch,
    loggeth,
    projectBranchPath,
  )
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
    PushTarget (..),
  )
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
          remoteProjectAndBranch <- ProjectUtils.resolveNames remoteProjectAndBranch0
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
      remoteProjectAndBranch <- ProjectUtils.resolveNames remoteProjectAndBranch0
      pushLooseCodeToProjectBranch localPath remoteProjectAndBranch
    -- push @some/project to .some.path
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (PathyTarget remotePath) -> do
      localProjectAndBranch <- ProjectUtils.resolveNamesToIds localProjectAndBranch0
      pushLooseCodeToLooseCode (projectBranchPath localProjectAndBranch) remotePath pushBehavior syncMode
    -- push @some/project to @some/project
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (ProjyTarget remoteProjectAndBranch) -> do
      localProjectAndBranch <- ProjectUtils.resolveNamesToIds localProjectAndBranch0
      pushProjectBranchToProjectBranch localProjectAndBranch (Just remoteProjectAndBranch)

-- Push a local namespace ("loose code") to a remote namespace ("loose code").
pushLooseCodeToLooseCode :: Path.Absolute -> WriteRemotePath -> PushBehavior -> SyncMode -> Cli ()
pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode = do
  case remotePath of
    WriteRemotePathGit gitRemotePath -> pushLooseCodeToGitLooseCode localPath gitRemotePath pushBehavior syncMode
    WriteRemotePathShare shareRemotePath -> pushLooseCodeToShareLooseCode localPath shareRemotePath pushBehavior

-- Push a local namespace ("loose code") to a Git-hosted remote namespace ("loose code").
pushLooseCodeToGitLooseCode :: Path.Absolute -> WriteGitRemotePath -> PushBehavior -> SyncMode -> Cli ()
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

-- Push a local namespace ("loose code") to a Share-hosted remote namespace ("loose code").
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
      checkAndSetPush (Share.API.hashJWTHash <$> maybeHashJwt)
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

-- Push a local namespace ("loose code") to a remote project branch.
pushLooseCodeToProjectBranch :: Path.Absolute -> ProjectAndBranch ProjectName ProjectBranchName -> Cli ()
pushLooseCodeToProjectBranch localPath remoteProjectAndBranch = do
  localBranchHead <- Cli.runEitherTransaction (loadCausalHashToPush localPath)
  uploadPlan <- pushToProjectBranch0 PushingLooseCode localBranchHead remoteProjectAndBranch
  executeUploadPlan uploadPlan

-- | Push a local project branch to a remote project branch. If the remote project branch is left unspecified, we either
-- use a pre-existing mapping for the local branch, or else infer what remote branch to push to (possibly creating it).
pushProjectBranchToProjectBranch ::
  ProjectAndBranch ProjectId ProjectBranchId ->
  Maybe (These ProjectName ProjectBranchName) ->
  Cli ()
pushProjectBranchToProjectBranch localProjectAndBranchIds maybeRemoteProjectAndBranchNames = do
  -- Load local project and branch from database and get the causal hash to push
  (localProjectAndBranch, localBranchHead) <-
    Cli.runEitherTransaction do
      loadCausalHashToPush (projectBranchPath localProjectAndBranchIds) >>= \case
        Left output -> pure (Left output)
        Right hash -> do
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
bazinga50 :: ProjectAndBranch Queries.Project Queries.Branch -> Hash32 -> Maybe ProjectBranchName -> Cli UploadPlan
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
        Queries.loadRemoteProjectBranch localProjectId localBranchId >>= \case
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
              myUserHandle <- oinkGetLoggedInUser
              let localBranchName = localProjectAndBranch ^. #branch . #name
              -- Derive the remote branch name from the user's handle and the local branch name.
              --
              -- user "bob" has local branch "topic":        remoteBranchName = "@bob/topic"
              -- user "bob" has local branch "@runar/topic": remoteBranchName = "@runar/topic"
              let remoteBranchName =
                    case projectBranchNameUserSlug localBranchName of
                      Nothing -> prependUserSlugToProjectBranchName myUserHandle localBranchName
                      Just _userSlug -> localBranchName
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
                Share.API.GetProjectBranchResponseBranchNotFound _ -> remoteProjectBranchDoesntExist
                Share.API.GetProjectBranchResponseProjectNotFound _ -> remoteProjectBranchDoesntExist
                Share.API.GetProjectBranchResponseUnauthorized x -> ProjectUtils.unauthorized x
                Share.API.GetProjectBranchResponseSuccess remoteBranch -> do
                  remoteBranch1 <- expectRemoteProjectAndBranch remoteBranch
                  afterUploadAction <-
                    makeFastForwardAfterUploadAction
                      (PushingProjectBranch localProjectAndBranch)
                      localBranchHead
                      remoteBranch
                  pure UploadPlan {remoteBranch = remoteBranch1, causalHash = localBranchHead, afterUploadAction}
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
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli UploadPlan
bazinga10 localProjectAndBranch localBranchHead remoteProjectAndBranchMaybes = do
  myUserHandle <- oinkGetLoggedInUser
  let localProjectName = localProjectAndBranch ^. #project . #name
  let localBranchName = localProjectAndBranch ^. #branch . #name
  let remoteProjectName =
        case remoteProjectAndBranchMaybes ^. #project of
          Nothing -> prependUserSlugToProjectName myUserHandle localProjectName
          Just remoteProjectName1 -> remoteProjectName1
  let remoteBranchName =
        case remoteProjectAndBranchMaybes ^. #branch of
          Nothing -> prependUserSlugToProjectBranchName myUserHandle localBranchName
          Just remoteBranchName1 -> remoteBranchName1
  let remoteProjectAndBranch = ProjectAndBranch remoteProjectName remoteBranchName
  pushToProjectBranch0 (PushingProjectBranch localProjectAndBranch) localBranchHead remoteProjectAndBranch

-- What are we pushing, a project branch or loose code?
data WhatAreWePushing
  = PushingProjectBranch (ProjectAndBranch Queries.Project Queries.Branch)
  | PushingLooseCode

-- we have the remote project and branch names, but we don't know whether either already exist
-- FIXME call this function with a slug-prefixed branch, so that it can create "main" without slug
pushToProjectBranch0 :: WhatAreWePushing -> Hash32 -> ProjectAndBranch ProjectName ProjectBranchName -> Cli UploadPlan
pushToProjectBranch0 pushing localBranchHead remoteProjectAndBranch = do
  let remoteProjectName = remoteProjectAndBranch ^. #project
  Share.getProjectByName remoteProjectName >>= \case
    Nothing -> do
      remoteProject <-
        Share.createProject remoteProjectName & onNothingM do
          Cli.returnEarly $
            Output.RemoteProjectBranchDoesntExist
              Share.hardCodedUri
              remoteProjectAndBranch
      pure
        UploadPlan
          { remoteBranch = remoteProjectAndBranch,
            causalHash = localBranchHead,
            afterUploadAction =
              createBranchAfterUploadAction
                pushing
                localBranchHead
                (over #project (remoteProject ^. #projectId,) remoteProjectAndBranch)
          }
    Just remoteProject -> do
      let remoteProjectId = remoteProject ^. #projectId
      Share.getProjectBranchByName (remoteProjectAndBranch & #project .~ remoteProjectId) >>= \case
        Share.API.GetProjectBranchResponseBranchNotFound {} -> do
          pure
            UploadPlan
              { remoteBranch = remoteProjectAndBranch,
                causalHash = localBranchHead,
                afterUploadAction =
                  createBranchAfterUploadAction
                    pushing
                    localBranchHead
                    (over #project (remoteProjectId,) remoteProjectAndBranch)
              }
        Share.API.GetProjectBranchResponseProjectNotFound {} ->
          Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri remoteProjectAndBranch)
        Share.API.GetProjectBranchResponseUnauthorized x -> ProjectUtils.unauthorized x
        Share.API.GetProjectBranchResponseSuccess remoteBranch -> do
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
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch (RemoteProjectId, ProjectName) ProjectBranchName ->
  Cli UploadPlan
pushToProjectBranch1 localProjectAndBranch localBranchHead remoteProjectAndBranch = do
  Share.getProjectBranchByName (over #project fst remoteProjectAndBranch) >>= \case
    Share.API.GetProjectBranchResponseBranchNotFound {} -> do
      pure
        UploadPlan
          { remoteBranch = over #project snd remoteProjectAndBranch,
            causalHash = localBranchHead,
            afterUploadAction =
              createBranchAfterUploadAction
                (PushingProjectBranch localProjectAndBranch)
                localBranchHead
                remoteProjectAndBranch
          }
    Share.API.GetProjectBranchResponseProjectNotFound {} -> remoteProjectBranchDoesntExist
    Share.API.GetProjectBranchResponseUnauthorized x -> ProjectUtils.unauthorized x
    Share.API.GetProjectBranchResponseSuccess remoteBranch -> do
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
  loggeth ["uploading entities"]
  Cli.with withEntitiesUploadedProgressCallback \uploadedCallback -> do
    let upload =
          Share.uploadEntities
            (codeserverBaseURL Codeserver.defaultCodeserver)
            -- On the wire, the remote branch is encoded as e.g.
            --   { "repo_info": "@unison/base/@arya/topic", ... }
            (Share.RepoInfo (into @Text (These (remoteBranch ^. #project) (remoteBranch ^. #branch))))
            (Set.NonEmpty.singleton causalHash)
            uploadedCallback
    upload & onLeftM \err -> do
      loggeth ["upload entities error"]
      loggeth [tShow err]
      Cli.returnEarlyWithoutOutput
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
  Hash32 ->
  ProjectAndBranch (RemoteProjectId, ProjectName) ProjectBranchName ->
  AfterUploadAction
createBranchAfterUploadAction pushing localBranchHead remoteProjectAndBranch = do
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
    Share.createProjectBranch createProjectBranchRequest >>= \case
      Share.API.CreateProjectBranchResponseUnauthorized x -> ProjectUtils.unauthorized x
      Share.API.CreateProjectBranchResponseNotFound _ ->
        Cli.returnEarly $
          Output.RemoteProjectBranchDoesntExist Share.hardCodedUri (over #project snd remoteProjectAndBranch)
      Share.API.CreateProjectBranchResponseMissingCausalHash hash -> bugRemoteMissingCausalHash hash
      Share.API.CreateProjectBranchResponseSuccess remoteBranch -> pure remoteBranch
  case pushing of
    PushingLooseCode -> pure ()
    PushingProjectBranch (ProjectAndBranch localProject localBranch) ->
      Cli.runTransaction do
        -- If the local branch has no associated remote then we
        -- associate this newly created branch.
        Queries.ensureBranchRemoteMapping
          (localProject ^. #projectId)
          (localBranch ^. #branchId)
          (RemoteProjectId (remoteBranch ^. #projectId))
          Share.hardCodedUri
          (RemoteProjectBranchId (remoteBranch ^. #branchId))

-- We intend to fast-forward a remote branch. There's one last check to do, which may cause this action to
-- short-circuit: check to see if the remote branch is indeed behind the given causal hash. If it is, then return an
-- action to perform after uploading (which will set the remote branch head).
makeFastForwardAfterUploadAction ::
  WhatAreWePushing ->
  Hash32 ->
  Share.API.ProjectBranch ->
  Cli AfterUploadAction
makeFastForwardAfterUploadAction pushing localBranchHead remoteBranch = do
  let remoteProjectAndBranchNames =
        ProjectAndBranch
          (unsafeFrom @Text (remoteBranch ^. #projectName))
          (unsafeFrom @Text (remoteBranch ^. #branchName))

  let remoteProjectBranchHeadMismatch :: Cli a
      remoteProjectBranchHeadMismatch =
        Cli.returnEarly (RemoteProjectBranchHeadMismatch Share.hardCodedUri remoteProjectAndBranchNames)

  whenM (Cli.runTransaction (wouldNotBeFastForward localBranchHead remoteBranchHead)) do
    remoteProjectBranchHeadMismatch

  pure do
    let request =
          Share.API.SetProjectBranchHeadRequest
            { projectId = remoteBranch ^. #projectId,
              branchId = remoteBranch ^. #branchId,
              branchOldCausalHash = Just remoteBranchHead,
              branchNewCausalHash = localBranchHead
            }
    Share.setProjectBranchHead request >>= \case
      Share.API.SetProjectBranchHeadResponseUnauthorized x -> ProjectUtils.unauthorized x
      Share.API.SetProjectBranchHeadResponseNotFound _ -> do
        Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri remoteProjectAndBranchNames)
      Share.API.SetProjectBranchHeadResponseMissingCausalHash hash -> bugRemoteMissingCausalHash hash
      Share.API.SetProjectBranchHeadResponseExpectedCausalHashMismatch _expected _actual ->
        remoteProjectBranchHeadMismatch
      Share.API.SetProjectBranchHeadResponseSuccess -> do
        case pushing of
          PushingLooseCode -> pure ()
          PushingProjectBranch (ProjectAndBranch localProject localBranch) -> do
            Cli.runTransaction do
              Queries.ensureBranchRemoteMapping
                (localProject ^. #projectId)
                (localBranch ^. #branchId)
                (remoteBranch ^. #projectId . to RemoteProjectId)
                Share.hardCodedUri
                (remoteBranch ^. #branchId . to RemoteProjectBranchId)
  where
    remoteBranchHead =
      Share.API.hashJWTHash (remoteBranch ^. #branchHead)

bugRemoteMissingCausalHash :: Hash32 -> a
bugRemoteMissingCausalHash hash =
  error (reportBug "E796475" ("Create remote branch: causal hash missing: " ++ show hash))

oinkGetLoggedInUser :: Cli Text
oinkGetLoggedInUser = do
  loggeth ["Getting current logged-in user on Share"]
  userInfo <- AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  let myUserHandle = userInfo ^. #handle
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

------------------------------------------------------------------------------------------------------------------------
-- Misc. sqlite queries

-- Resolve project/branch ids to project/branch records.
expectProjectAndBranch ::
  ProjectAndBranch ProjectId ProjectBranchId ->
  Sqlite.Transaction (ProjectAndBranch Queries.Project Queries.Branch)
expectProjectAndBranch (ProjectAndBranch projectId branchId) =
  ProjectAndBranch
    <$> Queries.expectProject projectId
    <*> Queries.expectProjectBranch projectId branchId

-- Get the causal hash to push at the given path, or error if there's no history there.
loadCausalHashToPush :: Path.Absolute -> Sqlite.Transaction (Either Output Hash32)
loadCausalHashToPush path =
  Operations.loadCausalHashAtPath segments <&> \case
    -- If there is nothing to push, fail with some message
    Nothing -> Left (EmptyPush (Path.absoluteToPath' path))
    Just (CausalHash hash) -> Right (Hash32.fromHash hash)
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

------------------------------------------------------------------------------------------------------------------------
-- Extracting things out of Share project/branch names
--
-- A Share project is just an opaque text, but we often need to assert that it actually is of the form @user/name

expectRemoteProjectAndBranch :: Share.API.ProjectBranch -> Cli (ProjectAndBranch ProjectName ProjectBranchName)
expectRemoteProjectAndBranch branch = do
  projectName <- expectProjectName (branch ^. #projectName)
  branchName <- expectBranchName (branch ^. #branchName)
  pure (ProjectAndBranch projectName branchName)
