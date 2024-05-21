-- | @push@ input handler
module Unison.Codebase.Editor.HandleInput.Push
  ( handleGist,
    handlePushRemoteBranch,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens (over, view, (.~), (^.), _1, _2)
import Control.Monad.Reader (ask)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text as Text
import Data.These (These (..))
import Data.Void (absurd)
import System.Console.Regions qualified as Console.Regions
import Text.Builder qualified
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project qualified as Sqlite (Project)
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch)
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.Share.Projects qualified as Share
import Unison.Cli.UnisonConfigUtils qualified as UnisonConfigUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch (..))
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.AuthLogin qualified as AuthLogin
import Unison.Codebase.Editor.Input
  ( GistInput (..),
    PushRemoteBranchInput (..),
    PushSource (..),
    PushSourceTarget (..),
  )
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.Output.PushPull (PushPull (Push))
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadRemoteNamespace (..),
    WriteGitRemoteNamespace (..),
    WriteRemoteNamespace (..),
    WriteShareRemoteNamespace (..),
    writeToReadGit,
  )
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import Unison.Codebase.PushBehavior qualified as PushBehavior
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Codebase.Type (GitPushBehavior (..))
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName))
import Unison.Hash qualified as Hash
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectBranchNameKind (..),
    ProjectName,
    classifyProjectBranchName,
    prependUserSlugToProjectName,
    projectNameUserSlug,
  )
import Unison.Share.API.Hash qualified as Share.API
import Unison.Share.API.Projects qualified as Share.API
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Share.Sync qualified as Share
import Unison.Share.Sync.Types qualified as Share
import Unison.Share.Types (codeserverBaseURL)
import Unison.Sqlite qualified as Sqlite
import Unison.Sync.Types qualified as Share
import Witch (unsafeFrom)

-- | Handle a @gist@ command.
handleGist :: GistInput -> Cli ()
handleGist (GistInput repo) = do
  Cli.Env {codebase} <- ask
  sourceBranch <- Cli.getCurrentBranch
  result <-
    Cli.ioE (Codebase.pushGitBranch codebase repo GitPushBehaviorGist (\_remoteRoot -> pure (Right sourceBranch))) \err ->
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
handlePushRemoteBranch PushRemoteBranchInput {sourceTarget, pushBehavior} = do
  case sourceTarget of
    -- push <implicit> to <implicit>
    PushSourceTarget0 ->
      ProjectUtils.getCurrentProjectBranch >>= \case
        Nothing -> do
          localPath <- Cli.getCurrentPath
          UnisonConfigUtils.resolveConfiguredUrl Push Path.currentPath >>= \case
            WriteRemoteNamespaceGit namespace -> pushLooseCodeToGitLooseCode localPath namespace pushBehavior
            WriteRemoteNamespaceShare namespace -> pushLooseCodeToShareLooseCode localPath namespace pushBehavior
            WriteRemoteProjectBranch v -> absurd v
        Just (localProjectAndBranch, _restPath) ->
          pushProjectBranchToProjectBranch
            force
            localProjectAndBranch
            Nothing
    -- push <implicit> to .some.path (git)
    PushSourceTarget1 (WriteRemoteNamespaceGit namespace) -> do
      localPath <- Cli.getCurrentPath
      pushLooseCodeToGitLooseCode localPath namespace pushBehavior
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
          pushLooseCodeToProjectBranch force localPath remoteProjectAndBranch
        Just (localProjectAndBranch, _restPath) ->
          pushProjectBranchToProjectBranch force localProjectAndBranch (Just remoteProjectAndBranch0)
    -- push .some.path to .some.path (git)
    PushSourceTarget2 (PathySource localPath0) (WriteRemoteNamespaceGit namespace) -> do
      localPath <- Cli.resolvePath' localPath0
      pushLooseCodeToGitLooseCode localPath namespace pushBehavior
    -- push .some.path to .some.path (share)
    PushSourceTarget2 (PathySource localPath0) (WriteRemoteNamespaceShare namespace) -> do
      localPath <- Cli.resolvePath' localPath0
      pushLooseCodeToShareLooseCode localPath namespace pushBehavior
    -- push .some.path to @some/project
    PushSourceTarget2 (PathySource localPath0) (WriteRemoteProjectBranch remoteProjectAndBranch0) -> do
      localPath <- Cli.resolvePath' localPath0
      remoteProjectAndBranch <- ProjectUtils.hydrateNames remoteProjectAndBranch0
      pushLooseCodeToProjectBranch force localPath remoteProjectAndBranch
    -- push @some/project to .some.path (git)
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (WriteRemoteNamespaceGit namespace) -> do
      ProjectAndBranch project branch <- ProjectUtils.expectProjectAndBranchByTheseNames localProjectAndBranch0
      pushLooseCodeToGitLooseCode
        (ProjectUtils.projectBranchPath (ProjectAndBranch (project ^. #projectId) (branch ^. #branchId)))
        namespace
        pushBehavior
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
      pushProjectBranchToProjectBranch force localProjectAndBranch (Just remoteProjectAndBranch)
  where
    force =
      case pushBehavior of
        PushBehavior.ForcePush -> True
        PushBehavior.RequireEmpty -> False
        PushBehavior.RequireNonEmpty -> False

-- Push a local namespace ("loose code") to a Git-hosted remote namespace ("loose code").
pushLooseCodeToGitLooseCode :: Path.Absolute -> WriteGitRemoteNamespace -> PushBehavior -> Cli ()
pushLooseCodeToGitLooseCode localPath gitRemotePath pushBehavior = do
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
  let behavior =
        case pushBehavior of
          PushBehavior.ForcePush -> GitPushBehaviorForce
          PushBehavior.RequireEmpty -> GitPushBehaviorFf
          PushBehavior.RequireNonEmpty -> GitPushBehaviorFf
  Cli.Env {codebase} <- ask
  let push =
        Codebase.pushGitBranch
          codebase
          (gitRemotePath ^. #repo)
          behavior
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
pushLooseCodeToShareLooseCode _ _ _ = do
  Cli.returnEarly LooseCodePushDeprecated

-- Push a local namespace ("loose code") to a remote project branch.
pushLooseCodeToProjectBranch :: Bool -> Path.Absolute -> ProjectAndBranch ProjectName ProjectBranchName -> Cli ()
pushLooseCodeToProjectBranch force localPath remoteProjectAndBranch = do
  _ <- AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  localBranchHead <-
    Cli.runTransactionWithRollback \rollback -> do
      loadCausalHashToPush localPath >>= \case
        Nothing -> rollback (EmptyLooseCodePush (Path.absoluteToPath' localPath))
        Just hash -> pure hash

  uploadPlan <- pushToProjectBranch0 force PushingLooseCode localBranchHead remoteProjectAndBranch
  executeUploadPlan uploadPlan

-- | Push a local project branch to a remote project branch. If the remote project branch is left unspecified, we either
-- use a pre-existing mapping for the local branch, or else infer what remote branch to push to (possibly creating it).
pushProjectBranchToProjectBranch ::
  Bool ->
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Maybe (These ProjectName ProjectBranchName) ->
  Cli ()
pushProjectBranchToProjectBranch force localProjectAndBranch maybeRemoteProjectAndBranchNames = do
  _ <- AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  let localProjectAndBranchIds = localProjectAndBranch & over #project (view #projectId) & over #branch (view #branchId)
  let localProjectAndBranchNames = localProjectAndBranch & over #project (view #name) & over #branch (view #name)

  -- Load local project and branch from database and get the causal hash to push
  (localProjectAndBranch, localBranchHead) <-
    Cli.runTransactionWithRollback \rollback -> do
      hash <-
        loadCausalHashToPush (ProjectUtils.projectBranchPath localProjectAndBranchIds) & onNothingM do
          rollback (EmptyProjectBranchPush localProjectAndBranchNames)
      localProjectAndBranch <- expectProjectAndBranch localProjectAndBranchIds
      pure (localProjectAndBranch, hash)

  uploadPlan <-
    case maybeRemoteProjectAndBranchNames of
      Nothing ->
        pushProjectBranchToProjectBranch'InferredProject
          force
          localProjectAndBranch
          localBranchHead
          Nothing
      Just (This remoteProjectName) ->
        pushProjectBranchToProjectBranch'IgnoreRemoteMapping
          force
          localProjectAndBranch
          localBranchHead
          (ProjectAndBranch (Just remoteProjectName) Nothing)
      Just (That remoteBranchName) ->
        pushProjectBranchToProjectBranch'InferredProject
          force
          localProjectAndBranch
          localBranchHead
          (Just remoteBranchName)
      Just (These remoteProjectName remoteBranchName) ->
        pushToProjectBranch0
          force
          (PushingProjectBranch localProjectAndBranch)
          localBranchHead
          (ProjectAndBranch remoteProjectName remoteBranchName)

  executeUploadPlan uploadPlan

-- "push" or "push /foo", remote mapping unknown
pushProjectBranchToProjectBranch'InferredProject ::
  Bool ->
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Hash32 ->
  Maybe ProjectBranchName ->
  Cli UploadPlan
pushProjectBranchToProjectBranch'InferredProject force localProjectAndBranch localBranchHead maybeRemoteBranchName = do
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
    Nothing ->
      pushProjectBranchToProjectBranch'IgnoreRemoteMapping
        force
        localProjectAndBranch
        localBranchHead
        (ProjectAndBranch Nothing maybeRemoteBranchName)
    Just (remoteProjectId, remoteProjectName, maybeRemoteBranchInfo) ->
      case maybeRemoteBranchName of
        Nothing -> do
          case maybeRemoteBranchInfo of
            -- "push" with remote mapping for project from ancestor branch
            Nothing -> do
              myUserHandle <- view #handle <$> AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
              let localBranchName = localProjectAndBranch ^. #branch . #name
              let remoteBranchName = deriveRemoteBranchName myUserHandle localBranchName
              pushToProjectBranch1
                force
                localProjectAndBranch
                localBranchHead
                (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
            -- "push" with remote mapping for branch
            Just (remoteBranchId, remoteBranchName) -> do
              let remoteProjectBranchDoesntExist = do
                    Cli.runTransaction $
                      Queries.deleteBranchRemoteMapping
                        localProjectId
                        localBranchId
                        Share.hardCodedUri
                    Cli.returnEarly $
                      Output.RemoteProjectBranchDoesntExist'Push
                        Share.hardCodedUri
                        (ProjectAndBranch remoteProjectName remoteBranchName)
              Share.getProjectBranchById Share.NoSquashedHead (ProjectAndBranch remoteProjectId remoteBranchId) >>= \case
                Share.GetProjectBranchResponseBranchNotFound -> remoteProjectBranchDoesntExist
                Share.GetProjectBranchResponseProjectNotFound -> remoteProjectBranchDoesntExist
                Share.GetProjectBranchResponseSuccess remoteBranch -> do
                  afterUploadAction <-
                    makeSetHeadAfterUploadAction
                      force
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
            force
            localProjectAndBranch
            localBranchHead
            (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
  where
    localProjectId = localProjectAndBranch ^. #project . #projectId
    localBranchId = localProjectAndBranch ^. #branch . #branchId

-- "push", "push foo", or "push /foo" ignoring remote mapping (if any)
pushProjectBranchToProjectBranch'IgnoreRemoteMapping ::
  Bool ->
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Hash32 ->
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli UploadPlan
pushProjectBranchToProjectBranch'IgnoreRemoteMapping
  force
  localProjectAndBranch
  localBranchHead
  remoteProjectAndBranchMaybes = do
    myUserHandle <- view #handle <$> AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
    let localProjectName = localProjectAndBranch ^. #project . #name
    let localBranchName = localProjectAndBranch ^. #branch . #name
    let remoteProjectName =
          case remoteProjectAndBranchMaybes ^. #project of
            Nothing -> prependUserSlugToProjectName myUserHandle localProjectName
            Just remoteProjectName1 -> remoteProjectName1
    let remoteBranchName =
          case remoteProjectAndBranchMaybes ^. #branch of
            Nothing -> deriveRemoteBranchName myUserHandle localBranchName
            Just remoteBranchName1 -> remoteBranchName1
    let remoteProjectAndBranch = ProjectAndBranch remoteProjectName remoteBranchName
    pushToProjectBranch0 force (PushingProjectBranch localProjectAndBranch) localBranchHead remoteProjectAndBranch

-- If left unspecified (and we don't yet have a remote mapping), we derive the remote branch name from the user's
-- handle and local branch name as follows:
--
--   * If the local branch name already has a user slug prefix or a (draft) release prefix, then we leave it alone.
--   * Otherwise, if the local branch name is "main", then we leave it alone.
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
deriveRemoteBranchName :: Text -> ProjectBranchName -> ProjectBranchName
deriveRemoteBranchName userHandle localBranchName =
  case classifyProjectBranchName localBranchName of
    ProjectBranchNameKind'Contributor _ _ -> localBranchName
    ProjectBranchNameKind'DraftRelease _ -> localBranchName
    ProjectBranchNameKind'Release _ -> localBranchName
    ProjectBranchNameKind'NothingSpecial
      | localBranchName == unsafeFrom @Text "main" -> localBranchName
      | otherwise ->
          (UnsafeProjectBranchName . Text.Builder.run . fold)
            [ Text.Builder.char '@',
              Text.Builder.text userHandle,
              Text.Builder.char '/',
              Text.Builder.text (into @Text localBranchName)
            ]

-- What are we pushing, a project branch or loose code?
data WhatAreWePushing
  = PushingProjectBranch (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
  | PushingLooseCode

-- we have the remote project and branch names, but we don't know whether either already exist
pushToProjectBranch0 ::
  Bool ->
  WhatAreWePushing ->
  Hash32 ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli UploadPlan
pushToProjectBranch0 force pushing localBranchHead remoteProjectAndBranch = do
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
      Share.getProjectBranchByName Share.NoSquashedHead (remoteProjectAndBranch & #project .~ remoteProjectId) >>= \case
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
          afterUploadAction <- makeSetHeadAfterUploadAction force pushing localBranchHead remoteBranch
          pure
            UploadPlan
              { remoteBranch = remoteProjectAndBranch,
                causalHash = localBranchHead,
                afterUploadAction
              }

-- "push /foo" with a remote mapping for the project (either from this branch or one of our ancestors)
-- but we don't know whether the remote branch exists
pushToProjectBranch1 ::
  Bool ->
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Hash32 ->
  ProjectAndBranch (RemoteProjectId, ProjectName) ProjectBranchName ->
  Cli UploadPlan
pushToProjectBranch1 force localProjectAndBranch localBranchHead remoteProjectAndBranch = do
  Share.getProjectBranchByName Share.NoSquashedHead (over #project fst remoteProjectAndBranch) >>= \case
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
        makeSetHeadAfterUploadAction force (PushingProjectBranch localProjectAndBranch) localBranchHead remoteBranch
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
              (Share.RepoInfo (into @Text (ProjectAndBranch (remoteBranch ^. #project) (remoteBranch ^. #branch))))
              (Set.NonEmpty.singleton causalHash)
              uploadedCallback
      upload & onLeftM \err0 -> do
        (Cli.returnEarly . Output.ShareError) case err0 of
          Share.SyncError err -> ShareErrorUploadEntities err
          Share.TransportError err -> ShareErrorTransport err
      liftIO getNumUploaded
  Cli.respond (Output.UploadedEntities numUploaded)
  afterUploadAction
  let ProjectAndBranch projectName branchName = remoteBranch
  Cli.respond (ViewOnShare (Right (Share.hardCodedUri, projectName, branchName)))

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

-- We intend to push to a remote branch.
--
-- There are two last checks to do that may cause this action to short-circuit:
--
--   1. If the remote branch head is equal to the hash we intend to set it to, then there's nothing to upload.
--
--   2. If the remote branch head is ahead of the hash we intend to fast-forward it to, and this isn't a force-push,
--      then we will refuse to push (until we implement some syntax for a force-push).
makeSetHeadAfterUploadAction ::
  Bool ->
  WhatAreWePushing ->
  Hash32 ->
  Share.RemoteProjectBranch ->
  Cli AfterUploadAction
makeSetHeadAfterUploadAction force pushing localBranchHead remoteBranch = do
  let remoteProjectAndBranchNames = ProjectAndBranch remoteBranch.projectName remoteBranch.branchName

  when (localBranchHead == Share.API.hashJWTHash remoteBranch.branchHead) do
    Cli.respond (RemoteProjectBranchIsUpToDate Share.hardCodedUri remoteProjectAndBranchNames)
    Cli.returnEarly (ViewOnShare (Right (Share.hardCodedUri, remoteBranch.projectName, remoteBranch.branchName)))


  when (not force) do
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
    let onSuccess =
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
    Share.setProjectBranchHead request >>= \case
      Share.SetProjectBranchHeadResponseSuccess -> onSuccess
      -- Sometimes a different request gets through in between checking the remote head and
      -- executing the check-and-set push, if it managed to set the head to what we wanted
      -- then the goal was achieved and we can consider it a success.
      Share.SetProjectBranchHeadResponseExpectedCausalHashMismatch _expected actual
        | actual == localBranchHead -> onSuccess
      Share.SetProjectBranchHeadResponseExpectedCausalHashMismatch _expected _actual ->
        Cli.returnEarly (RemoteProjectBranchHeadMismatch Share.hardCodedUri remoteProjectAndBranchNames)
      Share.SetProjectBranchHeadResponseNotFound -> do
        Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri remoteProjectAndBranchNames)
      Share.SetProjectBranchHeadResponseDeprecatedReleaseIsImmutable -> do
        Cli.returnEarly (Output.RemoteProjectReleaseIsDeprecated Share.hardCodedUri remoteProjectAndBranchNames)
      Share.SetProjectBranchHeadResponsePublishedReleaseIsImmutable -> do
        Cli.returnEarly (Output.RemoteProjectPublishedReleaseCannotBeChanged Share.hardCodedUri remoteProjectAndBranchNames)
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
  Operations.loadCausalHashAtPath Nothing segments <&> \case
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
