-- | @pull@ input handler
module Unison.Codebase.Editor.HandleInput.Pull
  ( handlePull,
    loadPropagateDiffDefaultPatch,
    mergeBranchAndPropagateDefaultPatch,
    propagatePatch,
  )
where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.Text qualified as Text
import Data.These
import U.Codebase.Sqlite.Project qualified as Sqlite (Project)
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch)
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.DownloadUtils
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.Share.Projects qualified as Share
import Unison.Cli.UnisonConfigUtils (resolveConfiguredUrl)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch (..))
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Merge qualified as Branch
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.Output.PushPull qualified as PushPull
import Unison.Codebase.Editor.Propagate qualified as Propagate
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace (..), printReadRemoteNamespace)
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Codebase.Patch (Patch (..))
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.CommandLine.InputPatterns qualified as InputPatterns
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchNameOrLatestRelease (..), ProjectName)
import Witch (unsafeFrom)

handlePull :: PullSourceTarget -> PullMode -> Verbosity.Verbosity -> Cli ()
handlePull unresolvedSourceAndTarget pullMode verbosity = do
  let includeSquashed = case pullMode of
        Input.PullWithHistory -> Share.NoSquashedHead
        Input.PullWithoutHistory -> Share.IncludeSquashedHead
  (source, target) <- resolveSourceAndTarget includeSquashed unresolvedSourceAndTarget
  remoteBranchObject <- do
    Cli.Env {codebase} <- ask
    causalHash <-
      case source of
        ReadRemoteNamespaceGit repo -> do
          downloadLooseCodeFromGitRepo
            codebase
            ( case pullMode of
                Input.PullWithHistory -> GitNamespaceHistoryTreatment'LetAlone
                Input.PullWithoutHistory -> GitNamespaceHistoryTreatment'DiscardAllHistory
            )
            repo
            & onLeftM (Cli.returnEarly . Output.GitError)
        ReadShare'LooseCode repo -> downloadLooseCodeFromShare repo & onLeftM (Cli.returnEarly . Output.ShareError)
        ReadShare'ProjectBranch remoteBranch ->
          downloadProjectBranchFromShare
            ( case pullMode of
                Input.PullWithHistory -> Share.NoSquashedHead
                Input.PullWithoutHistory -> Share.IncludeSquashedHead
            )
            remoteBranch
            & onLeftM (Cli.returnEarly . Output.ShareError)
    liftIO (Codebase.expectBranchForHash codebase causalHash)
  when (Branch.isEmpty0 (Branch.head remoteBranchObject)) do
    Cli.respond (PulledEmptyBranch source)
  targetAbsolutePath <-
    case target of
      Left path -> Cli.resolvePath' path
      Right (ProjectAndBranch project branch) ->
        pure $ ProjectUtils.projectBranchPath (ProjectAndBranch (project ^. #projectId) (branch ^. #branchId))
  let description =
        Text.unwords
          [ Text.pack . InputPattern.patternName $
              case pullMode of
                PullWithoutHistory -> InputPatterns.pullWithoutHistory
                PullWithHistory -> InputPatterns.pull,
            printReadRemoteNamespace (\remoteBranch -> into @Text (ProjectAndBranch (remoteBranch ^. #projectName) (remoteBranch ^. #branchName))) source,
            case target of
              Left path -> Path.toText' path
              Right (ProjectAndBranch project branch) -> into @Text (ProjectAndBranch (project ^. #name) (branch ^. #name))
          ]
  case pullMode of
    Input.PullWithHistory -> do
      targetBranchObject <- Cli.getBranch0At targetAbsolutePath
      if Branch.isEmpty0 targetBranchObject
        then do
          void $ Cli.updateAtM description targetAbsolutePath (const $ pure remoteBranchObject)
          Cli.respond $ MergeOverEmpty target
        else do
          Cli.respond AboutToMerge
          mergeBranchAndPropagateDefaultPatch
            Branch.RegularMerge
            description
            (Just (PullAlreadyUpToDate source target))
            remoteBranchObject
            (if Verbosity.isSilent verbosity then Nothing else Just target)
            targetAbsolutePath
    Input.PullWithoutHistory -> do
      didUpdate <-
        Cli.updateAtM
          description
          targetAbsolutePath
          (\targetBranchObject -> pure $ remoteBranchObject `Branch.consBranchSnapshot` targetBranchObject)
      Cli.respond
        if didUpdate
          then PullSuccessful source target
          else PullAlreadyUpToDate source target

resolveSourceAndTarget ::
  Share.IncludeSquashedHead ->
  PullSourceTarget ->
  Cli
    ( ReadRemoteNamespace Share.RemoteProjectBranch,
      Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
    )
resolveSourceAndTarget includeSquashed = \case
  Input.PullSourceTarget0 -> liftA2 (,) (resolveImplicitSource includeSquashed) resolveImplicitTarget
  Input.PullSourceTarget1 source -> liftA2 (,) (resolveExplicitSource includeSquashed source) resolveImplicitTarget
  Input.PullSourceTarget2 source target ->
    liftA2 (,) (resolveExplicitSource includeSquashed source) (ProjectUtils.expectLooseCodeOrProjectBranch target)

resolveImplicitSource :: Share.IncludeSquashedHead -> Cli (ReadRemoteNamespace Share.RemoteProjectBranch)
resolveImplicitSource includeSquashed =
  ProjectUtils.getCurrentProjectBranch >>= \case
    Nothing -> RemoteRepo.writeNamespaceToRead <$> resolveConfiguredUrl PushPull.Pull Path.currentPath
    Just (localProjectAndBranch, _restPath) -> do
      (remoteProjectId, remoteProjectName, remoteBranchId, remoteBranchName) <-
        Cli.runTransactionWithRollback \rollback -> do
          let localProjectId = localProjectAndBranch ^. #project . #projectId
          let localBranchId = localProjectAndBranch ^. #branch . #branchId
          Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId >>= \case
            Just (remoteProjectId, Just remoteBranchId) -> do
              remoteProjectName <- Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri
              remoteBranchName <-
                Queries.expectRemoteProjectBranchName
                  Share.hardCodedUri
                  remoteProjectId
                  remoteBranchId
              pure (remoteProjectId, remoteProjectName, remoteBranchId, remoteBranchName)
            _ -> rollback (Output.NoAssociatedRemoteProjectBranch Share.hardCodedUri localProjectAndBranch)
      remoteBranch <-
        ProjectUtils.expectRemoteProjectBranchById includeSquashed $
          ProjectAndBranch
            (remoteProjectId, remoteProjectName)
            (remoteBranchId, remoteBranchName)
      pure (ReadShare'ProjectBranch remoteBranch)

resolveExplicitSource ::
  Share.IncludeSquashedHead ->
  ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease) ->
  Cli (ReadRemoteNamespace Share.RemoteProjectBranch)
resolveExplicitSource includeSquashed = \case
  ReadRemoteNamespaceGit namespace -> pure (ReadRemoteNamespaceGit namespace)
  ReadShare'LooseCode namespace -> pure (ReadShare'LooseCode namespace)
  ReadShare'ProjectBranch (This remoteProjectName) -> do
    remoteProject <- ProjectUtils.expectRemoteProjectByName remoteProjectName
    let remoteProjectId = remoteProject ^. #projectId
    let remoteBranchName = unsafeFrom @Text "main"
    remoteProjectBranch <-
      ProjectUtils.expectRemoteProjectBranchByName
        includeSquashed
        (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
    pure (ReadShare'ProjectBranch remoteProjectBranch)
  ReadShare'ProjectBranch (That branchNameOrLatestRelease) -> do
    (ProjectAndBranch localProject localBranch, _restPath) <- ProjectUtils.expectCurrentProjectBranch
    let localProjectId = localProject ^. #projectId
    let localBranchId = localBranch ^. #branchId
    Cli.runTransaction (Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId) >>= \case
      Just (remoteProjectId, _maybeProjectBranchId) -> do
        remoteProjectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
        remoteBranchName <-
          case branchNameOrLatestRelease of
            ProjectBranchNameOrLatestRelease'Name name -> pure name
            ProjectBranchNameOrLatestRelease'LatestRelease -> do
              remoteProject <- ProjectUtils.expectRemoteProjectById remoteProjectId remoteProjectName
              ProjectUtils.expectLatestReleaseBranchName remoteProject
        remoteProjectBranch <-
          ProjectUtils.expectRemoteProjectBranchByName
            includeSquashed
            (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
        pure (ReadShare'ProjectBranch remoteProjectBranch)
      Nothing -> do
        Cli.returnEarly $
          Output.NoAssociatedRemoteProject
            Share.hardCodedUri
            (ProjectAndBranch (localProject ^. #name) (localBranch ^. #name))
  ReadShare'ProjectBranch (These projectName branchNameOrLatestRelease) -> do
    remoteProject <- ProjectUtils.expectRemoteProjectByName projectName
    let remoteProjectId = remoteProject ^. #projectId
    branchName <-
      case branchNameOrLatestRelease of
        ProjectBranchNameOrLatestRelease'Name name -> pure name
        ProjectBranchNameOrLatestRelease'LatestRelease -> ProjectUtils.expectLatestReleaseBranchName remoteProject
    remoteProjectBranch <-
      ProjectUtils.expectRemoteProjectBranchByName
        includeSquashed
        (ProjectAndBranch (remoteProjectId, projectName) branchName)
    pure (ReadShare'ProjectBranch remoteProjectBranch)

resolveImplicitTarget :: Cli (Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
resolveImplicitTarget =
  ProjectUtils.getCurrentProjectBranch <&> \case
    Nothing -> Left Path.currentPath
    Just (projectAndBranch, _restPath) -> Right projectAndBranch

-- | supply `dest0` if you want to print diff messages
--   supply unchangedMessage if you want to display it if merge had no effect
mergeBranchAndPropagateDefaultPatch ::
  Branch.MergeMode ->
  Text ->
  Maybe Output ->
  Branch IO ->
  Maybe (Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)) ->
  Path.Absolute ->
  Cli ()
mergeBranchAndPropagateDefaultPatch mode inputDescription unchangedMessage srcb maybeDest0 dest =
  ifM
    mergeBranch
    (loadPropagateDiffDefaultPatch inputDescription maybeDest0 dest)
    (for_ unchangedMessage Cli.respond)
  where
    mergeBranch :: Cli Bool
    mergeBranch =
      Cli.time "mergeBranch" do
        Cli.Env {codebase} <- ask
        destb <- Cli.getBranchAt dest
        merged <- liftIO (Branch.merge'' (Codebase.lca codebase) mode srcb destb)
        b <- Cli.updateAtM inputDescription dest (const $ pure merged)
        for_ maybeDest0 \dest0 -> do
          (ppe, diff) <- diffHelper (Branch.head destb) (Branch.head merged)
          Cli.respondNumbered (ShowDiffAfterMerge dest0 dest ppe diff)
        pure b

loadPropagateDiffDefaultPatch ::
  Text ->
  Maybe (Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)) ->
  Path.Absolute ->
  Cli ()
loadPropagateDiffDefaultPatch inputDescription maybeDest0 dest = do
  Cli.respond Output.AboutToPropagatePatch
  Cli.time "loadPropagateDiffDefaultPatch" do
    original <- Cli.getBranch0At dest
    patch <- liftIO $ Branch.getPatch NameSegment.defaultPatchSegment original
    patchDidChange <- propagatePatch inputDescription patch dest
    when patchDidChange do
      whenJust maybeDest0 \dest0 -> do
        Cli.respond Output.CalculatingDiff
        patched <- Cli.getBranchAt dest
        let patchPath = Path.Path' (Right (Path.Relative (Path.fromList [NameSegment.defaultPatchSegment])))
        (ppe, diff) <- diffHelper original (Branch.head patched)
        Cli.respondNumbered (ShowDiffAfterMergePropagate dest0 dest patchPath ppe diff)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatch ::
  Text ->
  Patch ->
  Path.Absolute ->
  Cli Bool
propagatePatch inputDescription patch scopePath = do
  Cli.time "propagatePatch" do
    Cli.stepAt'
      (inputDescription <> " (applying patch)")
      (Path.unabsolute scopePath, Propagate.propagateAndApply patch)
