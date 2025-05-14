-- | @pull@ input handler
module Unison.Codebase.Editor.HandleInput.Pull
  ( handlePull,
  )
where

import Control.Monad.Reader (ask)
import Data.Text qualified as Text
import Data.These
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project qualified as Sqlite (Project (..))
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.DownloadUtils
import Unison.Cli.MergeTypes (MergeSource (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.Merge2 (AliceMergeInfo (..), BobMergeInfo (..), LcaMergeInfo (..), MergeInfo (..), doMerge)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace (..), printReadRemoteNamespace)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.CommandLine.InputPatterns qualified as InputPatterns
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchNameOrLatestRelease (..), ProjectName)
import Witch (unsafeFrom)

handlePull :: PullSourceTarget -> PullMode -> Cli ()
handlePull unresolvedSourceAndTarget pullMode = do
  let includeSquashed = case pullMode of
        Input.PullWithHistory -> Share.NoSquashedHead
        Input.PullWithoutHistory -> Share.IncludeSquashedHead

  (source, target) <- resolveSourceAndTarget includeSquashed unresolvedSourceAndTarget

  remoteCausalHash <- do
    case source of
      ReadShare'LooseCode repo -> downloadLooseCodeFromShare repo & onLeftM (Cli.returnEarly . Output.ShareError)
      ReadShare'ProjectBranch remoteBranch ->
        downloadProjectBranchFromShare
          ( case pullMode of
              Input.PullWithHistory -> Share.NoSquashedHead
              Input.PullWithoutHistory -> Share.IncludeSquashedHead
          )
          remoteBranch
          & onLeftM (Cli.returnEarly . Output.ShareError)

  remoteBranchIsEmpty <-
    Cli.runTransaction do
      causal <- Operations.expectCausalBranchByCausalHash remoteCausalHash
      branch <- causal.value
      V2.Branch.isEmpty branch

  when remoteBranchIsEmpty (Cli.respond (PulledEmptyBranch source))

  let targetProjectPath = PP.projectBranchRoot (ProjectAndBranch target.project target.branch)

  let description =
        Text.unwords
          [ Text.pack . InputPattern.patternName $
              case pullMode of
                PullWithoutHistory -> InputPatterns.pullWithoutHistory
                PullWithHistory -> InputPatterns.pull,
            printReadRemoteNamespace (\remoteBranch -> into @Text (ProjectAndBranch remoteBranch.projectName remoteBranch.branchName)) source,
            case target of
              ProjectAndBranch project branch -> into @Text (ProjectAndBranch project.name branch.name)
          ]

  case pullMode of
    Input.PullWithHistory -> do
      targetBranch <- Cli.getBranchFromProjectPath targetProjectPath

      if Branch.isEmpty0 $ Branch.head targetBranch
        then do
          Cli.Env {codebase} <- ask
          remoteBranchObject <- liftIO (Codebase.expectBranchForHash codebase remoteCausalHash)
          void $ Cli.updateAtM description targetProjectPath (const $ pure remoteBranchObject)
          Cli.respond $ MergeOverEmpty target
        else do
          let aliceCausalHash = Branch.headHash targetBranch
          lcaCausalHash <- Cli.runTransaction (Operations.lca aliceCausalHash remoteCausalHash)

          doMerge
            MergeInfo
              { alice =
                  AliceMergeInfo
                    { causalHash = aliceCausalHash,
                      projectAndBranch = target
                    },
                bob =
                  BobMergeInfo
                    { causalHash = remoteCausalHash,
                      source =
                        case source of
                          ReadShare'ProjectBranch remoteBranch -> MergeSource'RemoteProjectBranch remoteBranch
                          ReadShare'LooseCode info -> MergeSource'RemoteLooseCode info
                    },
                lca =
                  LcaMergeInfo
                    { causalHash = lcaCausalHash
                    },
                description
              }
    Input.PullWithoutHistory -> do
      Cli.Env {codebase} <- ask
      remoteBranchObject <- liftIO (Codebase.expectBranchForHash codebase remoteCausalHash)

      didUpdate <-
        Cli.updateAtM
          description
          targetProjectPath
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
      ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch
    )
resolveSourceAndTarget includeSquashed = \case
  Input.PullSourceTarget0 -> liftA2 (,) (resolveImplicitSource includeSquashed) resolveImplicitTarget
  Input.PullSourceTarget1 source -> liftA2 (,) (resolveExplicitSource includeSquashed source) resolveImplicitTarget
  Input.PullSourceTarget2 source0 target0 -> do
    source <- resolveExplicitSource includeSquashed source0
    maybeTarget <-
      ProjectUtils.getProjectAndBranchByTheseNames case target0 of
        ProjectAndBranch Nothing branch -> That branch
        ProjectAndBranch (Just project) branch -> These project branch
    target <- maybeTarget & onNothing (Cli.returnEarly (Output.PullIntoMissingBranch source target0))
    pure (source, target)

resolveImplicitSource :: Share.IncludeSquashedHead -> Cli (ReadRemoteNamespace Share.RemoteProjectBranch)
resolveImplicitSource includeSquashed = do
  pp <- Cli.getCurrentProjectPath
  let localProjectAndBranch = PP.toProjectAndBranch pp
  (remoteProjectId, remoteProjectName, remoteBranchId, remoteBranchName) <-
    Cli.runTransactionWithRollback \rollback -> do
      let localProjectId = localProjectAndBranch.project.projectId
      let localBranchId = localProjectAndBranch.branch.branchId
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
  ReadShare'LooseCode namespace -> pure (ReadShare'LooseCode namespace)
  ReadShare'ProjectBranch (This remoteProjectName) -> do
    remoteProject <- ProjectUtils.expectRemoteProjectByName remoteProjectName
    let remoteProjectId = remoteProject.projectId
    let remoteBranchName = unsafeFrom @Text "main"
    remoteProjectBranch <-
      ProjectUtils.expectRemoteProjectBranchByName
        includeSquashed
        (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
    pure (ReadShare'ProjectBranch remoteProjectBranch)
  ReadShare'ProjectBranch (That branchNameOrLatestRelease) -> do
    localProjectAndBranch <- PP.toProjectAndBranch <$> Cli.getCurrentProjectPath
    let localProjectId = localProjectAndBranch.project.projectId
    let localBranchId = localProjectAndBranch.branch.branchId
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
          Output.NoAssociatedRemoteProject Share.hardCodedUri (ProjectUtils.justTheNames localProjectAndBranch)
  ReadShare'ProjectBranch (These projectName branchNameOrLatestRelease) -> do
    remoteProject <- ProjectUtils.expectRemoteProjectByName projectName
    let remoteProjectId = remoteProject.projectId
    branchName <-
      case branchNameOrLatestRelease of
        ProjectBranchNameOrLatestRelease'Name name -> pure name
        ProjectBranchNameOrLatestRelease'LatestRelease -> ProjectUtils.expectLatestReleaseBranchName remoteProject
    remoteProjectBranch <-
      ProjectUtils.expectRemoteProjectBranchByName
        includeSquashed
        (ProjectAndBranch (remoteProjectId, projectName) branchName)
    pure (ReadShare'ProjectBranch remoteProjectBranch)

resolveImplicitTarget :: Cli (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
resolveImplicitTarget = do
  PP.toProjectAndBranch <$> Cli.getCurrentProjectPath
