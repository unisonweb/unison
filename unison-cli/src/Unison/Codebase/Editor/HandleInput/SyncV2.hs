module Unison.Codebase.Editor.HandleInput.SyncV2
  ( handleSyncToFile,
    handleSyncFromFile,
    handleSyncFromCodebase,
    handleSyncFromCodeserver,
  )
where

import Control.Lens
import Control.Monad.Reader (MonadReader (..))
import Data.These (These (..))
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Project qualified as Projects
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.DownloadUtils (downloadProjectBranchFromShare)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Project
import Unison.Cli.Share.Projects qualified as ShareProjects
import Unison.Codebase (CodebasePath)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput.Branch (CreateFrom (..))
import Unison.Codebase.Editor.HandleInput.Branch qualified as Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Init qualified as Init
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.SqliteCodebase qualified as SqliteCodebase
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Share.SyncV2 qualified as SyncV2
import Unison.SyncV2.Types (BranchRef)

handleSyncToFile :: FilePath -> ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) -> Cli ()
handleSyncToFile destSyncFile branchToSync = do
  pp <- Cli.getCurrentProjectPath
  projectBranch <- Project.resolveProjectBranchInProject (pp ^. #project) (over #branch (Just . fromMaybe (pp.branch.name)) branchToSync)
  causalHash <- Cli.runTransaction $ Project.getProjectBranchCausalHash (projectBranch ^. #branch)
  let branchRef = into @BranchRef $ ProjectAndBranch (projectBranch ^. #project . #name) (projectBranch ^. #branch . #name)
  Cli.Env {codebase} <- ask
  liftIO (SyncV2.syncToFile codebase causalHash (Just branchRef) destSyncFile) >>= \case
    Left err -> Cli.respond (Output.SyncPullError err)
    Right _ -> pure ()

handleSyncFromFile :: Text -> FilePath -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleSyncFromFile description srcSyncFile destBranch = do
  let shouldValidate = True
  SyncV2.syncFromFile shouldValidate srcSyncFile >>= \case
    Left err -> Cli.respond (Output.SyncPullError err)
    Right (causalHash, _chId) -> do
      createOrUpdateBranch description destBranch causalHash

handleSyncFromCodebase :: Text -> CodebasePath -> ProjectAndBranch ProjectName ProjectBranchName -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleSyncFromCodebase description srcCodebasePath srcBranch destBranch = do
  Cli.Env {codebase} <- ask
  r <- liftIO $ Init.withOpenCodebase SqliteCodebase.init "sync-src" srcCodebasePath Init.DontLock (Init.MigrateAfterPrompt Init.Backup Init.Vacuum) \srcCodebase -> do
    Codebase.withConnection srcCodebase \srcConn -> do
      maySrcCausalHash <- Codebase.runTransaction srcCodebase $ do
        let ProjectAndBranch srcProjName srcBranchName = srcBranch
        runMaybeT do
          project <- MaybeT (Q.loadProjectByName srcProjName)
          branch <- MaybeT (Q.loadProjectBranchByName (project ^. #projectId) srcBranchName)
          lift $ Project.getProjectBranchCausalHash branch
      case maySrcCausalHash of
        Nothing -> pure $ Left (Output.SyncFromCodebaseMissingProjectBranch srcBranch)
        Just srcCausalHash -> do
          let shouldValidate = True
          Right . fmap (const srcCausalHash) <$> liftIO (SyncV2.syncFromCodebase shouldValidate srcConn codebase srcCausalHash)

  case r of
    Left openCodebaseErr -> Cli.respond (Output.OpenCodebaseError srcCodebasePath openCodebaseErr)
    Right (Left errOutput) -> Cli.respond errOutput
    Right (Right (Right causalHash)) -> do
      createOrUpdateBranch description destBranch causalHash
    Right (Right (Left syncErr)) -> do
      Cli.respond (Output.SyncPullError syncErr)

createOrUpdateBranch :: Text -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> CausalHash -> Cli ()
createOrUpdateBranch description destBranch causalHash = do
  theseNames <- case destBranch of
    ProjectAndBranch (Just projName) branchName -> do
      pure (These projName branchName)
    ProjectAndBranch Nothing branchName -> do
      pure (That branchName)
  Project.getProjectAndBranchByTheseNames theseNames >>= \case
    Just projectBranch -> do
      Cli.setProjectBranchRootToCausalHash (projectBranch ^. #branch) description causalHash
      Cli.switchProject (bimap Projects.projectId branchId projectBranch)
    Nothing -> do
      let createFrom = CreateFrom'CausalHash causalHash
      pp <- Cli.getCurrentProjectPath
      void $ Branch.createBranch description createFrom pp.project (pure destBranch.branch)

handleSyncFromCodeserver :: ShareProjects.IncludeSquashedHead -> ShareProjects.RemoteProjectBranch -> Cli (Either Output.ShareError CausalHash)
handleSyncFromCodeserver = downloadProjectBranchFromShare
