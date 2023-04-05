module Unison.Codebase.Editor.HandleInput.CreatePullRequest
  ( handleCreatePullRequest,
  )
where

import Control.Monad.Reader (ask)
import Data.These (These)
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.HandleInput.Pull (loadShareLooseCodeIntoMemory)
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace (..))
import Unison.Prelude
import Unison.Project (ProjectBranchName, ProjectName)

handleCreatePullRequest ::
  ReadRemoteNamespace (These ProjectName ProjectBranchName) ->
  ReadRemoteNamespace (These ProjectName ProjectBranchName) ->
  Cli ()
handleCreatePullRequest baseRepo0 headRepo0 = do
  Cli.Env {codebase} <- ask

  (baseRepo, headRepo) <-
    let rejectProjectBranch ::
          ReadRemoteNamespace (These ProjectName ProjectBranchName) ->
          Cli (ReadRemoteNamespace Void)
        rejectProjectBranch = \case
          ReadShare'ProjectBranch {} ->
            Cli.returnEarly (Output.NotImplementedYet "creating a pull request from a project branch")
          ReadRemoteNamespaceGit namespace -> pure (ReadRemoteNamespaceGit namespace)
          ReadShare'LooseCode namespace -> pure (ReadShare'LooseCode namespace)
     in (,) <$> rejectProjectBranch baseRepo0 <*> rejectProjectBranch headRepo0

  let withBranch :: ReadRemoteNamespace Void -> (forall x. (Branch IO -> Cli x) -> Cli x)
      withBranch rrn k = case rrn of
        ReadRemoteNamespaceGit repo -> do
          Cli.withE (Codebase.viewRemoteBranch codebase repo Git.RequireExistingBranch) \case
            Left err -> Cli.returnEarly (Output.GitError err)
            Right x -> k x
        ReadShare'LooseCode repo -> k =<< loadShareLooseCodeIntoMemory repo

  (ppe, diff) <- withBranch baseRepo \baseBranch -> withBranch headRepo \headBranch -> do
    merged <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge baseBranch headBranch)
    diffHelper (Branch.head baseBranch) (Branch.head merged)
  Cli.respondNumbered (ShowDiffAfterCreatePR baseRepo headRepo ppe diff)
