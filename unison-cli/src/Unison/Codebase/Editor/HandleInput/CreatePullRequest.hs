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
import Unison.Codebase.Editor.HandleInput.Pull (importRemoteShareBranch)
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

  let withBranch :: ReadRemoteNamespace Void -> (forall x. (Branch IO -> Cli x) -> Cli x)
      withBranch rrn k = case rrn of
        ReadRemoteNamespaceGit repo -> do
          Cli.withE (Codebase.viewRemoteBranch codebase repo Git.RequireExistingBranch) \case
            Left err -> Cli.returnEarly (Output.GitError err)
            Right x -> k x
        ReadRemoteNamespaceShare repo -> k =<< importRemoteShareBranch repo
        ReadRemoteProjectBranch _ -> wundefined

  (ppe, diff) <- withBranch (wundefined baseRepo0) \baseBranch -> withBranch (wundefined headRepo0) \headBranch -> do
    merged <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge baseBranch headBranch)
    diffHelper (Branch.head baseBranch) (Branch.head merged)
  Cli.respondNumbered (ShowDiffAfterCreatePR (wundefined baseRepo0) (wundefined headRepo0) ppe diff)
