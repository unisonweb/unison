module Unison.Codebase.Editor.HandleInput.LoadPullRequest
  ( handleLoadPullRequest,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Data.These (These)
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Codebase (Preprocessing (..))
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import Unison.Codebase.Editor.HandleInput.Pull (importRemoteShareBranch, loadPropagateDiffDefaultPatch)
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace (..))
import Unison.Codebase.Path (Path' (..))
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Prelude
import Unison.Project (ProjectBranchName, ProjectName)

handleLoadPullRequest ::
  Text ->
  (ReadRemoteNamespace (These ProjectName ProjectBranchName)) ->
  (ReadRemoteNamespace (These ProjectName ProjectBranchName)) ->
  Path' ->
  Cli ()
handleLoadPullRequest description baseRepo headRepo dest0 = do
  Cli.assertNoBranchAtPath' dest0
  Cli.Env {codebase} <- ask
  destAbs <- Cli.resolvePath' dest0
  let getBranch = \case
        ReadRemoteNamespaceGit repo ->
          Cli.ioE (Codebase.importRemoteBranch codebase repo SyncMode.ShortCircuit Unmodified) \err ->
            Cli.returnEarly (Output.GitError err)
        ReadRemoteNamespaceShare repo -> importRemoteShareBranch repo
        ReadShareProjectBranch _ -> wundefined
  baseb <- getBranch baseRepo
  headb <- getBranch headRepo
  mergedb <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge baseb headb)
  squashedb <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.SquashMerge headb baseb)
  Cli.updateAt description destAbs $ Branch.step \destBranch0 ->
    destBranch0
      & Branch.children
        %~ ( \childMap ->
               childMap
                 & at "base" ?~ baseb
                 & at "head" ?~ headb
                 & at "merged" ?~ mergedb
                 & at "squashed" ?~ squashedb
           )
  let base = snoc dest0 "base"
      head = snoc dest0 "head"
      merged = snoc dest0 "merged"
      squashed = snoc dest0 "squashed"
  Cli.respond $ LoadPullRequest (wundefined baseRepo) (wundefined headRepo) base head merged squashed
  loadPropagateDiffDefaultPatch
    description
    (Just merged)
    (snoc destAbs "merged")
