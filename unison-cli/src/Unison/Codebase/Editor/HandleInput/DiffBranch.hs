module Unison.Codebase.Editor.HandleInput.DiffBranch
  ( handleDiffBranch,
  )
where

import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.These (These (..))
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project qualified as Sqlite
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.UpdateUtils qualified as UpdateUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Input (DiffBranchArg (..))
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.DeclCoherencyCheck (asOneRandomIncoherentDeclReason)
import Unison.Merge qualified as Merge
import Unison.Merge.ThreeWay qualified as Merge.ThreeWay
import Unison.Merge.TwoOrThreeWay qualified as TwoOrThreeWay
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Defns (Defns (..), defnsAreEmpty)

handleDiffBranch :: DiffBranchArg -> DiffBranchArg -> Cli ()
handleDiffBranch aliceArg bobArg = do
  env <- ask

  currentProject <- Cli.getCurrentProject

  Cli.runTransactionWithRollback \abort -> do
    aliceCausalHash <- resolveDiffBranchArg abort currentProject aliceArg
    bobCausalHash <- resolveDiffBranchArg abort currentProject bobArg
    lcaCausalHash <- Operations.lca aliceCausalHash bobCausalHash
    let causalHashes0 =
          Merge.TwoOrThreeWay
            { alice = aliceCausalHash,
              bob = bobCausalHash,
              lca = lcaCausalHash
            }

    -- Temporary restriction: we just don't support diffing unrelated branches
    -- In the future: we think we want to set LCA=Alice in this case?

    causalHashes <-
      TwoOrThreeWay.toThreeWayA
        (abort (Output.Literal "TODO"))
        causalHashes0

    namespaces <-
      for causalHashes (Codebase.expectBranchForHashTx env.codebase)

    let namespaces0 =
          Branch.head <$> namespaces

    defns <-
      for namespaces0 \namespace ->
        Branch.asUnconflicted namespace
          & onLeft (abort . Output.ConflictedDefn)

    declNameLookups <- do
      aliceAndBob <-
        sequence $
          ( \x y z ->
              Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash x) y
                & onLeftM
                  ( abort
                      . Output.IncoherentDeclDuringDiffBranch z
                      . asOneRandomIncoherentDeclReason
                  )
          )
            <$> Merge.ThreeWay.forgetLca namespaces
            <*> Merge.ThreeWay.forgetLca defns
            <*> Merge.TwoWay {alice = aliceArg, bob = bobArg}
      lca <- Codebase.getBranchPartialDeclNameLookup env.codebase (Branch.namespaceHash namespaces.lca) defns.lca
      pure (Merge.ThreeWay.gfromTwoWay lca aliceAndBob)

    diffblob <-
      Merge.makeDiffblob
        Merge.emptyDiffblobLog
        ( \refs0 ->
            let refs = fold refs0
             in if defnsAreEmpty refs
                  then pure (Defns Map.empty Map.empty)
                  else UpdateUtils.hydrateRefs env.codebase refs
        )
        (\_ -> pure (Branch.toNames <$> namespaces0))
        defns
        (view Branch.libdeps_ <$> namespaces0)
        declNameLookups

    wundefined

  wundefined

resolveDiffBranchArg ::
  (forall void. Output -> Sqlite.Transaction void) ->
  Sqlite.Project ->
  DiffBranchArg ->
  Sqlite.Transaction CausalHash
resolveDiffBranchArg abort currentProject = \case
  DiffBranchArg'Branch names -> do
    projectAndBranch <-
      ProjectUtils.expectProjectAndBranchByTheseNamesTx abort currentProject case names.project of
        Nothing -> That names.branch
        Just projectName -> These projectName names.branch
    ProjectUtils.getProjectBranchCausalHash projectAndBranch.branch
  DiffBranchArg'Hash hash -> Cli.resolveShortCausalHashToCausalHash abort hash
