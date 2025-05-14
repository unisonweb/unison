-- | @branch.squash@ input handler
module Unison.Codebase.Editor.HandleInput.BranchSquash (handleBranchSquash) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.These (These (..))
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal (Causal (..))
import U.Codebase.Causal.Squash qualified as UCausal
import U.Codebase.Sqlite.V2.HashHandle qualified as HH
import Unison.Cli.Monad
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Project
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch qualified as Branch
import Unison.Codebase.Editor.Input (BranchId2)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Sqlite qualified as Sqlite

handleBranchSquash :: Maybe BranchId2 -> Maybe ProjectBranchName -> Cli ()
handleBranchSquash mayBranchToSquash mayDestBranch = label \done -> do
  causalBranchToSquash <-
    case mayBranchToSquash of
      Nothing -> do
        -- If no branch is specified, we assume the current branch
        causalHash <- Branch.headHash <$> Cli.getCurrentBranch
        Cli.runTransaction $ Codebase.expectCausalBranchByCausalHash causalHash
      Just (Left shortHash) -> Cli.runTransactionWithRollback \rollback -> do
        causalHash <- Cli.resolveShortCausalHashToCausalHash rollback shortHash
        Codebase.expectCausalBranchByCausalHash causalHash
      Just (Right path') -> do
        srcPP <- ProjectUtils.resolveBranchRelativePath path'
        Cli.runTransaction (Codebase.getMaybeShallowCausalAtProjectPath srcPP) >>= \case
          Nothing -> do
            Cli.respond $ Output.NamespaceEmpty (NonEmpty.singleton $ Right srcPP)
            done ()
          Just causal -> pure $ causal
  squashedCausal <- Cli.runTransaction $ squashCausal causalBranchToSquash
  -- Check if dest branch already exists
  mayExistingDest <- runMaybeT do
    destBranchName <- hoistMaybe mayDestBranch
    existingProjectBranch <- MaybeT $ Project.getProjectAndBranchByTheseNames (That destBranchName)
    existingCausalHash <- lift $ Cli.runTransaction (Project.getProjectBranchCausalHash existingProjectBranch.branch)
    pure (existingProjectBranch, existingCausalHash)
  let description = undefined
  let project = undefined
  let getBranchName = undefined
  case mayExistingDest of
    Nothing -> do
      _ <- Branch.createBranch description (Branch.CreateFrom'CausalHash squashedCausal.causalHash) project getBranchName
      pure ()
    Just (existingDestProjectBranch, existingDestBranchCausalHash) -> do
      Cli.setProjectBranchRootToCausalHash existingDestProjectBranch.branch description existingDestBranchCausalHash
  pure ()

squashCausal :: V2Branch.CausalBranch Sqlite.Transaction -> Sqlite.Transaction (V2Branch.CausalBranch Sqlite.Transaction)
squashCausal causalBranch = do
  UCausal.squashCausal HH.v2HashHandle causalBranch
