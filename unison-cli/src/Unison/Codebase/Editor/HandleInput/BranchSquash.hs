-- | @branch.squash@ input handler
module Unison.Codebase.Editor.HandleInput.BranchSquash (handleBranchSquash) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.These (These (..))
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal (Causal (..))
import U.Codebase.Causal.Squash qualified as UCausal
import U.Codebase.Sqlite.Project (Project)
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch)
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
import Unison.Codebase.ProjectPath qualified as ProjPath
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Sqlite qualified as Sqlite

data SquashDestination
  = -- Update an existing branch with the squash result
    ExistingBranch (ProjectAndBranch Project ProjectBranch)
  | -- Create a new branch from the squashed branch
    NewBranch ProjectUtils.Project ProjectBranchName
  | -- Squash and save, then just print the causal hash.
    Floating

handleBranchSquash :: Maybe BranchId2 -> Maybe ProjectBranchName -> Cli ()
handleBranchSquash mayBranchToSquash mayDestBranch = label \done -> do
  (source, causalBranchToSquash, squashDest) <-
    case (mayBranchToSquash, mayDestBranch) of
      (Nothing, mayDestBranch) -> do
        -- If no src or dest is specified, we assume the current branch for both
        causalHash <- Branch.headHash <$> Cli.getCurrentBranch
        causalToSquash <- Cli.runTransaction $ Codebase.expectCausalBranchByCausalHash causalHash
        dest <- resolveMayDestBranch mayDestBranch
        currentPAB <- Cli.getCurrentProjectAndBranch
        pure $ (Right $ ProjPath.projectBranchRoot currentPAB, causalToSquash, dest)
      (Just (Left shortHash), mayDestBranch) -> do
        dest <- case mayDestBranch of
          Nothing -> do
            -- Since we're squashing a bare hash, we assume 'floating'
            pure Floating
          Just destBranchName -> resolveMayDestBranch (Just destBranchName)
        -- If we're squashing a bare hash with no destination, assume 'floating'
        Cli.runTransactionWithRollback \rollback -> do
          causalHash <- Cli.resolveShortCausalHashToCausalHash rollback shortHash
          causalToSquash <- Codebase.expectCausalBranchByCausalHash causalHash
          pure (Left shortHash, causalToSquash, dest)
      (Just (Right path'), mayDest) -> do
        dest <- resolveMayDestBranch mayDest
        srcPP <- ProjectUtils.resolveBranchRelativePath path'
        Cli.runTransaction (Codebase.getMaybeShallowCausalAtProjectPath srcPP) >>= \case
          Nothing -> do
            Cli.respond $ Output.NamespaceEmpty (NonEmpty.singleton $ Right srcPP)
            done ()
          Just causal -> pure (Right srcPP, causal, dest)

  squashResult <- Cli.runTransaction $ squashCausal causalBranchToSquash
  let description = "Squashed from " <> tShow causalBranchToSquash.causalHash
  case squashDest of
    NewBranch project newBranchName -> do
      (newPAB, _) <- Branch.createBranch description (Branch.CreateFrom'CausalHash squashResult.causalHash) project (pure newBranchName)
      dest <- Cli.runTransaction $ ProjectUtils.expectProjectAndBranchByIds newPAB
      Cli.respond $ Output.BranchSquashSuccess source (Just dest) squashResult.causalHash
      pure ()
    ExistingBranch destBranch -> do
      Cli.setProjectBranchRootToCausalHash destBranch.branch description squashResult.causalHash
      Cli.respond $ Output.BranchSquashSuccess source (Just destBranch) squashResult.causalHash
    Floating -> do
      Cli.respond $ Output.BranchSquashSuccess source Nothing squashResult.causalHash
  pure ()
  where
    resolveMayDestBranch :: Maybe ProjectBranchName -> Cli SquashDestination
    resolveMayDestBranch = \case
      Just destBranchName -> do
        Project.getProjectAndBranchByTheseNames (That destBranchName) >>= \case
          Nothing -> do
            pab <- Cli.getCurrentProjectAndBranch
            pure $ NewBranch pab.project destBranchName
          Just destPAB -> pure $ ExistingBranch destPAB
      Nothing -> do
        currentPAB <- Cli.getCurrentProjectAndBranch
        pure $ ExistingBranch currentPAB

squashCausal :: V2Branch.CausalBranch Sqlite.Transaction -> Sqlite.Transaction (V2Branch.CausalBranch Sqlite.Transaction)
squashCausal causalBranch = do
  UCausal.squashCausal HH.v2HashHandle causalBranch
