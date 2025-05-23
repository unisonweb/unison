-- | @branch.squash@ input handler
module Unison.Codebase.Editor.HandleInput.BranchSquash (handleBranchSquash) where

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
import Unison.Codebase.Editor.HandleInput.Branch qualified as Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Sqlite qualified as Sqlite

data SquashDestination
  = -- Update an existing branch with the squash result
    ExistingBranch (ProjectAndBranch Project ProjectBranch)
  | -- Create a new branch from the squashed branch
    NewBranch ProjectUtils.Project ProjectBranchName

handleBranchSquash :: (ProjectAndBranch (Maybe ProjectName) ProjectBranchName) -> (ProjectAndBranch (Maybe ProjectName) ProjectBranchName) -> Cli ()
handleBranchSquash branchToSquash mayDestBranch = do
  currentProj <- Cli.getCurrentProject
  let sourceNames = case branchToSquash of
        ProjectAndBranch (Just project) branch -> These project branch
        ProjectAndBranch Nothing branch -> That branch
  sourcePAB <- ProjectUtils.expectProjectAndBranchByTheseNames sourceNames
  causalBranchToSquash <- Cli.runTransaction $ Codebase.expectProjectBranchRootCausal sourcePAB.branch
  let destNames = case mayDestBranch of
        ProjectAndBranch (Just project) branch -> These project branch
        ProjectAndBranch Nothing branch -> That branch
  destProj <-
    fromMaybe currentProj <$> runMaybeT do
      projName <- hoistMaybe branchToSquash.project
      MaybeT $ Project.getProjectByName projName
  mayDestPAB <- ProjectUtils.getProjectAndBranchByTheseNames destNames
  let squashDest = case mayDestPAB of
        Just destPAB -> ExistingBranch destPAB
        Nothing -> NewBranch destProj mayDestBranch.branch

  squashResult <- Cli.runTransaction $ squashCausal causalBranchToSquash
  let description = "Squashed from " <> tShow causalBranchToSquash.causalHash
  case squashDest of
    NewBranch project newBranchName -> do
      (newPAB, _) <- Branch.createBranch description (Branch.CreateFrom'CausalHash squashResult.causalHash) project (pure newBranchName)
      destBranch <- Cli.runTransaction $ ProjectUtils.expectProjectAndBranchByIds newPAB
      Cli.respond $ Output.BranchSquashSuccess sourcePAB destBranch
      pure ()
    ExistingBranch destBranch -> do
      Cli.setProjectBranchRootToCausalHash destBranch.branch description squashResult.causalHash
      Cli.respond $ Output.BranchSquashSuccess sourcePAB destBranch
  pure ()

squashCausal :: V2Branch.CausalBranch Sqlite.Transaction -> Sqlite.Transaction (V2Branch.CausalBranch Sqlite.Transaction)
squashCausal causalBranch = do
  UCausal.squashCausal HH.v2HashHandle causalBranch
