-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( currentNames,
    projectRootNames,
    projectRootNamesWithoutTransitiveLibs,
  )
where

import Control.Monad.Reader
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils (getCurrentBranch0)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Branch.Names.Cache qualified as NamesCache
import Unison.Names (Names)

-- | Produce a 'Names' object which contains names for the current branch.
currentNames :: Cli Names
currentNames = do
  Branch.toNames <$> getCurrentBranch0

projectRootNames :: Cli Names
projectRootNames = do
  Cli.Env {codebase} <- ask
  causalHash <- Branch.headHash <$> Cli.getProjectRootBranch
  NamesCache.BranchNames {branchNames} <- liftIO $ NamesCache.expectNamesForBranch codebase causalHash
  pure branchNames

projectRootNamesWithoutTransitiveLibs :: Cli Names
projectRootNamesWithoutTransitiveLibs = do
  Cli.Env {codebase} <- ask
  causalHash <- Branch.headHash <$> Cli.getProjectRootBranch
  NamesCache.BranchNames {branchNamesWithoutTransitiveLibs} <- liftIO $ NamesCache.expectNamesForBranch codebase causalHash
  pure branchNamesWithoutTransitiveLibs
