-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( currentNames,
    currentProjectRootNames,
    projectBranchNames,
  )
where

import U.Codebase.Sqlite.ProjectBranch (ProjectBranch)
import Unison.Cli.Monad (Cli)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Names (Names)

-- | Produce a 'Names' object which contains names for the current branch.
currentNames :: Cli Names
currentNames = do
  Branch.toNames <$> Cli.getCurrentBranch0

currentProjectRootNames :: Cli Names
currentProjectRootNames = do
  Branch.toNames <$> Cli.getCurrentProjectRoot0

projectBranchNames :: ProjectBranch -> Cli Names
projectBranchNames pb = do
  Branch.toNames . Branch.head <$> Cli.getProjectBranchRoot pb
