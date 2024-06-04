-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( currentNames,
    projectRootNames,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Names (Names)

-- | Produce a 'Names' object which contains names for the current branch.
currentNames :: Cli Names
currentNames = do
  Branch.toNames <$> Cli.getCurrentBranch0

projectRootNames :: Cli Names
projectRootNames = do
  Branch.toNames <$> Cli.getCurrentProjectRoot0
