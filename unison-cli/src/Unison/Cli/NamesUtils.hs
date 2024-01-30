-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( currentNames,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Cli.MonadUtils (getCurrentBranch0)
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Names (Names)

-- | Produce a 'Names' object which contains names for the current branch.
currentNames :: Cli Names
currentNames = do
  Branch.toNames <$> getCurrentBranch0
