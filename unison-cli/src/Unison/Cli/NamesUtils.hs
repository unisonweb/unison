-- | Utilities that have to do with names objects.
module Unison.Cli.NamesUtils
  ( currentNames,
    currentNameSearch,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils (getCurrentBranch0)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Names (Names)
import Unison.Server.NameSearch
import Unison.Server.NameSearch.FromNames qualified as NameSearch

-- | Produce a 'Names' object which contains names for the current branch.
currentNames :: Cli Names
currentNames = do
  Branch.toNames <$> getCurrentBranch0

-- | Produce a name searcher for things in the scope of the current branch.
currentNameSearch :: Applicative m => Cli (NameSearch m)
currentNameSearch = do
  hqLength <- Cli.runTransaction Codebase.hashLength
  NameSearch.makeNameSearch hqLength <$> currentNames
