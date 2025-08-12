module Unison.UnconflictedLocalDefnsView
  ( UnconflictedLocalDefnsView (..),
    Unison.UnconflictedLocalDefnsView.empty,
    fromDefns,
  )
where

import Data.Map.Strict qualified as Map
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Nametree (Nametree (..), unflattenNametrees)

-- | A view of a namespace's dsefinition (everything outside of `lib`) that is unconflicted: each name refers to one
-- thing. The data contained within is all just different expressions of the same contents, for various use cases. The
-- intention is to use laziness to avoid recomputing data structures whenever possible.
data UnconflictedLocalDefnsView = UnconflictedLocalDefnsView
  { defns :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name),
    nametree :: Nametree (DefnsF (Map NameSegment) Referent TypeReference),
    names :: Names
  }
  deriving stock (Generic, Show)

empty :: UnconflictedLocalDefnsView
empty =
  UnconflictedLocalDefnsView
    { defns = Defns BiMultimap.empty BiMultimap.empty,
      nametree = Nametree (Defns Map.empty Map.empty) Map.empty,
      names = Names.empty
    }

fromDefns :: DefnsF (Map Name) Referent TypeReference -> UnconflictedLocalDefnsView
fromDefns defns =
  UnconflictedLocalDefnsView
    { defns = bimap BiMultimap.fromRange BiMultimap.fromRange defns,
      nametree = unflattenNametrees defns,
      names = Names.fromUnconflicted defns
    }
