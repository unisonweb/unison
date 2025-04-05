module Unison.Merge.Mergeblob0
  ( Mergeblob0 (..),
    makeMergeblob0,
  )
where

import Unison.Merge.ThreeWay (ThreeWay)
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.Defns (Defns, DefnsF)
import Unison.Util.Nametree (Nametree, flattenNametrees)

data Mergeblob0 libdep = Mergeblob0
  { defns :: ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    libdeps :: ThreeWay (Map NameSegment libdep),
    nametrees :: ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
  }

makeMergeblob0 ::
  ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
  ThreeWay (Map NameSegment libdep) ->
  Mergeblob0 libdep
makeMergeblob0 nametrees libdeps =
  Mergeblob0
    { defns = flattenNametrees <$> nametrees,
      libdeps,
      nametrees
    }
