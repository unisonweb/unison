module Unison.Merge.Mergeblob0
  ( Mergeblob0 (..),
    makeMergeblob0,
  )
where

import Data.Map.Merge.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Unison.Merge.ThreeWay (ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay)
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns, DefnsF, zipDefnsWith)
import Unison.Util.Nametree (Nametree, flattenNametrees)

data Mergeblob0 libdep = Mergeblob0
  { defns :: ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    libdeps :: ThreeWay (Map NameSegment libdep),
    nametrees :: ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)),
    renames :: TwoWay (Defns (Map Referent Rename) (Map TypeReference Rename))
  }

makeMergeblob0 ::
  ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
  ThreeWay (Map NameSegment libdep) ->
  Mergeblob0 libdep
makeMergeblob0 nametrees libdeps =
  let defns = flattenNametrees <$> nametrees
   in Mergeblob0
        { defns,
          libdeps,
          nametrees,
          renames = makeRenames defns
        }

-- Invariant: it is not the case the both sets are empty
data Rename = Rename
  { deletes :: Set Name,
    adds :: Set Name
  }

makeRenames ::
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay (Defns (Map Referent Rename) (Map TypeReference Rename))
makeRenames defns =
  zipDefnsWith f f defns.lca <$> ThreeWay.forgetLca defns
  where
    f :: (Ord ref) => BiMultimap ref Name -> BiMultimap ref Name -> Map ref Rename
    f old new =
      Map.merge
        Map.dropMissing
        Map.dropMissing
        (Map.zipWithMaybeMatched g)
        (BiMultimap.domain old)
        (BiMultimap.domain new)

    g :: ref -> NESet Name -> NESet Name -> Maybe Rename
    g _ old new =
      let deletes = Set.NonEmpty.difference old new
          adds = Set.NonEmpty.difference new old
       in if Set.null deletes && Set.null adds
            then Nothing
            else Just Rename {deletes, adds}
