module Unison.Merge.Rename
  ( Rename (..),
    makeRenames,
    makeRenames',
    SimpleRenames (..),
    makeSimpleRenames,
  )
where

import Control.Lens
import Data.List qualified as List
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (GUpdated (..), Updated)
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.ReferentPrime qualified as Referent'
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, zipDefnsWith)

-- | A "rename" is a venn partition of two non-empty sets of names: both set differences and the set intersection.
--
-- Invariant: the sets are all disjoint
-- Invariant: it is not the case that adds and deletes are both empty
data Rename = Rename
  { adds :: Set Name,
    deletes :: Set Name,
    unchanged :: Set Name
  }

makeRenames ::
  ThreeWay (Defns (BiMultimap (Synhashed Referent) Name) (BiMultimap (Synhashed TypeReference) Name)) ->
  TwoWay (DefnsF [] Rename Rename)
makeRenames defns =
  zipDefnsWith (f termNamingsToRename) (f \_ -> namingsToRename) defns.lca <$> ThreeWay.forgetLca defns
  where
    f ::
      (Ord ref) =>
      (ref -> NESet Name -> NESet Name -> Maybe Rename) ->
      BiMultimap ref Name ->
      BiMultimap ref Name ->
      [Rename]
    f g old new =
      Map.elems $
        Map.merge
          Map.dropMissing
          Map.dropMissing
          (Map.zipWithMaybeMatched g)
          (BiMultimap.domain old)
          (BiMultimap.domain new)

makeRenames' ::
  Updated (Defns (BiMultimap (Synhashed Referent) Name) (BiMultimap (Synhashed TypeReference) Name)) ->
  DefnsF [] Rename Rename
makeRenames' defns =
  zipDefnsWith (f termNamingsToRename) (f \_ -> namingsToRename) defns.old defns.new
  where
    f ::
      (Ord ref) =>
      (ref -> NESet Name -> NESet Name -> Maybe Rename) ->
      BiMultimap ref Name ->
      BiMultimap ref Name ->
      [Rename]
    f g old new =
      Map.elems $
        Map.merge
          Map.dropMissing
          Map.dropMissing
          (Map.zipWithMaybeMatched g)
          (BiMultimap.domain old)
          (BiMultimap.domain new)

termNamingsToRename :: Synhashed Referent -> NESet Name -> NESet Name -> Maybe Rename
termNamingsToRename ref old new
  -- Throw away constructors; we don't really capture their renamings in the same way as types an terms, as the
  -- synhash of a constructor is just the synhash of its type.
  | Referent'.isConstructor ref.value = Nothing
  | otherwise = namingsToRename old new

namingsToRename :: NESet Name -> NESet Name -> Maybe Rename
namingsToRename old new =
  if Set.null adds && Set.null deletes
    then Nothing
    else Just Rename {adds, deletes, unchanged}
  where
    adds = Set.NonEmpty.difference new old
    deletes = Set.NonEmpty.difference old new
    unchanged = Set.NonEmpty.intersection old new

-- | A "simple" rename is one that moves one name to another, where neither the before- nor after-name have any aliases.
data SimpleRenames = SimpleRenames
  { forwards :: !(Map Name Name),
    backwards :: !(Map Name Name)
  }

makeSimpleRenames :: DefnsF [] Rename Rename -> Defns SimpleRenames SimpleRenames
makeSimpleRenames =
  bimap makeSimpleRenames1 makeSimpleRenames1

makeSimpleRenames1 :: [Rename] -> SimpleRenames
makeSimpleRenames1 =
  List.foldl' f (SimpleRenames Map.empty Map.empty)
  where
    f :: SimpleRenames -> Rename -> SimpleRenames
    f acc rename =
      case (Set.size rename.adds, Set.size rename.deletes, Set.size rename.unchanged) of
        (1, 1, 0) ->
          let old = Set.findMin rename.deletes
              new = Set.findMin rename.adds
           in SimpleRenames (Map.insert old new acc.forwards) (Map.insert new old acc.backwards)
        _ -> acc
