-- | An API for merging together two collections of library dependencies.
module Unison.Merge.Libdeps
  ( mergeLibdeps,
  )
where

import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.These (These (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay qualified as EitherWay
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.TwoDiffOps (TwoDiffOps (..))
import Unison.Merge.TwoDiffOps qualified as TwoDiffOps
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (Updated (..))
import Unison.Prelude hiding (catMaybes)
import Unison.Util.Map qualified as Map
import Witherable (catMaybes)

-- | Perform a three-way merge on two collections of library dependencies.
mergeLibdeps ::
  forall k v.
  (Ord k, Eq v) =>
  -- | Freshen a name, e.g. "base" -> ("base__4", "base__5").
  (Set k -> k -> (k, k)) ->
  -- | Library dependencies.
  ThreeWay (Map k v) ->
  -- | Merged library dependencies.
  Map k v
mergeLibdeps freshen libdeps =
  mergeDiffs (diff libdeps.lca libdeps.alice) (diff libdeps.lca libdeps.bob)
    & applyDiff (freshen usedNames) libdeps.lca
  where
    usedNames :: Set k
    usedNames =
      Set.unions
        [ Map.keysSet libdeps.lca,
          Map.keysSet libdeps.alice,
          Map.keysSet libdeps.bob
        ]

-- `diff old new` computes a diff between old thing `old` and new thing `new`.
--
-- Values present in `old` but not `new` are tagged as "deleted"; similar for "added" and "updated".
diff :: (Ord k, Eq v) => Map k v -> Map k v -> Map k (DiffOp v)
diff =
  Map.merge
    (Map.mapMissing \_ -> DiffOp'Delete)
    (Map.mapMissing \_ -> DiffOp'Add)
    ( Map.zipWithMaybeMatched \_ old new ->
        if old == new
          then Nothing
          else Just (DiffOp'Update Updated {old, new})
    )

-- Merge two library dependency diffs together:
--
--   * Keep all adds/updates (allowing conflicts as necessary, which will be resolved later)
--   * Ignore deletes that only one party makes (because the other party may expect the dep to still be there)
mergeDiffs ::
  forall k v.
  (Ord k, Eq v) =>
  -- The LCA->Alice library dependencies diff.
  Map k (DiffOp v) ->
  -- The LCA->Bob library dependencies diff.
  Map k (DiffOp v) ->
  -- The merged library dependencies diff.
  Map k (LibdepDiffOp v)
mergeDiffs alice bob =
  catMaybes (alignWith combineDiffOps alice bob)

combineDiffOps :: (Eq a) => These (DiffOp a) (DiffOp a) -> Maybe (LibdepDiffOp a)
combineDiffOps =
  TwoDiffOps.make >>> combineDiffOps1

combineDiffOps1 :: (Eq a) => TwoDiffOps a -> Maybe (LibdepDiffOp a)
combineDiffOps1 = \case
  TwoDiffOps'Add new -> Just (AddLibdep (EitherWay.value new))
  -- If Alice deletes a dep and Bob doesn't touch it, ignore the delete, since Bob may still be using it.
  TwoDiffOps'Delete _old -> Nothing
  -- If Alice updates a dep and Bob doesn't touch it, keep the old one around too, since Bob may still be using it.
  TwoDiffOps'Update x -> Just (AddBothLibdeps (EitherWay.value x).old (EitherWay.value x).new)
  TwoDiffOps'AddAdd TwoWay {alice, bob}
    | alice == bob -> Just (AddLibdep alice)
    | otherwise -> Just (AddBothLibdeps alice bob)
  -- If Alice and Bob both delete something, delete it.
  TwoDiffOps'DeleteDelete _ -> Just DeleteLibdep
  -- If Alice updates a dependency and Bob deletes the old one, ignore the delete and keep Alice's, and vice versa.
  TwoDiffOps'DeleteUpdate bob -> Just (AddLibdep bob.new)
  TwoDiffOps'UpdateDelete alice -> Just (AddLibdep alice.new)
  -- combineDiffOps (Deleted _) (Updated _ bob) = AddLibdep bob
  -- combineDiffOps (Updated _ alice) (Deleted _) = AddLibdep alice
  TwoDiffOps'UpdateUpdate _old TwoWay {alice, bob}
    | alice == bob -> Just (AddLibdep alice)
    | otherwise -> Just (AddBothLibdeps alice bob)

-- Apply a library dependencies diff to the LCA.
applyDiff ::
  forall k v.
  (Ord k) =>
  -- Freshen a name, e.g. "base" -> ("base__4", "base__5")
  (k -> (k, k)) ->
  -- The LCA library dependencies.
  Map k v ->
  -- LCA->Alice+Bob library dependencies diff.
  Map k (LibdepDiffOp v) ->
  -- The merged library dependencies.
  Map k v
applyDiff freshen =
  Map.mergeMap Map.singleton f (\name _ -> f name)
  where
    f :: k -> LibdepDiffOp v -> Map k v
    f k = \case
      AddLibdep v -> Map.singleton k v
      AddBothLibdeps v1 v2 ->
        let (k1, k2) = freshen k
         in Map.fromList [(k1, v1), (k2, v2)]
      DeleteLibdep -> Map.empty

data LibdepDiffOp a
  = AddLibdep !a
  | AddBothLibdeps !a !a
  | DeleteLibdep
