-- | An API for merging together two collections of library dependencies.
module Unison.Merge.Libdeps
  ( mergeLibdeps,
  )
where

import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Prelude
import Unison.Util.Map qualified as Map

-- | Merge two collections of library dependencies.
mergeLibdeps ::
  forall lib name.
  Ord name =>
  -- | Are these library dependencies equal?
  (lib -> lib -> Bool) ->
  -- | Freshen a name, e.g. "base" -> ("base__4", "base__5").
  (Set name -> name -> (name, name)) ->
  -- | LCA library dependencies, if there is an LCA.
  Maybe (Map name lib) ->
  -- | Alice's library dependencies.
  Map name lib ->
  -- | Bob's library dependencies.
  Map name lib ->
  -- | Merged library dependencies.
  Map name lib
mergeLibdeps eq freshen maybeLca alice bob =
  case maybeLca of
    Nothing -> twoWayLibdepsMerge eq (freshen usedNames) alice bob
    Just lca -> threeWayLibdepsMerge eq (freshen usedNames) lca alice bob
  where
    usedNames :: Set name
    usedNames =
      maybe Set.empty Map.keysSet maybeLca <> Map.keysSet alice <> Map.keysSet bob

-- Perform a two-way merge on two collections of library dependencies.
twoWayLibdepsMerge ::
  forall lib name.
  Ord name =>
  -- | Are these library dependencies equal?
  (lib -> lib -> Bool) ->
  -- | Freshen a name, e.g. "base" -> ("base__4", "base__5").
  (name -> (name, name)) ->
  -- | Alice's library dependencies.
  Map name lib ->
  -- | Bob's library dependencies.
  Map name lib ->
  -- | Merged library dependencies.
  Map name lib
twoWayLibdepsMerge eq freshen alice bob =
  Map.mergeMap Map.singleton Map.singleton both alice bob
  where
    both :: name -> lib -> lib -> Map name lib
    both name lib1 lib2
      | eq lib1 lib2 = Map.singleton name lib1
      | otherwise =
          let (name1, name2) = freshen name
           in Map.insert name2 lib2 (Map.singleton name1 lib1)

-- Perform a three-way merge on two collections of library dependencies.
threeWayLibdepsMerge ::
  forall lib name.
  Ord name =>
  -- | Are these library dependencies equal?
  (lib -> lib -> Bool) ->
  -- | Freshen a name, e.g. "base" -> ("base__4", "base__5").
  (name -> (name, name)) ->
  -- | LCA library dependencies.
  Map name lib ->
  -- | Alice's library dependencies.
  Map name lib ->
  -- | Bob's library dependencies.
  Map name lib ->
  -- | Merged library dependencies.
  Map name lib
threeWayLibdepsMerge eq freshen lca alice bob =
  applyLibdepsDiff freshen lca (mergeLibdepsDiffs eq (diff alice) (diff bob))
  where
    diff :: Map name lib -> Map name (DiffOp lib)
    diff =
      Map.merge
        (Map.mapMissing \_ -> Deleted)
        (Map.mapMissing \_ -> Added)
        ( Map.zipWithMaybeMatched \_ old new ->
            if eq old new
              then Nothing
              else Just (Updated old new)
        )
        lca

data LibdepDiffOp a
  = AddLibdep !a
  | AddBothLibdeps !a !a
  | DeleteLibdep

-- Merge two library dependency diffs together:
--   * Keep all adds/updates (allowing conflicts as necessary, which will be resolved later)
--   * Ignore deletes that only one party makes (because the other party may expect the dep to still be there)
mergeLibdepsDiffs ::
  forall lib name.
  (Ord name) =>
  -- | Are these library dependencies equal?
  (lib -> lib -> Bool) ->
  -- | The LCA->Alice library dependencies diff.
  Map name (DiffOp lib) ->
  -- | The LCA->Bob library dependencies diff.
  Map name (DiffOp lib) ->
  -- | The merged library dependencies diff.
  Map name (LibdepDiffOp lib)
mergeLibdepsDiffs eq =
  Map.merge onAliceOrBob onAliceOrBob onAliceAndBob
  where
    onAliceOrBob :: Map.WhenMissing Identity name (DiffOp lib) (LibdepDiffOp lib)
    onAliceOrBob =
      Map.traverseMaybeMissing \_name -> Identity . f
      where
        f :: DiffOp lib -> Maybe (LibdepDiffOp lib)
        f = \case
          Added new -> Just (AddLibdep new)
          -- If Alice deletes a dep and Bob doesn't touch it, ignore the delete, since Bob may still be using it.
          Deleted _ -> Nothing
          -- If Alice updates a dep and Bob doesn't touch it, keep the old and new deps, since Bob may still be using
          -- the old one.
          Updated old new -> Just (AddBothLibdeps old new)

    onAliceAndBob :: Map.WhenMatched Identity name (DiffOp lib) (DiffOp lib) (LibdepDiffOp lib)
    onAliceAndBob =
      Map.zipWithAMatched \_name x y -> Identity (f (x, y))
      where
        f :: (DiffOp lib, DiffOp lib) -> LibdepDiffOp lib
        f = \case
          (Added alice, Added bob)
            | eq alice bob -> AddLibdep alice
            | otherwise -> AddBothLibdeps alice bob
          (Updated _ alice, Updated _ bob)
            | eq alice bob -> AddLibdep alice
            | otherwise -> AddBothLibdeps alice bob
          -- If Alice updates a dependency and Bob deletes the old one, ignore the delete and keep Alice's.
          (Deleted _, Updated _ bob) -> AddLibdep bob
          (Updated _ alice, Deleted _) -> AddLibdep alice
          -- If Alice and Bob both delete something, delete it.
          (Deleted {}, Deleted {}) -> DeleteLibdep
          -- These are all nonsense: if one person's change was classified as an add, then it didn't exist in the
          -- LCA, so the other person's change to the same name couldn't be classified as an update/delete
          (Added {}, Deleted {}) -> wundefined
          (Added {}, Updated {}) -> wundefined
          (Deleted {}, Added {}) -> wundefined
          (Updated {}, Added {}) -> wundefined

-- Apply a library dependencies diff to the LCA.
applyLibdepsDiff ::
  forall lib name.
  (Ord name) =>
  -- | Freshen a name, e.g. "base" -> ("base__4", "base__5")
  (name -> (name, name)) ->
  -- | The LCA library dependencies.
  Map name lib ->
  -- | LCA->Alice+Bob library dependencies diff.
  Map name (LibdepDiffOp lib) ->
  -- | The merged library dependencies.
  Map name lib
applyLibdepsDiff freshen lca diff =
  Map.mergeMap Map.singleton f (\name _ -> f name) lca diff
  where
    f :: name -> LibdepDiffOp lib -> Map name lib
    f name = \case
      AddLibdep val -> Map.singleton name val
      AddBothLibdeps val1 val2 ->
        let (name1, name2) = freshen name
         in Map.insert name2 val2 (Map.singleton name1 val1)
      DeleteLibdep -> Map.empty
