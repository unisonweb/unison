module Unison.Merge.Diff
  ( diffSynhashedDefns,
    diffSynhashedDefns1,
  )
where

import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Zip qualified as Zip
import Unison.Merge.DiffOp (DiffOp (..), DiffOp2 (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (GUpdated (..), Updated)
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.Util.Defns (DefnsF, DefnsF2, DefnsF3, unzipDefns, zipDefnsWith)

-- | @diffSynhashedDefns defns@, given the output of @synhashDefns@, computes the two two-way diffs (each consisting of
-- the "core" diffs, i.e. adds/delete/updates, alongside the propagated updates, i.e. updates that have the same synhash
-- but different Unison hashes).
diffSynhashedDefns ::
  (Eq term, Eq typ) =>
  TwoWay (Updated (DefnsF2 (Map Name) Synhashed term typ)) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    TwoWay (DefnsF3 (Map Name) DiffOp Synhashed term typ),
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    TwoWay (DefnsF (Map Name) (Updated term) (Updated typ))
  )
diffSynhashedDefns =
  Zip.unzip . fmap diffSynhashedDefns1

-- | Like 'diffSynhashedDefns', but for just one LCA->Head side.
diffSynhashedDefns1 ::
  (Eq term, Eq typ) =>
  Updated (DefnsF2 (Map Name) Synhashed term typ) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    DefnsF3 (Map Name) DiffOp Synhashed term typ,
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    DefnsF (Map Name) (Updated term) (Updated typ)
  )
diffSynhashedDefns1 defns =
  unzipDefns (zipDefnsWith f f defns.old defns.new)
  where
    f ::
      (Eq ref) =>
      Map Name (Synhashed ref) ->
      Map Name (Synhashed ref) ->
      (Map Name (DiffOp (Synhashed ref)), Map Name (Updated ref))
    f old new =
      partitionPropagated (diffSynhashedDefns2 old new)

-- Compute the diff by comparing old-and-new values, resulting in either an add, delete, update (propagated or not),
-- or dropping the thing entirely (because old and new have the same hash).
diffSynhashedDefns2 ::
  forall ref.
  (Eq ref) =>
  Map Name (Synhashed ref) ->
  Map Name (Synhashed ref) ->
  Map Name (DiffOp2 (Synhashed ref))
diffSynhashedDefns2 =
  Map.merge
    (Map.mapMissing \_ -> DiffOp2'Delete)
    (Map.mapMissing \_ -> DiffOp2'Add)
    (Map.zipWithMaybeMatched \_ -> f)
  where
    f :: Synhashed ref -> Synhashed ref -> Maybe (DiffOp2 (Synhashed ref))
    f old new = do
      let equalSynhashes = old == new
      -- Drop things that haven't changed
      when equalSynhashes do
        guard (Synhashed.value old /= Synhashed.value new)
      Just (DiffOp2'Update Updated {old, new} equalSynhashes)

-- Partition add/delete/update/propagated-update into add/delete/update + propagated-update
partitionPropagated :: Map Name (DiffOp2 (Synhashed ref)) -> (Map Name (DiffOp (Synhashed ref)), Map Name (Updated ref))
partitionPropagated =
  Map.mapEither \case
    DiffOp2'Add ref -> Left (DiffOp'Add ref)
    DiffOp2'Delete ref -> Left (DiffOp'Delete ref)
    DiffOp2'Update refs propagated
      | propagated -> Right (Updated.map Synhashed.value refs)
      | otherwise -> Left (DiffOp'Update refs)
