module Unison.Merge.Diff
  ( diffSynhashedDefns,
    humanizeDiffs,
  )
where

import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.These (These (..))
import Data.Zip qualified as Zip
import U.Codebase.Reference (TypeReference)
import Unison.Merge.DiffOp (DiffOp (..), DiffOp2 (..))
import Unison.Merge.DiffOp qualified as DiffOp
import Unison.Merge.HumanDiffOp (HumanDiffOp (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (GUpdated (..), Updated)
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude hiding (catMaybes)
import Unison.Referent (Referent)
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, unzipDefns, zipDefnsWith)
import Unison.Util.Defns qualified as Defns
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation

-- | @diffSynhashedDefns defns@, given the output of @synhashDefns@, computes the two two-way diffs (each consisting of
-- the "core" diffs, i.e. adds/delete/updates, alongside the propagated updates, i.e. updates that have the same synhash
-- but different Unison hashes).
diffSynhashedDefns ::
  TwoWay (Updated (DefnsF2 (Map Name) Synhashed Referent TypeReference)) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    TwoWay (DefnsF (Map Name) (Updated Referent) (Updated TypeReference))
  )
diffSynhashedDefns =
  Zip.unzip . fmap diffSynhashedDefns0

diffSynhashedDefns0 ::
  (Eq term, Eq typ) =>
  Updated (DefnsF2 (Map Name) Synhashed term typ) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    DefnsF3 (Map Name) DiffOp Synhashed term typ,
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    DefnsF (Map Name) (Updated term) (Updated typ)
  )
diffSynhashedDefns0 defns =
  unzipDefns (zipDefnsWith f f defns.old defns.new)
  where
    f ::
      (Eq ref) =>
      Map Name (Synhashed ref) ->
      Map Name (Synhashed ref) ->
      (Map Name (DiffOp (Synhashed ref)), Map Name (Updated ref))
    f old new =
      partitionPropagated (diffSynhashedDefns1 old new)

-- Compute the diff by comparing old-and-new values, resulting in either an add, delete, update (propagated or not),
-- or dropping the thing entirely (because old and new have the same hash).
diffSynhashedDefns1 ::
  forall ref.
  (Eq ref) =>
  Map Name (Synhashed ref) ->
  Map Name (Synhashed ref) ->
  Map Name (DiffOp2 (Synhashed ref))
diffSynhashedDefns1 =
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

-- | Post-process a diff to identify relationships humans might care about, such as whether a given addition could be
-- interpreted as an alias of an existing definition, or whether an add and deletion could be a rename.
humanizeDiffs ::
  ThreeWay Names ->
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) ->
  TwoWay (DefnsF (Map Name) (Updated Referent) (Updated TypeReference)) ->
  TwoWay (DefnsF2 (Map Name) HumanDiffOp Referent TypeReference)
humanizeDiffs names3 =
  let names3' = names3 <&> \names -> Defns names.terms names.types
   in zipWith3
        (Defns.zipDefnsWith4 computeHumanDiffOp computeHumanDiffOp names3'.lca)
        (ThreeWay.forgetLca names3')
  where
    zipWith3 :: (Zip.Zip f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    zipWith3 f a b =
      Zip.zipWith (uncurry f) (Zip.zip a b)

    computeHumanDiffOp ::
      forall ref.
      (Show ref, Ord ref) =>
      Relation Name ref ->
      Relation Name ref ->
      Map Name (DiffOp (Synhashed ref)) ->
      Map Name (Updated ref) ->
      Map Name (HumanDiffOp ref)
    computeHumanDiffOp oldNamespace newNamespace =
      alignWith \case
        This diff -> humanizeDiffOp (DiffOp.map Synhashed.value diff)
        That updated -> HumanDiffOp'PropagatedUpdate updated
        These diff updated ->
          error $
            reportBug
              "E488729"
              ( "The impossible happened, an update in merge was detected as both a propagated AND core update "
                  ++ show diff
                  ++ " and "
                  ++ show updated
              )
      where
        humanizeDiffOp :: DiffOp ref -> HumanDiffOp ref
        humanizeDiffOp = \case
          DiffOp'Add ref ->
            -- This name is newly added. We need to check if it's a new definition, an alias, or a rename.
            case Set.toList (Relation.lookupRan ref oldNamespace) of
              -- No old names for this ref, so it's a new addition not an alias
              [] -> HumanDiffOp'Add ref
              -- There are old names for this ref, but not old refs for this name, so it's
              -- either a new alias or a rename.
              --
              -- If at least one old name for this ref no longer exists, we treat it like a
              -- rename.
              n : ns -> do
                let existingNames = NESet.fromList (n List.NonEmpty.:| ns)
                case NESet.nonEmptySet (Relation.lookupRan ref newNamespace) of
                  Nothing -> error (reportBug "E458329" ("Expected to find at least one name for ref in new namespace, since we found the ref by the name."))
                  Just allNewNames ->
                    case NESet.nonEmptySet (NESet.difference existingNames allNewNames) of
                      -- If all the old names still exist in the new namespace, it's a new alias.
                      Nothing -> HumanDiffOp'AliasOf ref existingNames
                      -- Otherwise, treat it as a rename.
                      Just namesWhichDisappeared ->
                        HumanDiffOp'RenamedFrom ref namesWhichDisappeared
          DiffOp'Delete ref ->
            case List.NonEmpty.nonEmpty $ Set.toList (Relation.lookupRan ref newNamespace) of
              -- No names for this ref, it was removed.
              Nothing -> HumanDiffOp'Delete ref
              Just newNames -> HumanDiffOp'RenamedTo ref (NESet.fromList newNames)
          DiffOp'Update Updated {old, new} -> HumanDiffOp'Update Updated {old, new}
