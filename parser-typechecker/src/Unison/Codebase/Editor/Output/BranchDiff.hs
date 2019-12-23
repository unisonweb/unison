{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.Output.BranchDiff where

import Unison.Prelude

import Unison.Name (Name)
import qualified Unison.Codebase.Patch as P
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Codebase.BranchDiff as BranchDiff
import Unison.Codebase.BranchDiff (BranchDiff(BranchDiff), DiffSlice)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation3 as R3
import qualified Unison.Codebase.Metadata as Metadata
import qualified Data.Set as Set
import qualified Data.Map as Map
import Unison.Util.List (uniqueBy)

import Unison.Reference (Reference)
import Unison.Type (Type)
import Unison.HashQualified' (HashQualified)
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Referent as Referent
import Unison.Referent (Referent)
import Data.Set (Set)
import qualified Unison.Names2 as Names2
import Unison.Names3 (Names0)
import Unison.DataDeclaration (DeclOrBuiltin)
import Unison.Runtime.IOSource (isPropagatedValue)

data MetadataDiff tm =
  MetadataDiff { addedMetadata :: [tm]
               , removedMetadata :: [tm] }
  deriving (Ord,Eq,Functor,Foldable,Traversable,Show)

instance Semigroup (MetadataDiff tm) where
  a <> b = MetadataDiff (addedMetadata a <> addedMetadata b)
                        (removedMetadata a <> removedMetadata b)

instance Monoid (MetadataDiff tm) where
  mempty = MetadataDiff mempty mempty
  mappend = (<>)

data BranchDiffOutput v a = BranchDiffOutput {
  updatedTypes      :: [UpdateTypeDisplay v a],
  updatedTerms      :: [UpdateTermDisplay v a],
  propagatedUpdates :: Int,
  updatedPatches    :: [PatchDisplay],
  addedTypes        :: [TypeDisplay v a],
  addedTerms        :: [TermDisplay v a],
  addedPatches      :: [PatchDisplay],
  removedTypes      :: [TypeDisplay v a],
  removedTerms      :: [TermDisplay v a],
  removedPatches    :: [PatchDisplay],
  movedTypes        :: [RenameTypeDisplay v a],
  movedTerms        :: [RenameTermDisplay v a],
  copiedTypes       :: [RenameTypeDisplay v a],
  copiedTerms       :: [RenameTermDisplay v a]
 } deriving Show

-- Need to be able to turn a (Name,Reference) into a HashQualified relative to... what.
-- the new namespace?

type TermDisplay v a = (HashQualified, Maybe (Type v a), MetadataDiff (MetadataDisplay v a))
type TypeDisplay v a = (HashQualified, Maybe (DeclOrBuiltin v a), MetadataDiff (MetadataDisplay v a))

type SimpleTermDisplay v a = (HashQualified, Maybe (Type v a))
type SimpleTypeDisplay v a = (HashQualified, Maybe (DeclOrBuiltin v a))

type UpdateTermDisplay v a = (Maybe [SimpleTermDisplay v a], [TermDisplay v a])
type UpdateTypeDisplay v a = (Maybe [SimpleTypeDisplay v a], [TypeDisplay v a])

type MetadataDisplay v a = SimpleTermDisplay v a
type RenameTermDisplay v a = (Referent, Maybe (Type v a), Set HashQualified, Set HashQualified)
type RenameTypeDisplay v a = (Reference, Maybe (DeclOrBuiltin v a), Set HashQualified, Set HashQualified)
type PatchDisplay = (Name, P.PatchDiff)

toOutput :: forall m v a
          . Monad m
         => (Referent -> m (Maybe (Type v a)))
         -> (Reference -> m (Maybe (DeclOrBuiltin v a)))
         -> Int
         -> Names0
         -> Names0
         -> PPE.PrettyPrintEnv
         -> BranchDiff.BranchDiff
         -> m (BranchDiffOutput v a)
toOutput typeOf declOrBuiltin hqLen names1 names2 ppe
            (BranchDiff termsDiff typesDiff patchesDiff) = do
  let
    -- | This calculates the new reference's metadata as:
    --   adds: now-attached metadata that was missing from
    --         any of the old references associated with the name
    --   removes: not-attached metadata that had been attached to any of
    --         the old references associated with the name
    getNewMetadataDiff :: Ord r => DiffSlice r -> Name -> Set r -> r -> MetadataDiff Metadata.Value
    getNewMetadataDiff s n rs_old r_new = let
      old_metadatas :: [Set Metadata.Value] =
        toList . R.toMultimap . R.restrictDom rs_old . R3.lookupD2 n $
          BranchDiff.tremovedMetadata s
      old_intersection :: Set Metadata.Value =
        foldl1' Set.intersection old_metadatas
      old_union :: Set Metadata.Value =
        foldl1' Set.union old_metadatas
      new_metadata :: Set Metadata.Value =
        R.lookupDom n . R3.lookupD1 r_new $ BranchDiff.taddedMetadata s
      in MetadataDiff
          { addedMetadata = toList $ new_metadata `Set.difference` old_intersection
          , removedMetadata = toList $ old_union `Set.difference` new_metadata
          }
    getMetadataUpdates s =
      [ (n, (Set.singleton r, Set.singleton r))
      | (r,n,v) <- R3.toList $ BranchDiff.taddedMetadata s <>
                               BranchDiff.tremovedMetadata s
      , v /= isPropagatedValue ]

  updatedTypes :: [UpdateTypeDisplay v a] <- let
    -- things where what the name pointed to changed
    nsUpdates :: [(Name, (Set Reference, Set Reference))] =
      Map.toList $ BranchDiff.namespaceUpdates typesDiff
    -- things where the metadata changed (`uniqueBy` below removes these
    -- if they were already included in `nsUpdates)
    metadataUpdates = getMetadataUpdates typesDiff
    loadOld :: Name -> Reference -> m (SimpleTypeDisplay v a)
    loadOld n r_old =
      (,) <$> pure (Names2.hqTypeName hqLen names1 n r_old)
          <*> declOrBuiltin r_old
    loadNew :: Name -> Set Reference -> Reference -> m (TypeDisplay v a)
    loadNew n rs_old r_new =
      (,,) <$> pure (Names2.hqTypeName hqLen names2 n r_new)
           <*> declOrBuiltin r_new
           <*> fillMetadata ppe (getNewMetadataDiff typesDiff n rs_old r_new)
    loadEntry :: (Name, (Set Reference, Set Reference)) -> m (UpdateTypeDisplay v a)
    loadEntry (n, (rs_old, rs_new)) =
      (,) <$> (Just <$> for (toList rs_old) (loadOld n))
          <*> for (toList rs_new) (loadNew n rs_old)
    in for (sortOn fst . uniqueBy fst $ nsUpdates <> metadataUpdates) loadEntry

  updatedTerms :: [UpdateTermDisplay v a] <- let
    -- things where what the name pointed to changed
    nsUpdates =
      Map.toList $ BranchDiff.namespaceUpdates termsDiff
    -- things where the metadata changed (`uniqueBy` below removes these
    -- if they were already included in `nsUpdates)
    metadataUpdates = getMetadataUpdates termsDiff
    loadOld n r_old =
      (,) <$> pure (Names2.hqTermName hqLen names1 n r_old)
          <*> typeOf r_old
    loadNew n rs_old r_new =
      (,,) <$> pure (Names2.hqTermName hqLen names2 n r_new)
           <*> typeOf r_new
           <*> fillMetadata ppe (getNewMetadataDiff termsDiff n rs_old r_new)
    loadEntry (n, (rs_old, rs_new)) =
      (,) <$> (Just <$> for (toList rs_old) (loadOld n))
          <*> for (toList rs_new) (loadNew n rs_old)
    in for (sortOn fst . uniqueBy fst $ nsUpdates <> metadataUpdates) loadEntry

  let propagatedUpdates :: Int =
        (Set.size . R3.d2s . BranchDiff.propagatedNamespaceUpdates) typesDiff +
        (Set.size . R3.d2s . BranchDiff.propagatedNamespaceUpdates) termsDiff

  let updatedPatches :: [PatchDisplay] =
        [(name, diff) | (name, BranchDiff.Modify diff) <- Map.toList patchesDiff]

  addedTypes :: [TypeDisplay v a] <- let
    typeAdds :: [(Reference, Name, [Metadata.Value])] =
      [ (r, n, toList $ getAddedMetadata r n typesDiff )
      | (r, n) <- R.toList . BranchDiff.adds $ typesDiff ]
    in for typeAdds $ \(r, n, mdRefs) ->
      (,,) <$> pure (Names2.hqTypeName hqLen names2 n r)
           <*> declOrBuiltin r
           <*> fillMetadata ppe (mempty { addedMetadata = mdRefs })

  addedTerms :: [TermDisplay v a] <- let
    termAdds :: [(Referent, Name, [Metadata.Value])]=
      [ (r, n, toList $ getAddedMetadata r n termsDiff )
      | (r, n) <- R.toList . BranchDiff.adds $ termsDiff ]
    in for termAdds $ \(r, n, mdRefs) ->
      (,,) <$> pure (Names2.hqTermName hqLen names2 n r)
           <*> typeOf r
           <*> fillMetadata ppe (mempty { addedMetadata = mdRefs })

  let addedPatches :: [PatchDisplay] =
        [ (name, diff)
        | (name, BranchDiff.Create diff) <- Map.toList patchesDiff ]

  removedTypes :: [TypeDisplay v a] <- let
    typeRemoves :: [(Reference, Name, [Metadata.Value])] =
      [ (r, n, toList $ getRemovedMetadata r n typesDiff )
      | (r, n) <- R.toList . BranchDiff.removes $ typesDiff ]
    in for typeRemoves $ \(r, n, mdRefs) ->
      (,,) <$> pure (Names2.hqTypeName hqLen names1 n r)
           <*> declOrBuiltin r
           <*> fillMetadata ppe (mempty { removedMetadata = mdRefs })

  removedTerms :: [TermDisplay v a] <- let
    termRemoves :: [(Referent, Name, [Metadata.Value])] =
      [ (r, n, toList $ getRemovedMetadata r n termsDiff )
      | (r, n) <- R.toList . BranchDiff.removes $ termsDiff ]
    in for termRemoves $ \(r, n, mdRefs) ->
      (,,) <$> pure (Names2.hqTermName hqLen names1 n r)
           <*> typeOf r
           <*> fillMetadata ppe (mempty { removedMetadata = mdRefs })

  let removedPatches :: [PatchDisplay] =
        [ (name, diff)
        | (name, BranchDiff.Delete diff) <- Map.toList patchesDiff ]

  let movedOrCopiedTerm :: Map Referent (Set Name, Set Name) -> m [RenameTermDisplay v a]
      movedOrCopiedTerm copiesOrMoves =
        for (Map.toList copiesOrMoves) $ \(r, (ol'names, new'names)) ->
          (,,,) <$> pure r
                <*> typeOf r
                <*> pure (Set.map (\n -> Names2.hqTermName hqLen names1 n r) ol'names)
                <*> pure (Set.map (\n -> Names2.hqTermName hqLen names2 n r) new'names)

  let movedOrCopiedType :: Map Reference (Set Name, Set Name) -> m [RenameTypeDisplay v a]
      movedOrCopiedType copiesOrMoves =
        for (Map.toList copiesOrMoves) $ \(r, (ol'names, new'names)) ->
          (,,,) <$> pure r
                <*> declOrBuiltin r
                <*> pure (Set.map (\n -> Names2.hqTypeName hqLen names1 n r) ol'names)
                <*> pure (Set.map (\n -> Names2.hqTypeName hqLen names2 n r) new'names)

  movedTypes :: [RenameTypeDisplay v a] <- movedOrCopiedType (BranchDiff.tmoves typesDiff)
  movedTerms :: [RenameTermDisplay v a] <- movedOrCopiedTerm (BranchDiff.tmoves termsDiff)
  copiedTypes :: [RenameTypeDisplay v a] <- movedOrCopiedType (BranchDiff.tcopies typesDiff)
  copiedTerms :: [RenameTermDisplay v a] <- movedOrCopiedTerm (BranchDiff.tcopies termsDiff)

  pure $ BranchDiffOutput
    updatedTypes
    updatedTerms
    propagatedUpdates
    updatedPatches
    addedTypes
    addedTerms
    addedPatches
    removedTypes
    removedTerms
    removedPatches
    movedTypes
    movedTerms
    copiedTypes
    copiedTerms
  where
  fillMetadata :: PPE.PrettyPrintEnv -> MetadataDiff Metadata.Value -> m (MetadataDiff (MetadataDisplay v a))
  fillMetadata ppe = traverse $ -- metadata values are all terms
    \(Referent.Ref -> mdRef) -> (HQ'.unsafeFromHQ $ PPE.termName ppe mdRef,) <$> typeOf mdRef
  _getMetadata :: Ord r => r -> Name -> R3.Relation3 r Name Metadata.Value -> Set Metadata.Value
  _getMetadata r n r3 = R.lookupDom n . R3.lookupD1 r $ r3

  getAddedMetadata, getRemovedMetadata :: Ord r => r -> Name -> BranchDiff.DiffSlice r -> Set Metadata.Value
  getAddedMetadata r n slice = _getMetadata r n $ BranchDiff.taddedMetadata slice
  getRemovedMetadata r n slice = _getMetadata r n $ BranchDiff.tremovedMetadata slice

-- references for definitions that were updated

-- two ways of computing updates
--   the stuff in the patch is a primary update
--   if the hash associated with a name has changed and it's not a primary
--   update, that's a secondary update (heuristic)
--
-- stuff that's added but not in updates is treated as an add
-- stuff that's removed but not in updates is treated as a remove

-- Idea A: basically redo propagation in-memory and compare to the diff updates
--  * May give unexpected results if your propagation algorithm is different
--    from the one that made the changes
--
-- vvv currently doing vvv
-- Idea B: Anything explicitly in the patch is listed in one section,
--         anything not explicitly in the patch is listed separately,
--         e.g. with an additional command
--
-- Idea C: Structurally compare the old and new defns; if they have the
--         "same structure", put them in the second list.
--
-- vvv going to do vvv
-- Idea D: Record metadata about human vs automatic replacements during
--         `update`/`patch`.
--         When doing an update or propagate, add this metadata to each name
--         that receives an update.
--  two versions of "namespaceUpdates":
--    - machine said it did it
--    - machine didn't say it did it


-- When we show a move or copy (anything involving a set of
-- old names and set of new names), we can render the old names
-- using the old namespace/ppe/something, and render the new
-- names using the new namespace. (!)
-- (Except it can't actually be a PPE because we want `(r,n) -> HQ`)
-- This has the property that you can `view` the old/new
-- identifiers in their respective namespaces, and it's unambiguous.


-- Suppose I initiate the diff with a command:
--   diff.namespace a.b.c a.b.c2
-- How should this affect the prefixing of names in the output?
{--

Updates:

  In the patch:

	1.  foo#abc : Nat -> Nat -> Poop
	2.   ↳ foo#def : Nat -> Nat -> Poop

	3.  bar#ghi     : Nat -> Nat -> Poop
	4.   ↳ bar	   : Poop
	5.   + bar.docs : Doc

	6.  ability Baz x
	7.   + Baz.docs          : Doc
	8.   + MIT               : License
  9.   - AllRightsReserved : License

	10. ability Bar#fgh
	11.  ↳ ability Bar

	12. patch p (added 3 updates, deleted 1)

  Other updates (from the namespace - hash associated w/ foo hash changed):

  <same format as above>

	& 26 auto-propagated updates (based on isPropagated link)

Adds:

	13. ┌ability Yyz         (+1 metadata)
	14. └ability copies.Yyz  (+2 metadata)
	15. ┌ Baz.docs : Doc
	16. └ Frazz.docs : Doc
	17. cat : Nat -> Nat -> Poop  (3 metadata)
	18. patch q

Removes:

	10.  oldn'busted : Nat -> Nat -> Poop
	11.  ability BadType
	12.  patch defunctThingy

Moves:

	13. peach  ┐  =>   ┌  14. moved.peach
  15. peach' ┘       │  16. ooga.booga
                     └  17. ooga.booga2
  18. blah      =>   19. blah2

--	13. peach  ┐  =>   ┌  14. moved.peach
--  15. peach' ┘       │  16. ooga.booga
--                     └  17. ooga.booga2
--	13. ┌peach       ┌  14. moved.peach
--  15. └peach'      ├  16. ooga.booga
--                   └  17. ooga.booga2
--
--	13. peach      ┐┌  14. moved.peach
--  15. peach'     ┴┼  16. ooga.booga
--                  └  17. ooga.booga2
--
--  1. peach, 2. peach' => 3. moved.peach, 4. ooga.booga
--  1. peach, 2. peach' => 3. moved.peach,
--                         4. ooga.booga
--                         5. ooga.booga2
--
--
--  [1] peach, [2] peach' => [3] moved.peach
--                           [4] ooga.booga
--                           [5] ooga.booga2
--  [1] peach
--  [2] peach'
--  =>
--  [3] moved.peach
--  [4] ooga.booga
--  [5] ooga.booga2
--
--  1. peach
--  2. peach'
--  =>
--  3. moved.peach
--  4. ooga.booga
--  5. ooga.booga2
--
--	15. unmoved.almond almond

Copied:

	15. mouse          copy.foo
	16. dog	           copy.dog
-}
