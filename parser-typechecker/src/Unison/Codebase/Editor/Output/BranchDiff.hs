{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.Output.BranchDiff where

import Unison.Name (Name)
import qualified Unison.Codebase.Patch as P
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Codebase.BranchDiff as BranchDiff
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation3 as R3
import qualified Unison.Codebase.Metadata as Metadata
import qualified Data.Set as Set
import qualified Data.Map as Map

import Unison.Reference (Reference)
import Unison.Type (Type)
import Unison.ConstructorType (ConstructorType)
import Unison.HashQualified' (HashQualified)
import qualified Unison.HashQualified' as HQ'
import Unison.Prelude (for)
import qualified Unison.Referent as Referent
import Unison.Referent (Referent)
import Data.Set (Set)
import Unison.Names3 (Names)

data Thing tm ty md patch
  = Term tm (MetadataDiff md)
  | Type ty (MetadataDiff md)
  | Patch patch P.PatchDiff
  deriving (Ord,Eq)

data MetadataDiff tm =
  MetadataDiff { addedMetadata :: [tm]
               , removedMetadata :: [tm] }
               deriving (Ord,Eq,Functor,Foldable,Traversable)

instance Semigroup (MetadataDiff tm) where
  a <> b = MetadataDiff (addedMetadata a <> addedMetadata b)
                        (removedMetadata a <> removedMetadata b)

instance Monoid (MetadataDiff tm) where
  mempty = MetadataDiff mempty mempty
  mappend = (<>)

data BranchDiffOutput v a = BranchDiffOutput {
  updatedTypes      :: [TypeDisplay v a],
  updatedTerms      :: [TermDisplay v a],
  propagatedUpdates :: Int,
  updatedPatches    :: [PatchDisplay],
  addedTypes        :: [TypeDisplay v a],
  addedTerms        :: [TermDisplay v a],
  addedPatches      :: [PatchDisplay],
  removedTypes      :: [TypeDisplay v a],
  removedTerms      :: [TermDisplay v a],
  removedPatches    :: [PatchDisplay],
  movedTypes        :: [RenameTypeDisplay],
  movedTerms        :: [RenameTermDisplay v a],
  copiedTypes       :: [RenameTypeDisplay],
  copiedTerms       :: [RenameTermDisplay v a]
 }

-- Need to be able to turn a (Name,Reference) into a HashQualified relative to... what.
-- the new namespace?

type TermDisplay v a = (HashQualified, Type v a, MetadataDiff (MetadataDisplay v a))
type TypeDisplay v a = (HashQualified, ConstructorType, MetadataDiff (MetadataDisplay v a))
type MetadataDisplay v a = (HashQualified, Type v a)
type RenameTermDisplay v a = (Referent, Type v a, Set HashQualified, Set HashQualified)
type RenameTypeDisplay = (Referent, ConstructorType, Set HashQualified, Set HashQualified)
type PatchDisplay = (Name, P.PatchDiff)

toOutput :: forall m v a
          . Monad m
         => (Referent -> m (Type v a))
         -> (Reference -> m ConstructorType)
         -> Int
         -> Names
         -> Names
         -> BranchDiff.BranchDiff
         -> m (BranchDiffOutput v a)
toOutput typeOf ctorType hqLen names1 names2 diff = do
  let ppe1 = PPE.fromNames hqLen names1
      ppe2 = PPE.fromNames hqLen names2
--  -- "Propagated" metadata should be present on new, propagated updates.
--  -- What if it's "Propagated" *and* in the patch?  Report as part of the patch.
--  let _propagatedTypes :: Relation Reference Name =
--        R3.lookupD3 isPropagatedValue
--        . BranchDiff.taddedMetadata
--        $ BranchDiff.typesDiff diff
--      _propagatedTerms :: Relation Referent Name =
--        R3.lookupD3 isPropagatedValue
--        . BranchDiff.taddedMetadata
--        $ BranchDiff.termsDiff diff

  updatedTypes :: [TypeDisplay v a] <- undefined

  updatedTerms :: [TermDisplay v a] <- undefined

  propagatedUpdates :: Int <- undefined

  let updatedPatches :: [PatchDisplay] =
        [(name, diff) | (name, BranchDiff.Modify diff) <-
                            Map.toList . BranchDiff.patchesDiff $ diff]

  addedTypes :: [TypeDisplay v a] <-
    let typeAdds :: [(Reference, [Metadata.Value])] =
          [ (r, getMetadata r n (BranchDiff.typesDiff diff) )
          | (r, n) <- R.toList . BranchDiff.adds $ BranchDiff.typesDiff diff ]
    in for typeAdds $ \(r, mdRefs) ->
      (,,) <$> pure (HQ'.unsafeFromHQ $ PPE.typeName ppe2 r)
           <*> ctorType r
           <*> fillMetadata ppe2 (mempty { addedMetadata = mdRefs })

  addedTerms :: [TermDisplay v a] <-
    let termAdds :: [(Referent, [Metadata.Value])]=
          [ (r, getMetadata r n (BranchDiff.termsDiff diff) )
          | (r, n) <- R.toList . BranchDiff.adds $ BranchDiff.termsDiff diff ]
    in for termAdds $ \(r, mdRefs) ->
      (,,) <$> pure (HQ'.unsafeFromHQ $ PPE.termName ppe2 r)
           <*> typeOf r
           <*> fillMetadata ppe2 (mempty { addedMetadata = mdRefs })

  let addedPatches :: [PatchDisplay] =
        [(name, diff) | (name, BranchDiff.Create diff) <-
                            Map.toList . BranchDiff.patchesDiff $ diff]

  removedTypes :: [TypeDisplay v a] <-
    let typeRemoves :: [(Reference, [Metadata.Value])] =
          [ (r, getMetadata r n (BranchDiff.typesDiff diff) )
          | (r, n) <- R.toList . BranchDiff.removes $ BranchDiff.typesDiff diff ]
    in for typeRemoves $ \(r, mdRefs) ->
      (,,) <$> pure (HQ'.unsafeFromHQ $ PPE.typeName ppe1 r)
           <*> ctorType r
           <*> fillMetadata ppe1 (mempty { removedMetadata = mdRefs })

  removedTerms :: [TermDisplay v a] <-
    let termRemoves :: [(Referent, [Metadata.Value])]=
          [ (r, getMetadata r n (BranchDiff.termsDiff diff) )
          | (r, n) <- R.toList . BranchDiff.removes $ BranchDiff.termsDiff diff ]
    in for termRemoves $ \(r, mdRefs) ->
      (,,) <$> pure (HQ'.unsafeFromHQ $ PPE.termName ppe1 r)
           <*> typeOf r
           <*> fillMetadata ppe1 (mempty { removedMetadata = mdRefs })

  let removedPatches :: [PatchDisplay] =
        [(name, diff) | (name, BranchDiff.Delete diff) <-
                            Map.toList . BranchDiff.patchesDiff $ diff]

  movedTypes :: [RenameTypeDisplay] <- undefined
  movedTerms :: [RenameTermDisplay v a] <- undefined
  copiedTypes :: [RenameTypeDisplay] <- undefined
  copiedTerms :: [RenameTermDisplay v a] <- undefined

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
  getMetadata :: Ord r => r -> Name -> BranchDiff.DiffSlice r -> [Metadata.Value]
  getMetadata r n slice
    = Set.toList
    . R.lookupDom n
    . R3.lookupD1 r
    $ BranchDiff.taddedMetadata slice

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

	6.  ability Baz
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
