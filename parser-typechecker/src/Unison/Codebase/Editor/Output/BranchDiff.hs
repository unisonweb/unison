{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unison.Codebase.Editor.Output.BranchDiff where

import qualified Unison.Util.Relation as R
import Unison.ConstructorType (ConstructorType)
import Unison.HashQualified (HashQualified)
import Unison.Name (Name)
import Unison.Parser (Ann)
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import qualified Unison.Codebase.BranchDiff as B
import qualified Unison.Codebase.Patch as P
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Util.Star3 as Star3
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Runtime.IOSource as IOSource

import Unison.Codebase.Metadata (Star)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

data Thing tm ty patch = Term tm | Type ty | Patch patch deriving (Ord,Eq)

data MetadataDiff tm =
  MetadataDiff { addedMetadata :: [tm]
               , removedMetadata :: [tm] }
               deriving (Ord,Eq,Functor,Foldable,Traversable)

hydrateOutput :: Monad m
              => (Reference -> m (Type v a))
              -> (Reference -> m ConstructorType)
              -> PPE.PrettyPrintEnv
              -> BranchDiffOutput Reference Reference P.Patch
              -> m (BranchDiffOutput
                      (HashQualified, Type v a)
                      HashQualified
                      (Name, P.PatchDiff))
hydrateOutput typeOf ctorType ppe diff = undefined

data BranchDiffOutput tm ty patch = BranchDiffOutput {
  -- if fst pair /= snd pair, then the definition was replaced;
  -- else just a metadata change.
  -- MetadataDiff is metadata on new - metadata on old
  updates           :: [(Thing (tm,tm) (ty,ty) (Name,patch,patch), MetadataDiff tm)],
  propagatedUpdates :: Int,
  adds              :: [(Thing tm ty patch, [tm])],
  removes           :: [Thing tm ty patch],
  moves             :: [(Name, Name, Thing tm ty patch)],
                    --   ^old  ^new
  copies            :: [(Name, Name, Thing tm ty patch)] }

data ThingIn r = ThingIn {
  names :: R.Relation r Name,
  metadata :: R.Relation r (Name, Metadata.Value)
}

data ThingOut r = ThingOut {
  tpatchUpdates :: R.Relation r r, -- old new
  tnamespaceUpdates :: R.Relation (r, r) Name,
  tadds :: R.Relation r Name,
  tremoves :: R.Relation r Name,
  -- moves / copies will be cross-product / duplicated data
  tcopies :: R.Relation r (Name, Name), -- ref (old, new)
  tmoves :: R.Relation r (Name, Name),
  taddedMetadata :: R.Relation r (Name, Metadata.Value),
  tremovedMetadata :: R.Relation r (Name, Metadata.Value)
}

thingThing :: ThingIn Referent -> ThingIn Referent -> P.Patch -> (ThingOut Referent, ThingOut Reference)
thingThing oldTerms newTerms p = undefined where
--  termsOut = ThingOut tmPatUpdates tmNsUpdates tmAdds tmRemoves tmCopies tmMoves tmAddMd tmRmMd
--  typesOut = ThingOut tpPatUpdates tpNsUpdates tpAdds tpRemoves tpCopies tpMoves tpAddMd tpRmMd
  copies :: Ord r => ThingIn r -> ThingIn r -> Map r (Set Name, Set Name)
  copies old new =
    -- pair the set of old names with the set of names that are only new
    R.toUnzippedMultimap $
      names old `R.joinDom` (names new `R.difference` names old)

  moves :: Ord r => ThingIn r -> ThingIn r -> Map r (Set Name, Set Name)
  moves old new =
    R.toUnzippedMultimap $
      (names old `R.difference` names new)
        `R.joinDom` (names new `R.difference` names old)

  adds :: Ord r => ThingIn r -> ThingIn r -> R.Relation r r -> R.Relation r Name
  adds old new edits =
    R.subtractDom (R.ran edits) (names new `R.difference` names old)

  removes :: Ord r => ThingIn r -> ThingIn r -> R.Relation r r -> R.Relation r Name
  removes old new edits =
    R.subtractDom (R.dom edits) (names old `R.difference` names new)

  termPatchUpdates :: P.Patch -> R.Relation Referent Referent
  termPatchUpdates p = R.fromList
    [ (Referent.Ref old, Referent.Ref new)
    | (old, TermEdit.Replace new _) <- R.toList $ P._termEdits p ]

  typePatchUpdates :: P.Patch -> R.Relation Reference Reference
  typePatchUpdates p = R.fromList
    [ (old, new)
    | (old, TypeEdit.Replace new) <- R.toList $ P._typeEdits p ]

  namespaceUpdates :: Ord r => ThingIn r -> ThingIn r -> R.Relation r r -> R.Relation (r, r) Name
  namespaceUpdates old new edits =
    R.filterDom f (names old `R.joinRan` names new)
    where f (old, new) = old /= new && R.notMember old new edits

  addedMetadata :: Ord r => ThingIn r -> ThingIn r -> R.Relation r (Name, Metadata.Value)
  addedMetadata old new =
    R.collectRan matchMdName
      (names new `R.joinDom` (metadata new `R.difference` metadata old))

  removedMetadata :: Ord r => ThingIn r -> ThingIn r -> R.Relation r (Name, Metadata.Value)
  removedMetadata old new =
    R.collectRan matchMdName 
      (names old `R.joinDom` (metadata old `R.difference` metadata new))
    
  matchMdName (n1, p@(n2, _)) = if n1 == n2 then Just p else Nothing

--toOutput :: ThingIn
--         -> ThingIn
--         -> P.Patch
--         -> BranchDiffOutput Reference Reference P.Patch
--toOutput old new p =
--  undefined -- BranchDiffOutput updates propagatedUpdates adds removes moves copies
--  where
--  -- references for definitions that were updated

--addedMetadata(ref, name, md) :-
--  newTerms.names(ref, name),
--  newTerms.metadata(ref, (name,md)),
--  !oldTerms.metadata(ref, (name,md)).
--
--removedMetadata(ref, name, md) :-
--  oldTerms.names(ref, name),
--  oldTerms.metadata(ref, (name,md)),
--  !newTerms.metadata(ref, (name,md)).
--
--tm.patchUpdates(old, new) :-
--  patch.termEdits(old, new).
--
--tm.namespaceUpdates(old, new, name) :-
--  oldTerms.names(old, name),
--  newTerms.names(new, name),
--  old != new,
--  !tm.patchUpdates(old,new).
--
--tm.adds(ref, name) :-
--  !oldTerms.names(ref, name),
--  newTerms.names(ref, name),
--  !patchUpdates(_, ref).
--
--tm.removes(ref, name) :-
--  oldTerms.names(ref, name),
--  !newTerms.names(ref, name),
--  !patchUpdates(ref, _).
--
--tm.moves(ref, oldName, newName) :-
--  oldTerms.names(ref, oldName),
--  !oldTerms.names(ref, newName),
--  !newTerms.names(ref, oldName),
--  newTerms.names(ref, newName),
--  !tm.patchUpdates(_oldRef, ref). -- do we want this clause? we decided not to implement it.

--tm.copies(ref, oldName, newName) :-
--  oldTerms.names(ref, oldName),
--  !oldTerms.names(ref, newName),
--  newTerms.names(ref, oldName),
--  newTerms.names(ref, newName),
--  oldName != newName.
--
--allUpdates(old, new) :-
--  tm.namespaceUpdates(old, new, _).
--allUpdates(old, new) :-
--  tm.patchUpdates(old, new).

{-

  updates = patchUpdates <> namespaceUpdates
  --1. updates specified in the patch, where
        --data Patch = Patch
        --  { _termEdits :: Relation Reference TermEdit
        --  , _typeEdits :: Relation Reference TypeEdit
        --  } deriving (Eq, Ord, Show)
  patchUpdates, namespaceUpdates :: [(Thing (Reference, Reference) (Reference, Reference) (Name, P.Patch, P.Patch), MetadataDiff Reference)]
  patchUpdates =
    [ (Term (old, new), metadataDiff old new) |
      (old, TermEdit.Replace new _typing) <- R.toList $ P._termEdits p ] <>
    [ (Type (old, new), metadataDiff old new) |
      (old, TypeEdit.Replace new) <- R.toList $ P._typeEdits p ]

  --2. updates detected in the namespace (a name has been both added and removed)
  namespaceUpdates = namespaceUpdates' (not.automatic)
  namespaceUpdates' include =
    [ (Term (old, new), metadataDiff old new) |
      ((Referent.Ref old, Referent.Ref new), _name) <-
        R.toList $ R.joinRan (Star3.d1 $ B.removedTerms b) (Star3.d1 $ B.addedTerms b)
    , include new
    ] <>
    [ (Type (old, new), metadataDiff old new) |
      ((old, new), _name) <-
        R.toList $ R.joinRan (Star3.d1 $ B.removedTypes b) (Star3.d1 $ B.addedTypes b)
    , include new
    ]

  propagatedUpdates = length (namespaceUpdates' automatic)

  automatic :: Reference -> Bool
  automatic new =
    Metadata.hasMetadata (Referent.Ref new) propType propValue (B.addedTerms b) ||
    Metadata.hasMetadata new propType propValue (B.addedTypes b)
    where (propType, propValue) = IOSource.isPropagated

  -- A definition has an `old` set of names and a `new` set of names.
  -- If old ⊃ new, then names were deleted.
  -- If old ⊂ new, then names have been added
  -- If new == empty, then definition was deleted (or maybe updated)
  -- If old == empty, then definition was added (or maybe the result of an update)
  -- If old `symmetricDifference` new /= empty then there have been moves/aliases


  moves = undefined
  copies = undefined

  -- adds are addedTerms that aren't update new-terms
  adds, removes :: [(Thing Reference Reference _, [Reference])]
  adds = [ (Term added, snd <$> Set.toList md)
         | (Referent.Ref added, md) <- Map.toList . R.toMultimap . Star3.d3 $ B.addedTerms b
         , Set.notMember added newDefnsConsideredUpdates ]
     <>  [ (Type added, snd <$> Set.toList md)
         | (added, md) <- Map.toList . R.toMultimap . Star3.d3 $ B.addedTypes b
         , Set.notMember added newDefnsConsideredUpdates ]

  -- the `new` terms in an `old -> new` update.
  newDefnsConsideredUpdates :: Set Reference
  newDefnsConsideredUpdates = Set.fromList $
    [ new | (Term (_old, new), _mdd) <- updates ] <>
    [ new | (Type (_old, new), _mdd) <- updates ]

  oldDefnsConsideredUpdates :: Set Reference
  oldDefnsConsideredUpdates = Set.fromList $
    [ old | (Term (old, _new), _mdd) <- updates ] <>
    [ old | (Type (old, _new), _mdd) <- updates ]

  removes =
    [ (Term r, snd <$> Set.toList md)
    | (Referent.Ref r, md) <- Map.toList . R.toMultimap . Star3.d3 $ B.removedTerms b
    , Set.notMember r oldDefnsConsideredUpdates ] <>
    [ (Type r, snd <$> Set.toList md)
    | (r, md) <- Map.toList . R.toMultimap . Star3.d3 $ B.removedTypes b
    , Set.notMember r oldDefnsConsideredUpdates
    ]


  metadataDiff :: Reference -> Reference -> MetadataDiff Reference
  metadataDiff old new = MetadataDiff added removed where
    added =
      (fmap snd . Set.toList $
        R.lookupDom (Referent.Ref new) (Star3.d3 . B.addedTerms $ b)) <>
      (fmap snd . Set.toList $ R.lookupDom new (Star3.d3 . B.addedTypes $ b))
    removed =
      (fmap snd . Set.toList $
        R.lookupDom (Referent.Ref new) (Star3.d3 . B.removedTerms $ b)) <>
      (fmap snd . Set.toList $ R.lookupDom new (Star3.d3 . B.removedTypes $ b))


  --2b. updates tagged as Automatic (a name has been both added and removed,
  --                                 and new reference has Automatic metadata)

--  termUpdateStar :: Star Referent Name
--  termUpdateStar = undefined


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
-- Idea B: Anything explicitly in the patch is listed in one section,
--         anything not explicitly in the patch is listed separately,
--         e.g. with an additional command
--
-- Idea C: Structurally compare the old and new defns; if they have the
--         "same structure", put them in the second list.
--
-- Idea D: Record metadata about human vs automatic replacements during
--         `update`/`patch`.
--         When doing an update or propagate, add this metadata to each name
--         that receives an update.
--
-}

{-

data Patch = Patch
  { _termEdits :: Relation Reference TermEdit
  , _typeEdits :: Relation Reference TypeEdit
  } deriving (Eq, Ord, Show)

Updates:

  In the patch:

	1. foo#abc : Nat -> Nat -> Poop
	 ↳ foo#def : Nat -> Nat -> Poop

	2. bar#ghi : Nat -> Nat -> Poop
	 ↳ bar	   : Poop
		 + bar.docs : Doc

	3. ability Baz
		 + Baz.docs          : Doc
		 + MIT               : License
		 - AllRightsReserved : License

	4. ability Bar#fgh
	 ↳ ability Bar

	5. patch p (added 3 updates, deleted 1)

  Other updates (from the namespace - hash associated w/ foo hash changed):

  <same format as above>

	& 26 auto-propagated updates (based on isPropagated link)

Adds:

	6.  ability Yyz         (+1 metadata)
		          copies.Yyz  (+2 metadata)
	7.  Baz.docs : Doc
	8.  cat : Nat -> Nat -> Poop  (3 metadata)
	9.  patch q

Removes:

	10.  oldn'busted : Nat -> Nat -> Poop
	11.  ability BadType
	12.  patch defunctThingy

Moves:

	    Original name	 New name
	13. peach          moved.peach
	14. unmoved.almond almond
	15. patch s		     patch moved.s

Copied:

	    Original name	 New name(s)
	16. mouse          copy.foo
	17. dog	           copy.dog
	18. patch t	       patch copy.t
-}
