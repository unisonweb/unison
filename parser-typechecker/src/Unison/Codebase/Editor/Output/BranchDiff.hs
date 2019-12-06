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

toOutput :: B.BranchDiff 
         -> P.Patch
         -> BranchDiffOutput Reference Reference P.Patch
toOutput b p = BranchDiffOutput updates propagatedUpdates adds removes moves copies
  where
  -- references for definitions that were updated


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

  -- adds are addedTerms that aren't update new-terms
  adds :: [(Thing Reference Reference _, [Reference])]
  adds = [ (Term added, snd <$> Set.toList md)
         | (Referent.Ref added, md) <- Map.toList . R.toMultimap . Star3.d3 $ B.addedTerms b
         , Set.notMember added updateNewTerms ]
     <>  [ (Type added, snd <$> Set.toList md)
         | (added, md) <- Map.toList . R.toMultimap . Star3.d3 $ B.addedTypes b
         , Set.notMember added updateNewTerms ]

  updateNewTerms :: Set Reference
  updateNewTerms = undefined

  removes = undefined
  moves = undefined
  copies = undefined

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

	4. ability Bar#def 
	 ↳ ability Bar

	5. patch p (added 3 updates, deleted 1)

  Other updates (from the namespace - hash associated w/ foo hash changed):

  <same format as above>

	& 26 auto-propagated updates (based on isPropagated link)

Adds:
	
	6.  ability Bar         (+1 metadata)
		          copies.Bar' (+2 metadata)
	7.  Baz.docs : Doc
	8.  foo : Nat -> Nat -> Poop  (3 metadata)
	9.  patch q

Removes:

	10.  oldn'busted : Nat -> Nat -> Poop
	11.  ability BadType
	12.  patch defunctThingy

Moves:

	    Original name	 New name
	13. foo			       moved.foo
	14. unmoved.bar	   bar
	15. patch s		     patch moved.s
		
Copied:

	    Original name	 New name(s)
	16. cat			       copy.foo
	17. dog			       copy.dog
	18. patch t		     patch copy.t
-}
