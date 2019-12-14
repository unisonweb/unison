{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unison.Codebase.Editor.Output.BranchDiff where

import Unison.ConstructorType (ConstructorType)
import Unison.HashQualified (HashQualified)
import Unison.Name (Name)
import Unison.Type (Type)
import qualified Unison.Codebase.Patch as P
import qualified Unison.PrettyPrintEnv as PPE

import Unison.Reference (Reference)

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
hydrateOutput _typeOf _ctorType _ppe _diff = undefined

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

--toOutput :: ThingIn
--         -> ThingIn
--         -> P.Patch
--         -> BranchDiffOutput Reference Reference P.Patch
--toOutput old new p =
--  undefined -- BranchDiffOutput updates propagatedUpdates adds removes moves copies
--  where
--  -- references for definitions that were updated

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

{-

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
