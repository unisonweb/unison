{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Unison.Codebase.Editor.Output.BranchDiff where

import Unison.ConstructorType (ConstructorType)
import Unison.HashQualified (HashQualified)
import Unison.Name (Name)
import Unison.Parser (Ann)
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import qualified Unison.Codebase.BranchDiff as B
import qualified Unison.Codebase.Patch as P
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as R
-- import Unison.Codebase

data Thing tm ty patch = Term tm | Type ty | Patch patch deriving (Ord,Eq)

data MetadataDiff tm =
  MetadataDiff { addedMetadata :: [tm]
               , removedMetadata :: [tm] } 
               deriving (Ord,Eq,Functor,Foldable,Traversable)

data BranchDiffOutput tm ty patch = BranchDiffOutput {
  updates           :: [(Thing (tm,tm) (ty,ty) (Name,patch,patch), MetadataDiff tm)],
  propagatedUpdates :: Maybe Int, -- Nothing if no patch
  adds              :: [(Thing tm ty patch, MetadataDiff tm)],
  removes           :: [Thing tm ty patch],
  moves             :: [(Name, Name, Thing tm ty patch)],
                    --   ^old  ^new
  copies            :: [(Name, Name, Thing tm ty patch)] }

toOutput :: B.BranchDiff 
         -> P.Patch 
         -> BranchDiffOutput R.Reference R.Reference P.Patch
toOutput b p = undefined

hydrateOutput :: Monad m 
              => (R.Reference -> m (Type v a))
              -> (R.Reference -> m ConstructorType)
              -> PPE.PrettyPrintEnv 
              -> BranchDiffOutput R.Reference R.Reference P.Patch 
              -> m (BranchDiffOutput 
                      (HashQualified, Type v a)
                      HashQualified
                      (Name, P.PatchDiff))
hydrateOutput typeOf ctorType ppe diff = undefined

-- two ways of computing updates
--   the stuff in the patch is a primary update
--   if the hash associated with a name has changed and it's not a primary
--   update, that's a secondary update (heuristic)
--
-- stuff that's added but not in updates is treated as an add
-- stuff that's removed but not in updates is treated as a remove

{-

data Patch = Patch
  { _termEdits :: Relation Reference TermEdit
  , _typeEdits :: Relation Reference TypeEdit
  } deriving (Eq, Ord, Show)

Updates:

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

	& 26 auto-propagated updates

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
