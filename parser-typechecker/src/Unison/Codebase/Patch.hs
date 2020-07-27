{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Codebase.Patch where

import Unison.Prelude hiding (empty)

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens            hiding ( children, cons, transform )
import qualified Data.Set                      as Set
import           Unison.Codebase.TermEdit       ( TermEdit, TermEditH, Typing(Same) )
import qualified Unison.Codebase.TermEdit      as TermEdit
import           Unison.Codebase.TypeEdit       ( TypeEdit, TypeEditH )
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import           Unison.Hash                    ( Hash )
import           Unison.Hashable                ( Hashable )
import qualified Unison.Hashable               as H
import           Unison.Reference               ( Reference, ReferenceH )
import qualified Unison.Reference              as Reference
import qualified Unison.Util.Relation          as R
import           Unison.Util.Relation           ( Relation )
import qualified Unison.LabeledDependency      as LD
import           Unison.LabeledDependency       ( LabeledDependency )

type Patch = PatchH Hash
-- todo: who's looking these up by TermEdit? These would probably be better off
-- as Map instead of Relation.
data PatchH h = Patch
  { _termEdits :: Relation (ReferenceH h) (TermEditH h)
  , _typeEdits :: Relation (ReferenceH h) (TypeEditH h)
  } deriving (Eq, Ord)
deriving instance Show (ReferenceH h) => Show (PatchH h)

type PatchDiff = PatchDiffH Hash
data PatchDiffH h = PatchDiff
  { _addedTermEdits :: Relation (ReferenceH h) (TermEditH h)
  , _addedTypeEdits :: Relation (ReferenceH h) (TypeEditH h)
  , _removedTermEdits :: Relation (ReferenceH h) (TermEditH h)
  , _removedTypeEdits :: Relation (ReferenceH h) (TypeEditH h)
  } deriving (Eq, Ord)
deriving instance Show (ReferenceH h) => Show (PatchDiffH h)

makeLenses ''PatchH
makeLenses ''PatchDiffH

hmap :: Ord h' => (h -> h') -> PatchH h -> PatchH h'
hmap f (Patch tms tps) =
  Patch
    (R.fromList [ (Reference.hmap f r, TermEdit.hmap f e) | (r, e) <- R.toList tms])
    (R.fromList [ (Reference.hmap f r, TypeEdit.hmap f e) | (r, e) <- R.toList tps])

diff :: Patch -> Patch -> PatchDiff
diff new old = PatchDiff
  { _addedTermEdits   = R.difference (view termEdits new) (view termEdits old)
  , _addedTypeEdits   = R.difference (view typeEdits new) (view typeEdits old)
  , _removedTypeEdits = R.difference (view typeEdits old) (view typeEdits new)
  , _removedTermEdits = R.difference (view termEdits old) (view termEdits new)
  }

labeledDependencies :: Patch -> Set LabeledDependency
labeledDependencies Patch {..} =
  Set.map LD.termRef (R.dom _termEdits)
    <> Set.fromList
         (fmap LD.termRef $ TermEdit.references =<< toList (R.ran _termEdits))
    <> Set.map LD.typeRef (R.dom _typeEdits)
    <> Set.fromList
         (fmap LD.typeRef $ TypeEdit.references =<< toList (R.ran _typeEdits))

empty :: Patch
empty = Patch mempty mempty

isEmpty :: Patch -> Bool
isEmpty p = p == empty

allReferences :: Patch -> Set Reference
allReferences p = typeReferences p <> termReferences p where
  typeReferences p = Set.fromList
    [ r | (old, TypeEdit.Replace new) <- R.toList (_typeEdits p)
        , r <- [old, new] ]
  termReferences p = Set.fromList
    [ r | (old, TermEdit.Replace new _) <- R.toList (_termEdits p)
        , r <- [old, new] ]

-- | Returns the set of references which are the target of an arrow in the patch
allReferenceTargets :: Patch -> Set Reference
allReferenceTargets p = typeReferences p <> termReferences p where
  typeReferences p = Set.fromList
    [ new | (_, TypeEdit.Replace new) <- R.toList (_typeEdits p) ]
  termReferences p = Set.fromList
    [ new | (_, TermEdit.Replace new _) <- R.toList (_termEdits p) ]

updateTerm :: (Reference -> Reference -> Typing)
           -> Reference -> TermEdit -> Patch -> Patch
updateTerm typing r edit p =
  -- get D ~= lookupRan r
  -- for each d ∈ D, remove (d, r) and add (d, r')
  -- add (r, r') and remove (r', r')
  let deleteCycle = case edit of
        TermEdit.Deprecate -> id
        TermEdit.Replace r' _ -> R.delete r' (TermEdit.Replace r' Same)
      edits' :: Relation Reference TermEdit
      edits' = deleteCycle . R.insert r edit . R.map f $ _termEdits p
      f (x, TermEdit.Replace y _) | y == r = case edit of
        TermEdit.Replace r' _ -> (x, TermEdit.Replace r' (typing x r'))
        TermEdit.Deprecate -> (x, TermEdit.Deprecate)
      f p = p
  in p { _termEdits = edits' }

updateType :: Reference -> TypeEdit -> Patch -> Patch
updateType r edit p =
  let deleteCycle = case edit of
        TypeEdit.Deprecate -> id
        TypeEdit.Replace r' -> R.delete r' (TypeEdit.Replace r')
      edits' :: Relation Reference TypeEdit
      edits' = deleteCycle . R.insert r edit . R.map f $ _typeEdits p
      f (x, TypeEdit.Replace y) | y == r = case edit of
        TypeEdit.Replace r' -> (x, TypeEdit.Replace r')
        TypeEdit.Deprecate -> (x, TypeEdit.Deprecate)
      f p = p
  in p { _typeEdits = edits' }

conflicts :: Patch -> Patch
conflicts Patch{..} =
  Patch (R.filterManyDom _termEdits) (R.filterManyDom _typeEdits)

instance Semigroup Patch where
  a <> b = Patch (_termEdits a <> _termEdits b)
                 (_typeEdits a <> _typeEdits b)

instance Monoid Patch where
  mappend = (<>)
  mempty = Patch mempty mempty

instance Hashable Patch where
  tokens e = [ H.Hashed (H.accumulate (H.tokens (_termEdits e))),
               H.Hashed (H.accumulate (H.tokens (_typeEdits e))) ]

instance Semigroup PatchDiff where
  a <> b = PatchDiff
    { _addedTermEdits = _addedTermEdits a <> _addedTermEdits b
    , _addedTypeEdits = _addedTypeEdits a <> _addedTypeEdits b
    , _removedTermEdits = _removedTermEdits a <> _removedTermEdits b
    , _removedTypeEdits = _removedTypeEdits a <> _removedTypeEdits b
    }

instance Monoid PatchDiff where
  mappend = (<>)
  mempty = PatchDiff mempty mempty mempty mempty
