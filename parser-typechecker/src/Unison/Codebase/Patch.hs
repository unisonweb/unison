{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Patch where

import Unison.Prelude hiding (empty)

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens            hiding ( children, cons, transform )
import qualified Data.Set                      as Set
import           Unison.Codebase.TermEdit       ( TermEdit, Typing(Same) )
import qualified Unison.Codebase.TermEdit      as TermEdit
import           Unison.Codebase.TypeEdit       ( TypeEdit )
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import           Unison.Hashable                ( Hashable )
import qualified Unison.Hashable               as H
import           Unison.Reference               ( Reference )
import qualified Unison.Util.Relation          as R
import           Unison.Util.Relation           ( Relation )
import qualified Unison.LabeledDependency      as LD
import           Unison.LabeledDependency       ( LabeledDependency )

data Patch r = Patch
  { _termEdits :: Relation r (TermEdit r)
  , _typeEdits :: Relation r (TypeEdit r)
  } deriving (Eq, Ord, Show)

data PatchDiff r = PatchDiff
  { _addedTermEdits :: Relation r (TermEdit r)
  , _addedTypeEdits :: Relation r (TypeEdit r)
  , _removedTermEdits :: Relation r (TermEdit r)
  , _removedTypeEdits :: Relation r (TypeEdit r)
  } deriving (Eq, Ord, Show)

makeLenses ''Patch
makeLenses ''PatchDiff

diff :: Ord r => Patch r -> Patch r -> PatchDiff r
diff new old = PatchDiff
  { _addedTermEdits   = R.difference (view termEdits new) (view termEdits old)
  , _addedTypeEdits   = R.difference (view typeEdits new) (view typeEdits old)
  , _removedTypeEdits = R.difference (view typeEdits old) (view typeEdits new)
  , _removedTermEdits = R.difference (view termEdits old) (view termEdits new)
  }

labeledDependencies :: Patch Reference -> Set LabeledDependency
labeledDependencies Patch {..} =
  Set.map LD.termRef (R.dom _termEdits)
    <> Set.fromList
         (fmap LD.termRef $ TermEdit.references =<< toList (R.ran _termEdits))
    <> Set.map LD.typeRef (R.dom _typeEdits)
    <> Set.fromList
         (fmap LD.typeRef $ TypeEdit.references =<< toList (R.ran _typeEdits))

empty :: Ord r => Patch r
empty = Patch mempty mempty

isEmpty :: Ord r => Patch r -> Bool
isEmpty p = p == empty

allReferences :: Ord r => Patch r -> Set r
allReferences p = typeReferences p <> termReferences p where
  typeReferences p = Set.fromList
    [ r | (old, TypeEdit.Replace new) <- R.toList (_typeEdits p)
        , r <- [old, new] ]
  termReferences p = Set.fromList
    [ r | (old, TermEdit.Replace new _) <- R.toList (_termEdits p)
        , r <- [old, new] ]

-- | Returns the set of references which are the target of an arrow in the patch
allReferenceTargets :: Ord r => Patch r -> Set r
allReferenceTargets p = typeReferences p <> termReferences p where
  typeReferences p = Set.fromList
    [ new | (_, TypeEdit.Replace new) <- R.toList (_typeEdits p) ]
  termReferences p = Set.fromList
    [ new | (_, TermEdit.Replace new _) <- R.toList (_termEdits p) ]

updateTerm :: forall r.
  Ord r => (r -> r -> Typing) -> r -> TermEdit r -> Patch r -> Patch r
updateTerm typing r edit p =
  -- get D ~= lookupRan r
  -- for each d ∈ D, remove (d, r) and add (d, r')
  -- add (r, r') and remove (r', r')
  let deleteCycle = case edit of
        TermEdit.Deprecate -> id
        TermEdit.Replace r' _ -> R.delete r' (TermEdit.Replace r' Same)
      edits' :: Relation r (TermEdit r)
      edits' = deleteCycle . R.insert r edit . R.map f $ _termEdits p
      f (x, TermEdit.Replace y _) | y == r = case edit of
        TermEdit.Replace r' _ -> (x, TermEdit.Replace r' (typing x r'))
        TermEdit.Deprecate -> (x, TermEdit.Deprecate)
      f p = p
  in p { _termEdits = edits' }

updateType :: forall r.
  Ord r => r -> TypeEdit r -> Patch r -> Patch r
updateType r edit p =
  let deleteCycle = case edit of
        TypeEdit.Deprecate -> id
        TypeEdit.Replace r' -> R.delete r' (TypeEdit.Replace r')
      edits' :: Relation r (TypeEdit r)
      edits' = deleteCycle . R.insert r edit . R.map f $ _typeEdits p
      f (x, TypeEdit.Replace y) | y == r = case edit of
        TypeEdit.Replace r' -> (x, TypeEdit.Replace r')
        TypeEdit.Deprecate -> (x, TypeEdit.Deprecate)
      f p = p
  in p { _typeEdits = edits' }

conflicts :: Ord r => Patch r -> Patch r
conflicts Patch{..} =
  Patch (R.filterManyDom _termEdits) (R.filterManyDom _typeEdits)

instance Ord r => Semigroup (Patch r) where
  a <> b = Patch (_termEdits a <> _termEdits b)
                 (_typeEdits a <> _typeEdits b)

instance Ord r => Monoid (Patch r) where
  mappend = (<>)
  mempty = Patch mempty mempty

instance Hashable r => Hashable (Patch r) where
  tokens e = [ H.Hashed (H.accumulate (H.tokens (_termEdits e))),
               H.Hashed (H.accumulate (H.tokens (_typeEdits e))) ]

instance Ord r => Semigroup (PatchDiff r) where
  a <> b = PatchDiff
    { _addedTermEdits = _addedTermEdits a <> _addedTermEdits b
    , _addedTypeEdits = _addedTypeEdits a <> _addedTypeEdits b
    , _removedTermEdits = _removedTermEdits a <> _removedTermEdits b
    , _removedTypeEdits = _removedTypeEdits a <> _removedTypeEdits b
    }

instance Ord r => Monoid (PatchDiff r) where
  mappend = (<>)
  mempty = PatchDiff mempty mempty mempty mempty
