{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Patch where

import Control.Lens hiding (children, cons, transform)
import Data.Set qualified as Set
import Unison.Codebase.TermEdit (TermEdit, Typing (Same))
import Unison.Codebase.TermEdit qualified as TermEdit
import Unison.Codebase.TypeEdit (TypeEdit)
import Unison.Codebase.TypeEdit qualified as TypeEdit
import Unison.Prelude hiding (empty)
import Unison.Reference (Reference)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as R
import Prelude hiding (head, read, subtract)

data Patch = Patch
  { _termEdits :: Relation Reference TermEdit,
    _typeEdits :: Relation Reference TypeEdit
  }
  deriving (Eq, Ord, Show)

data PatchDiff = PatchDiff
  { _addedTermEdits :: Relation Reference TermEdit,
    _addedTypeEdits :: Relation Reference TypeEdit,
    _removedTermEdits :: Relation Reference TermEdit,
    _removedTypeEdits :: Relation Reference TypeEdit
  }
  deriving (Eq, Ord, Show)

makeLenses ''Patch
makeLenses ''PatchDiff

diff :: Patch -> Patch -> PatchDiff
diff new old =
  PatchDiff
    { _addedTermEdits = R.difference (view termEdits new) (view termEdits old),
      _addedTypeEdits = R.difference (view typeEdits new) (view typeEdits old),
      _removedTypeEdits = R.difference (view typeEdits old) (view typeEdits new),
      _removedTermEdits = R.difference (view termEdits old) (view termEdits new)
    }

empty :: Patch
empty = Patch mempty mempty

-- | Returns the set of references which are the target of an arrow in the patch
allReferenceTargets :: Patch -> Set Reference
allReferenceTargets p = typeReferences p <> termReferences p
  where
    typeReferences p =
      Set.fromList
        [new | (_, TypeEdit.Replace new) <- R.toList (_typeEdits p)]
    termReferences p =
      Set.fromList
        [new | (_, TermEdit.Replace new _) <- R.toList (_termEdits p)]

updateTerm ::
  (Reference -> Reference -> Typing) ->
  Reference ->
  TermEdit ->
  Patch ->
  Patch
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
   in p {_termEdits = edits'}

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
   in p {_typeEdits = edits'}

instance Semigroup Patch where
  a <> b =
    Patch
      (_termEdits a <> _termEdits b)
      (_typeEdits a <> _typeEdits b)

instance Monoid Patch where
  mempty = Patch mempty mempty

instance Semigroup PatchDiff where
  a <> b =
    PatchDiff
      { _addedTermEdits = _addedTermEdits a <> _addedTermEdits b,
        _addedTypeEdits = _addedTypeEdits a <> _addedTypeEdits b,
        _removedTermEdits = _removedTermEdits a <> _removedTermEdits b,
        _removedTypeEdits = _removedTypeEdits a <> _removedTypeEdits b
      }

instance Monoid PatchDiff where
  mempty = PatchDiff mempty mempty mempty mempty
