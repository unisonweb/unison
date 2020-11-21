module U.Codebase.Sqlite.Patch.Diff where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalHashId, LocalTextId)
import U.Codebase.Sqlite.Patch.TermEdit (TermEdit')
import U.Codebase.Sqlite.Patch.TypeEdit (TypeEdit')

type LocalPatchDiff = PatchDiff' LocalTextId LocalHashId LocalDefnId

data PatchDiff' t h d = PatchDiff
  { addedTermEdits :: Map (Referent' (Reference' t h) (Reference' t h)) (Set (TermEdit' t d)),
    addedTypeEdits :: Map (Reference' t h) (Set (TypeEdit' t d)),
    removedTermEdits :: Map (Referent' (Reference' t h) (Reference' t h)) (Set (TermEdit' t d)),
    removedTypeEdits :: Map (Reference' t h) (Set (TypeEdit' t d))
  }
  deriving (Eq, Ord, Show)
