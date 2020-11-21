module U.Codebase.Sqlite.Patch.Full where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import qualified U.Codebase.Sqlite.DbId as Db
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalHashId, LocalTextId)
import U.Codebase.Sqlite.Patch.TermEdit (TermEdit')
import U.Codebase.Sqlite.Patch.TypeEdit (TypeEdit')

type Patch = Patch' Db.TextId Db.HashId Db.ObjectId

type LocalPatch = Patch' LocalTextId LocalHashId LocalDefnId

data Patch' t h o = Patch
  { termEdits :: Map (Referent' (Reference' t h) (Reference' t h)) (Set (TermEdit' t o)),
    typeEdits :: Map (Reference' t h) (Set (TypeEdit' t o))
  }
