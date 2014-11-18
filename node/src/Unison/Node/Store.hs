module Unison.Node.Store where

import Data.Set (Set)
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Reference (Reference)
import Unison.Syntax.Type (Type)
import Unison.Syntax.Term (Term)
import Unison.Node.Metadata (Metadata)
import Unison.Note (Noted)

data Store f = Store {
  hashes :: Maybe (Set Reference) -> Noted f (Set Reference), -- ^ The set of hashes in this store, optionally constrained to intersect the given set
  readTerm :: Hash -> Noted f Term,
  writeTerm :: Hash -> Term -> Noted f (),
  readType :: Hash -> Noted f Type,
  writeType :: Hash -> Type -> Noted f (),
  readMetadata :: Reference -> Noted f (Metadata Hash),
  writeMetadata :: Reference -> Metadata Hash -> Noted f ()
}
