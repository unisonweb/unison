module Unison.Codebase.Store where

import Data.Set (Set)
import Unison.Metadata (Metadata)
import Unison.Note (Noted)
import Unison.Reference (Reference)
import Unison.Hash (Hash)
import Unison.Term (Term)
import Unison.Type (Type)

data Store f v = Store {
  hashes :: Maybe (Set Reference) -> Noted f (Set Reference), -- ^ The set of hashes in this store, optionally constrained to intersect the given set
  readTerm :: Hash -> Noted f (Term v),
  writeTerm :: Hash -> Term v -> Noted f (),
  typeOfTerm :: Reference -> Noted f (Type v),
  annotateTerm :: Reference -> Type v -> Noted f (),
  readMetadata :: Reference -> Noted f (Metadata v Reference),
  writeMetadata :: Reference -> Metadata v Reference -> Noted f ()
}

