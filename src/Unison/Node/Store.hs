module Unison.Node.Store where

import Data.Set (Set)
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Type (Type)
import Unison.Syntax.Term (Term)
import Unison.Node.Metadata (Metadata)
import Unison.Note (Noted)

data Store f = Store {
  hashes :: Maybe (Set Hash) -> Noted f (Set Hash), -- ^ The set of hashes in this store, optionally constrained to intersect the given set
  readTerm :: Hash -> Noted f Term,
  writeTerm :: Hash -> Term -> Noted f (),
  readType :: Hash -> Noted f Type,
  writeType :: Hash -> Type -> Noted f (),
  readMetadata :: Hash -> Noted f (Metadata Hash),
  writeMetadata :: Hash -> Metadata Hash -> Noted f ()
}
