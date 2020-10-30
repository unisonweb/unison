module U.Codebase.Branch where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import U.Codebase.HashTags (PatchHash)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import U.Codebase.TermEdit (TermEdit)
import U.Codebase.TypeEdit (TypeEdit)

newtype NameSegment = NameSegment Text

newtype MdValues = MdValues (Set Reference)

data Branch m = Branch
  { terms :: Map NameSegment (Map Referent (m MdValues)),
    types :: Map NameSegment (Map Reference (m MdValues)),
    patches :: Map NameSegment (PatchHash, m Patch),
    children :: Map NameSegment (m (Branch m))
  }

data Patch = Patch
  { termEdits :: Map Referent TermEdit,
    typeEdits :: Map Reference TypeEdit
  }
