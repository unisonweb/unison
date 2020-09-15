module U.Codebase.Branch where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import U.Codebase.TermEdit (TermEdit)
import U.Codebase.TypeEdit (TypeEdit)
import U.Util.Hash (Hash)

newtype NameSegment = NameSegment Text
newtype EditHash = EditHash Hash
newtype CausalHash = CausalHash Hash
newtype BranchHash = BranchHash Hash
newtype MdValues = MdValues (Set Reference)
newtype PatchHash = PatchHash Hash

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