module U.Codebase.Branch where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import U.Codebase.Causal (CausalHead)
import U.Codebase.HashTags (BranchHash, CausalHash, PatchHash)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import U.Codebase.TermEdit (TermEdit)
import U.Codebase.TypeEdit (TypeEdit)

newtype NameSegment = NameSegment Text deriving (Eq, Ord, Show)

newtype MdValues = MdValues (Set Reference) deriving (Eq, Ord, Show)

data Branch m = Branch
  { terms :: Map NameSegment (Map Referent (m MdValues)),
    types :: Map NameSegment (Map Reference (m MdValues)),
    patches :: Map NameSegment (PatchHash, m Patch),
    children :: Map NameSegment (CausalHead m CausalHash BranchHash (Branch m))
  }

data Patch = Patch
  { termEdits :: Map Referent (Set TermEdit),
    typeEdits :: Map Reference (Set TypeEdit)
  }
