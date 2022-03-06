module U.Codebase.Branch where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import qualified U.Codebase.Causal as C
import U.Codebase.HashTags (BranchHash, CausalHash, PatchHash)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import U.Codebase.TermEdit (TermEdit)
import U.Codebase.TypeEdit (TypeEdit)

newtype NameSegment = NameSegment {unNameSegment :: Text} deriving (Eq, Ord, Show)

type MetadataType = Reference

type MetadataValue = Reference

data MdValues = MdValues (Map MetadataValue MetadataType) deriving (Eq, Ord, Show)

type Causal m = C.Causal m CausalHash BranchHash (Branch m)

-- | V2.Branch is like V1.Branch0; I would rename it, at least temporarily, but too hard.
data Branch m = Branch
  { terms :: Map NameSegment (Map Referent (m MdValues)),
    types :: Map NameSegment (Map Reference (m MdValues)),
    patches :: Map NameSegment (PatchHash, m Patch),
    children :: Map NameSegment (Causal m)
  }

data Patch = Patch
  { termEdits :: Map Referent (Set TermEdit),
    typeEdits :: Map Reference (Set TypeEdit)
  }

instance Show (Branch m) where
  show b =
    "Branch { terms = " ++ show (fmap Map.keys (terms b))
      ++ ", types = "
      ++ show (fmap Map.keys (types b))
      ++ ", patches = "
      ++ show (fmap fst (patches b))
      ++ ", children = "
      ++ show (Map.keys (children b))
