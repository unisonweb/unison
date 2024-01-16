module U.Codebase.Branch.Type
  ( Branch (..),
    CausalBranch,
    Patch (..),
    MetadataType,
    MetadataValue,
    MdValues (..),
    NamespaceStats (..),
    hasDefinitions,
    childAt,
    hoist,
    hoistCausalBranch,
    U.Codebase.Branch.Type.empty,
  )
where

import Data.Map.Strict qualified as Map
import U.Codebase.Causal (Causal)
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash, CausalHash, PatchHash)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import U.Codebase.TermEdit (TermEdit)
import U.Codebase.TypeEdit (TypeEdit)
import Unison.NameSegment (NameSegment)
import Unison.Prelude

type MetadataType = Reference

type MetadataValue = Reference

newtype MdValues = MdValues {unMdValues :: Map MetadataValue MetadataType} deriving (Eq, Ord, Show)

type CausalBranch m = Causal m CausalHash BranchHash (Branch m) (Branch m)

-- | A re-imagining of Unison.Codebase.Branch which is less eager in what it loads,
-- which can often speed up load times and keep fewer things in memory.
data Branch m = Branch
  { terms :: !(Map NameSegment (Map Referent (m MdValues))),
    types :: !(Map NameSegment (Map Reference (m MdValues))),
    patches :: !(Map NameSegment (PatchHash, m Patch)),
    children :: !(Map NameSegment (CausalBranch m))
  }
  deriving stock (Generic)

empty :: Branch m
empty = Branch mempty mempty mempty mempty

data Patch = Patch
  { termEdits :: !(Map Referent (Set TermEdit)),
    typeEdits :: !(Map Reference (Set TypeEdit))
  }

instance Show (Branch m) where
  show b =
    "Branch { terms = "
      ++ show (fmap Map.keys (terms b))
      ++ ", types = "
      ++ show (fmap Map.keys (types b))
      ++ ", patches = "
      ++ show (fmap fst (patches b))
      ++ ", children = "
      ++ show (Map.keys (children b))

-- | Useful statistics about a namespace.
-- All contained statistics should be 'static', i.e. they can be computed when a branch is
-- first saved, and won't change unless the branch hash also changes.
data NamespaceStats = NamespaceStats
  { numContainedTerms :: !Int,
    numContainedTypes :: !Int,
    numContainedPatches :: !Int
  }
  deriving (Show, Eq, Ord)

instance Semigroup NamespaceStats where
  NamespaceStats a1 b1 c1 <> NamespaceStats a2 b2 c2 =
    NamespaceStats (a1 + a2) (b1 + b2) (c1 + c2)

instance Monoid NamespaceStats where
  mempty = NamespaceStats 0 0 0

-- | Whether the provided stats indicate the presence of any definitions in the namespace.
hasDefinitions :: NamespaceStats -> Bool
hasDefinitions (NamespaceStats numTerms numTypes _numPatches) =
  numTerms + numTypes > 0

childAt :: NameSegment -> Branch m -> Maybe (CausalBranch m)
childAt ns (Branch {children}) = Map.lookup ns children

hoist :: (Functor n) => (forall x. m x -> n x) -> Branch m -> Branch n
hoist f Branch {children, patches, terms, types} =
  Branch
    { terms = (fmap . fmap) f terms,
      types = (fmap . fmap) f types,
      patches = (fmap . fmap) f patches,
      children = fmap (hoistCausalBranch f) children
    }

hoistCausalBranch :: (Functor n) => (forall x. m x -> n x) -> CausalBranch m -> CausalBranch n
hoistCausalBranch f cb =
  cb
    & Causal.hoist f
    & Causal.emap (hoist f) (hoist f)
