{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Branch
  ( Branch (..),
    CausalBranch,
    Patch (..),
    MetadataType,
    MetadataValue,
    MdValues (..),
    NameSegment (..),
    CausalHash,
    NamespaceStats (..),
    namespaceStatistics,
    nonEmptyChildren,
    childAt,
    hoist,
    hoistCausalBranch,
  )
where

import Control.Lens hiding (children)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import U.Codebase.Causal (Causal)
import qualified U.Codebase.Causal as Causal
import U.Codebase.HashTags (BranchHash, CausalHash, PatchHash)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import U.Codebase.TermEdit (TermEdit)
import U.Codebase.TypeEdit (TypeEdit)
import Unison.Prelude

newtype NameSegment = NameSegment {unNameSegment :: Text} deriving (Eq, Ord, Show)

type MetadataType = Reference

type MetadataValue = Reference

data MdValues = MdValues (Map MetadataValue MetadataType) deriving (Eq, Ord, Show)

type CausalBranch m = Causal m CausalHash BranchHash (Branch m)

-- | A re-imagining of Unison.Codebase.Branch which is less eager in what it loads,
-- which can often speed up load times and keep fewer things in memory.
data Branch m = Branch
  { terms :: Map NameSegment (Map Referent (m MdValues)),
    types :: Map NameSegment (Map Reference (m MdValues)),
    patches :: Map NameSegment (PatchHash, m Patch),
    children :: Map NameSegment (CausalBranch m, NamespaceStats)
  }

instance AsEmpty (Branch m) where
  _Empty =
    nearly
      (Branch mempty mempty mempty mempty)
      (\(Branch terms types patches children) -> null terms && null types && null patches && null children)

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

nonEmptyChildren :: Branch m -> Map NameSegment (CausalBranch m)
nonEmptyChildren Branch {children} =
  children & mapMaybe \(cb, ns) ->
    if nonZeroStats ns
      then Just cb
      else Nothing
  where
    nonZeroStats (NamespaceStats numContainedTerms numContainedTypes _numContainedPatches) =
      numContainedTerms + numContainedTypes > 0

-- | Useful statistics about a namespace.
-- All contained statistics should be 'static', i.e. they can be computed when a branch is
-- first saved, and won't change unless the branch hash also changes.
data NamespaceStats = NamespaceStats
  { numContainedTerms :: Int,
    numContainedTypes :: Int,
    numContainedPatches :: Int
  }
  deriving (Show)

-- | Compute statistics from a branch by summarizing the branch contents and the statistics of
-- its children.
namespaceStatistics :: Branch m -> NamespaceStats
namespaceStatistics Branch {terms, types, patches, children} =
  NamespaceStats
    { numContainedTerms =
        let childTermCount = sumOf (folded . _2 . to numContainedTerms) children
            termCount = lengthOf (folded . folded) terms
         in childTermCount + termCount,
      numContainedTypes =
        let childTypeCount = sumOf (folded . _2 . to numContainedTypes) children
            typeCount = lengthOf (folded . folded) types
         in childTypeCount + typeCount,
      numContainedPatches =
        let childPatchCount = sumOf (folded . _2 . to numContainedPatches) children
            patchCount = Map.size patches
         in childPatchCount + patchCount
    }

childAt :: NameSegment -> Branch m -> Maybe (CausalBranch m)
childAt ns (Branch {children}) = fst <$> Map.lookup ns children

hoist :: Functor n => (forall x. m x -> n x) -> Branch m -> Branch n
hoist f Branch {..} =
  Branch
    { terms = (fmap . fmap) f terms,
      types = (fmap . fmap) f types,
      patches = (fmap . fmap) f patches,
      children =
        fmap (first (fmap (hoist f) . Causal.hoist f)) children
    }

hoistCausalBranch :: Functor n => (forall x. m x -> n x) -> CausalBranch m -> CausalBranch n
hoistCausalBranch f cb =
  cb
    & Causal.hoist f
    & fmap (hoist f)
