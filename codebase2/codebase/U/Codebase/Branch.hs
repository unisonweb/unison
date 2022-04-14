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
    childAt,
    hoist,
    hoistCausalBranch,
  )
where

import Control.Lens hiding (children)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import qualified U.Codebase.Causal as C
import qualified U.Codebase.Causal as Causal
import U.Codebase.HashTags (BranchHash, CausalHash, PatchHash)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import U.Codebase.TermEdit (TermEdit)
import U.Codebase.TypeEdit (TypeEdit)

newtype NameSegment = NameSegment {unNameSegment :: Text} deriving (Eq, Ord, Show)

type MetadataType = Reference

type MetadataValue = Reference

data MdValues = MdValues (Map MetadataValue MetadataType) deriving (Eq, Ord, Show)

type CausalBranch m = C.Causal m CausalHash BranchHash (Branch m)

-- | V2.Branch is like V1.Branch0; I would rename it, at least temporarily, but too hard.
data Branch m = Branch
  { terms :: Map NameSegment (Map Referent (m MdValues)),
    types :: Map NameSegment (Map Reference (m MdValues)),
    patches :: Map NameSegment (PatchHash, m Patch),
    children :: Map NameSegment (CausalBranch m)
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

childAt :: NameSegment -> Branch m -> Maybe (CausalBranch m)
childAt ns (Branch {children}) = Map.lookup ns children

hoist :: Functor n => (forall x. m x -> n x) -> Branch m -> Branch n
hoist f Branch {..} =
  Branch
    { terms = terms & traversed . traversed %~ f,
      types = types & traversed . traversed %~ f,
      patches = patches & traversed . _2 %~ f,
      children = children & traversed %~ fmap (hoist f) . Causal.hoist f
    }

hoistCausalBranch :: Functor n => (forall x. m x -> n x) -> CausalBranch m -> CausalBranch n
hoistCausalBranch f cb =
  cb
    & Causal.hoist f
    & fmap (hoist f)
