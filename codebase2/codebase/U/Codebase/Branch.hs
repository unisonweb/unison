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
    toNamesMaps,
  )
where

import Control.Lens (AsEmpty (..), ifoldMap, nearly)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
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
    { terms = (fmap . fmap) f terms,
      types = (fmap . fmap) f types,
      patches = (fmap . fmap) f patches,
      children = fmap (fmap (hoist f) . Causal.hoist f) children
    }

hoistCausalBranch :: Functor n => (forall x. m x -> n x) -> CausalBranch m -> CausalBranch n
hoistCausalBranch f cb =
  cb
    & Causal.hoist f
    & fmap (hoist f)

-- | Collects two maps, one with all term names and one with all type names.
-- Note that unlike the `Name` type in `unison-core1`, this list of name segments is in
-- forward order, e.g. `["base", "List", "map"]`
toNamesMaps :: Monad m => CausalBranch m -> m (Map (NonEmpty NameSegment) (Set Referent), Map (NonEmpty NameSegment) (Set Reference))
toNamesMaps cb = do
  b <- Causal.value cb
  let (shallowTermNames, shallowTypeNames) = (Map.keysSet <$> terms b, Map.keysSet <$> types b)
  allChildNames <- for (children b) toNamesMaps
  let (prefixedChildTerms, prefixedChildTypes) =
        flip ifoldMap allChildNames \nameSegment (childTermNames, childTypeNames) ->
          let addSegment = Map.mapKeys (nameSegment NEList.<|)
           in (addSegment childTermNames, addSegment childTypeNames)
  pure (Map.mapKeys (NEList.:| []) shallowTermNames <> prefixedChildTerms, Map.mapKeys (NEList.:| []) shallowTypeNames <> prefixedChildTypes)
