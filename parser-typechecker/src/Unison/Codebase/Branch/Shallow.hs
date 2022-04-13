{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Branch.Shallow
  ( ShallowBranch (..),
    childAt,
    shallowNames,
    Hash,
    pattern Empty,
  )
where

import Control.Lens (AsEmpty (..), only, pattern Empty)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Unison.Codebase.Causal.Type as Causal
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Hash as Hash
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment)
import Unison.Names (Names (Names))
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Util.Relation as Rel

type Star r n = Metadata.Star r n

type EditHash = Hash.Hash

type Hash = Causal.RawHash ShallowBranch

-- | A shallow branch structure.
data ShallowBranch = ShallowBranch
  { terms :: Star Referent NameSegment,
    types :: Star Reference NameSegment,
    children :: Map NameSegment Hash,
    patches :: Map NameSegment EditHash
  }
  deriving (Show, Eq)

instance AsEmpty ShallowBranch where
  _Empty = only (ShallowBranch mempty mempty mempty mempty)

childAt :: NameSegment -> ShallowBranch -> Maybe Hash
childAt ns (ShallowBranch {children}) = Map.lookup ns children

-- | A 'Names' which only includes mappings for things _directly_ accessible from
-- ShallowBranch.
--
-- I.e. names in nested children are omitted.
shallowNames :: ShallowBranch -> Names
shallowNames b =
  Names
    (Rel.bimap Name.fromSegment id . Rel.swap . Metadata.toRelation $ terms b)
    (Rel.bimap Name.fromSegment id . Rel.swap . Metadata.toRelation $ types b)
