module Unison.Codebase.Branch.Shallow
  ( ShallowBranch (..),
    childAt,
    shallowNames,
    Hash,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Unison.Codebase.Causal.Type as Causal
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Hash as Hash
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference)
import Unison.Referent (Referent)

type Star r n = Metadata.Star r n

type EditHash = Hash.Hash

type Hash = Causal.RawHash ShallowBranch

-- The raw Branch
data ShallowBranch = ShallowBranch
  { terms :: Star Referent NameSegment,
    types :: Star Reference NameSegment,
    children :: Map NameSegment Hash,
    patches :: Map NameSegment EditHash
  }

childAt :: NameSegment -> ShallowBranch -> Maybe Hash
childAt ns (ShallowBranch {children}) = Map.lookup ns children

-- | A 'Names' which only includes mappings for things _directly_ accessible from
-- ShallowBranch.
--
-- I.e. names in nested children are omitted.
shallowNames :: ShallowBranch -> Names
shallowNames = _
