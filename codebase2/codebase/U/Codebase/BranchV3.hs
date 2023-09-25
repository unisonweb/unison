-- | V3 branches.
module U.Codebase.BranchV3
  ( BranchV3 (..),
    CausalBranchV3,
  )
where

import U.Codebase.Causal (Causal)
import U.Codebase.HashTags (BranchHash, CausalHash)
import U.Codebase.Reference (TypeReference)
import U.Codebase.Referent (Referent)
import Unison.NameSegment (NameSegment)
import Unison.Prelude

-- | A V3 branch is a trimmed-down V2 branch:
--
--   * Names can't be conflicted.
--   * Metadata doesn't exist.
--   * Patches don't exist.
data BranchV3 m = BranchV3
  { terms :: Map NameSegment Referent,
    types :: Map NameSegment TypeReference,
    children :: Map NameSegment (CausalBranchV3 m)
  }

type CausalBranchV3 m =
  Causal m CausalHash BranchHash (BranchV3 m)
