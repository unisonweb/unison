-- | V3 branches and causals.
module U.Codebase.BranchV3
  ( BranchV3 (..),
    CausalBranchV3,
  )
where

import U.Codebase.Branch.Type (Branch)
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
  { children :: !(Map NameSegment (CausalBranchV3 m)),
    decls :: !(Map NameSegment TypeReference),
    terms :: !(Map NameSegment Referent)
  }
  deriving stock (Generic)

-- | A V3 branch's history has V3 branches everywhere at the latest causal (so, no metadata, no patches, etc. in any
-- children namespaces), but when we go back in history, we find V2 branches, because that's what we used to have ;)
type CausalBranchV3 m = Causal m CausalHash BranchHash (Branch m) (BranchV3 m)
