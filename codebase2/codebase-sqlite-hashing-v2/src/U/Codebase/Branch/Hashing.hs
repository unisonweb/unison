module U.Codebase.Branch.Hashing
  ( hashBranch,
    hashBranchV3,
  )
where

import U.Codebase.Branch (Branch)
import U.Codebase.BranchV3 (BranchV3)
import U.Codebase.HashTags
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as Hashing
import Unison.Hashing.V2.Convert2 (convertBranchV3, v2ToH2Branch)

hashBranch :: forall m. Monad m => Branch m -> m BranchHash
hashBranch branch =
  BranchHash . Hash32.fromHash . Hashing.contentHash <$> v2ToH2Branch branch

-- | Hash a V3 branch.
hashBranchV3 :: BranchV3 m -> BranchHash
hashBranchV3 =
  BranchHash . Hash32.fromHash . Hashing.contentHash . convertBranchV3
