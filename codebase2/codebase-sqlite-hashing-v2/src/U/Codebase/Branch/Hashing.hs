module U.Codebase.Branch.Hashing (hashBranch) where

import U.Codebase.Branch (Branch)
import U.Codebase.HashTags
import Unison.Hashing.V2 qualified as Hashing
import Unison.Hashing.V2.Convert2 (v2ToH2Branch)

hashBranch :: forall m. Monad m => Branch m -> m BranchHash
hashBranch branch =
  BranchHash . Hashing.contentHash <$> v2ToH2Branch branch
