module U.Codebase.Causal.Squash (squashCausal) where

import U.Codebase.Branch.Type
import U.Codebase.Causal (Causal (..))
import U.Codebase.Sqlite.HashHandle qualified as HH
import U.Codebase.Sqlite.Operations qualified as SqliteOps
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

-- Recursively discards history, resulting in a namespace tree with only single a single
-- Causal node at every level.
squashCausal :: HH.HashHandle -> CausalBranch Sqlite.Transaction -> Sqlite.Transaction (CausalBranch Sqlite.Transaction)
squashCausal hashHandle@HH.HashHandle {hashCausal, hashBranch} Causal {valueHash = unsquashedBranchHash, value} = do
  runMaybeT (MaybeT (SqliteOps.tryGetSquashResult unsquashedBranchHash) >>= MaybeT . SqliteOps.loadCausalBranchByCausalHash) >>= \case
    Just cb -> pure cb
    Nothing -> do
      branch@Branch {children} <- value
      squashedChildren <- traverse (squashCausal hashHandle) children
      let squashedBranchHead = branch {children = squashedChildren}
      squashedBranchHash <- hashBranch squashedBranchHead
      let squashedCausalHash = hashCausal squashedBranchHash mempty
      let squashedCausalBranch =
            Causal
              { causalHash = squashedCausalHash,
                valueHash = squashedBranchHash,
                parents = mempty,
                value = pure squashedBranchHead
              }
      SqliteOps.saveBranch hashHandle squashedCausalBranch
      SqliteOps.saveSquashResult unsquashedBranchHash squashedCausalHash
      pure squashedCausalBranch
