module U.Codebase.Causal.Squash (squashCausal) where

import U.Codebase.Branch.Hashing qualified as Branch
import U.Codebase.Branch.Type
import U.Codebase.Causal (Causal (..))
import U.Codebase.Sqlite.Operations qualified as SqliteOps
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

-- Recursively discards history, resulting in a namespace tree with only single a single
-- Causal node at every level.
squashCausal :: CausalBranch Sqlite.Transaction -> Sqlite.Transaction (CausalBranch Sqlite.Transaction)
squashCausal Causal {valueHash = unsquashedBranchHash, value} = do
  runMaybeT (MaybeT (SqliteOps.tryGetSquashResult unsquashedBranchHash) >>= MaybeT . SqliteOps.loadCausalBranchByCausalHash) >>= \case
    Just cb -> pure cb
    Nothing -> do
      branch@Branch {children} <- value
      squashedChildren <- traverse squashCausal children
      let squashedBranchHead = branch {children = squashedChildren}
      squashedBranchHash <- Branch.hashBranch squashedBranchHead
      let squashedCausalHash = hashCausal mempty squashedBranchHash
      let squashedCausalBranch =
            Causal
              { causalHash = squashedCausalHash,
                valueHash = squashedBranchHash,
                parents = mempty,
                value = pure squashedBranchHead
              }
      SqliteOps.saveBranch v2HashHandle squashedCausalBranch
      SqliteOps.saveSquashResult unsquashedBranchHash squashedCausalHash
      pure squashedCausalBranch
