{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Sqlite.Causal where

import U.Codebase.Sqlite.DbId (BranchHashId, CausalHashId)
import Unison.Prelude

data GDbCausal causalHash valueHash = DbCausal
  { selfHash :: causalHash,
    valueHash :: valueHash,
    parents :: Set causalHash
  }

-- Causal Plan
-- * Load a DbCausal (how do we do this)
-- => new function Queries.localCausalByCausalHashId, can model after loadCausalByCausalHash or factor out of
-- * Add valueHashId as a dependency if unmigrated
-- * Add parent causal hash ids as dependencies if unmigrated
  -- => Queries.loadCausalParents
-- * Map over Branch hash IDs
-- * Inside saveDBCausal (new / factored out of original)
--   * Save as a new self-hash
--    ==> Queries.saveCausal
--   * Map over parent causal hash IDs
--    ==> Queries.saveCausalParents

type DbCausal = GDbCausal CausalHashId BranchHashId

-- causalHashes_ :: Traversal (GDbCausal ch vh) (GDbCausal ch' vh) ch ch'
-- causalHashes_ f DbCausal {..} =
--     DbCausal <$> f selfHash <*> pure valueHash <*> (fmap Set.fromList . traverse f . Set.toList $ parents)

-- valueHashes_ :: Lens (GDbCausal ch vh) (GDbCausal ch vh) vh vh'
-- valueHashes_ f DbCausal{..} =
--     (\p vh -> DbCausal selfHash vh p) parents <$> f valueHash

-- data Causal m hc he e = Causal
--   { causalHash :: hc,
--     valueHash :: he,
--     parents :: Map hc (m (Causal m hc he e)),
--     value :: m e
--   }
