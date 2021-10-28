{-# LANGUAGE RecordWildCards #-}
module U.Codebase.Sqlite.Causal where

import Unison.Prelude

data GDbCausal causalHash valueHash =
  DbCausal { selfHash :: causalHash, valueHash :: valueHash, parents :: Set causalHash }

-- type DbCausal = GDbCausal CausalHashId BranchHashId

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
