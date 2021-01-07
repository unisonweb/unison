{-# LANGUAGE RankNTypes, PatternSynonyms #-}
module U.Codebase.Causal where

import Data.Map (Map)

-- | Causal doesn't necessarily pre-load anything other than some hashes.
-- data Causal m hc he e = Causal
--   { causalHash :: hc,
--     valueHash :: he,
--     parents :: Map hc (m (Causal m hc he e)),
--     value :: m (Maybe e)
--   }
type Causal m hc he e = CausalHead m hc he e

pattern Causal hc he hp hv = CausalHead hc he hp hv
{-# COMPLETE Causal #-}

data CausalHead m hc he e = CausalHead
  { headCausalHash :: hc,
    headValueHash :: he,
    headParents :: Map hc (m (CausalHead m hc he e)),
    headValue :: m e
  }
