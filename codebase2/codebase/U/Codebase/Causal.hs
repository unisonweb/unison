module U.Codebase.Causal where

import Data.Map (Map)

-- | Causal doesn't necessarily pre-load anything other than some hashes.
data Causal m hc he e = Causal
  { causalHash :: hc,
    valueHash :: he,
    parents :: Map hc (m (Causal m hc he e)),
    value :: m (Maybe e)
  }
