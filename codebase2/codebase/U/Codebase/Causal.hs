{-# LANGUAGE RankNTypes, PatternSynonyms #-}
module U.Codebase.Causal where

import Data.Map (Map)

data Causal m hc he e = Causal
  { causalHash :: hc,
    valueHash :: he,
    parents :: Map hc (m (Causal m hc he e)),
    value :: m e
  }


