{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE RankNTypes, PatternSynonyms #-}
module U.Codebase.Causal where

import Data.Map (Map)

data Causal m hc he e = Causal
  { causalHash :: hc,
    valueHash :: he,
    parents :: Map hc (m (Causal m hc he e)),
    value :: m e
  }


