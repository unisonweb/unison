{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Causal
  ( Causal (..),
    hoist,
  )
where

import Unison.Prelude

data Causal m hc he e = Causal
  { causalHash :: hc,
    valueHash :: he,
    parents :: Map hc (m (Causal m hc he e)),
    value :: m e
  }
  deriving (Functor)

hoist :: (Functor n) => (forall x. m x -> n x) -> Causal m hc he e -> Causal n hc he e
hoist f (Causal {..}) =
  Causal
    { parents = parents & fmap f & (fmap . fmap) (hoist f),
      value = f value,
      ..
    }
