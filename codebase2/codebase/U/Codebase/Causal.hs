{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Causal
  ( Causal (..),
    emap,
    hoist,
  )
where

import Data.Map.Strict qualified as Map
import Unison.Prelude

data Causal m hc he pe e = Causal
  { causalHash :: hc,
    valueHash :: he,
    parents :: Map hc (m (Causal m hc he pe pe)),
    value :: m e
  }
  deriving stock (Functor, Generic)

-- | @emap f g@ maps over the values and parents' values with @f@ and @g@.
emap :: Functor m => (e -> e') -> (pe -> pe') -> Causal m hc he pe e -> Causal m hc he pe' e'
emap f g causal@Causal {parents, value} =
  causal
    { parents = Map.map (fmap (emap g g)) parents,
      value = f <$> value
    }

hoist :: (Functor n) => (forall x. m x -> n x) -> Causal m hc he pe e -> Causal n hc he pe e
hoist f (Causal {..}) =
  Causal
    { parents = parents & fmap f & (fmap . fmap) (hoist f),
      value = f value,
      ..
    }
