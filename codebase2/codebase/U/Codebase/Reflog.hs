{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Reflog where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Time (UTCTime)

data Entry causal text = Entry
  { time :: UTCTime,
    rootCausalHash :: causal,
    reason :: text
  }

instance Bifunctor Entry where
  bimap = bimapDefault

instance Bifoldable Entry where
  bifoldMap = bifoldMapDefault

instance Bitraversable Entry where
  bitraverse f g (Entry time rch reason) =
    Entry time <$> f rch <*> g reason
