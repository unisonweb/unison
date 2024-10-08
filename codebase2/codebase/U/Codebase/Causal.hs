{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Causal
  ( Causal (..),
  )
where

import Data.Function (on)
import Unison.Prelude

data Causal m hc he pe e = Causal
  { causalHash :: hc,
    valueHash :: he,
    parents :: Map hc (m (Causal m hc he pe pe)),
    value :: m e
  }
  deriving stock (Functor, Generic)

instance (Eq hc) => Eq (Causal m hc he pe e) where
  (==) = (==) `on` causalHash
