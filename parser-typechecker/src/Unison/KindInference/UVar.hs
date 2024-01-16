{-# LANGUAGE DataKinds #-}

module Unison.KindInference.UVar
  ( UVar (..),
  )
where

import Unison.Symbol
import Unison.Type qualified as T

data UVar v loc = UVar
  { _uvarSymbol :: Symbol,
    uvarType :: T.Type v loc
  }
  deriving stock (Eq, Ord, Show)
