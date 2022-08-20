{-# LANGUAGE DeriveAnyClass #-}

module Unison.Position
  ( Position (..),
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | An indicator of whether something is absolute, e.g. ".foo.bar", or relative, e.g. "foo.bar"
data Position
  = Absolute
  | Relative
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)
