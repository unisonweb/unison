module Unison.ConstructorType
  ( ConstructorType (..),
  )
where

import Control.DeepSeq (NFData)
import Unison.Prelude

data ConstructorType = Data | Effect
  deriving stock (Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)
