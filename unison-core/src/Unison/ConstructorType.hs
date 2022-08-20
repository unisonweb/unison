{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Unison.ConstructorType where

import Control.DeepSeq (NFData)
import Unison.Prelude

data ConstructorType = Data | Effect
  deriving stock (Eq, Ord, Show, Enum, Generic)
  deriving anyclass (NFData)
