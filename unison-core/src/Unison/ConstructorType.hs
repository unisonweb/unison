{-# LANGUAGE DeriveGeneric #-}

module Unison.ConstructorType where

import Unison.Prelude

data ConstructorType = Data | Effect
  deriving stock (Eq, Ord, Show, Enum, Generic)
  deriving anyclass (NFData)
