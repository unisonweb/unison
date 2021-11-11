{-# LANGUAGE DeriveGeneric #-}

module Unison.ConstructorType where

import Unison.Prelude

data ConstructorType = Data | Effect deriving (Eq, Ord, Show, Enum, Generic)
