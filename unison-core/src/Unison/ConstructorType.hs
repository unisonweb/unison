{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DeriveGeneric #-}

module Unison.ConstructorType where

import Unison.Prelude

data ConstructorType = Data | Effect deriving (Eq, Ord, Show, Enum, Generic)
