{-# LANGUAGE DeriveGeneric #-}

module Unison.Hashing.V2.Kind where

import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import Unison.Prelude

data Kind = Star | Arrow Kind Kind deriving (Eq, Ord, Read, Show, Generic)

instance Tokenizable Kind where
  tokens k = case k of
    Star -> [Hashable.Tag 0]
    Arrow k1 k2 -> (Hashable.Tag 1 : Hashable.tokens k1) ++ Hashable.tokens k2
