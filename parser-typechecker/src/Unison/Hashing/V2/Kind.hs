{-# LANGUAGE DeriveGeneric #-}

module Unison.Hashing.V2.Kind where

import Unison.Prelude

import Unison.Hashing.V2.BuildHashable (Hashable)
import qualified Unison.Hashing.V2.BuildHashable as Hashable

data Kind = Star | Arrow Kind Kind deriving (Eq,Ord,Read,Show,Generic)

instance Hashable Kind where
  tokens k = case k of
    Star -> [Hashable.Tag 0]
    Arrow k1 k2 -> (Hashable.Tag 1 : Hashable.tokens k1) ++ Hashable.tokens k2
