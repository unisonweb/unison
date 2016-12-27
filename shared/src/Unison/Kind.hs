{-# LANGUAGE DeriveGeneric #-}

module Unison.Kind where

import GHC.Generics
import Data.Aeson
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as Hashable

data Kind = Star | Constraint | Arrow Kind Kind deriving (Eq,Ord,Read,Show,Generic)

instance Hashable Kind where
  tokens k = case k of
    Star -> [Hashable.Tag 0]
    Constraint -> [Hashable.Tag 1]
    Arrow k1 k2 -> (Hashable.Tag 2 : Hashable.tokens k1) ++ Hashable.tokens k2

instance ToJSON Kind
instance FromJSON Kind
