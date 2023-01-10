module Unison.Hashing.V2.Kind
  ( Kind (..),
  )
where

import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import Unison.Prelude

data Kind
  = KindStar
  | KindArrow Kind Kind
  deriving (Eq, Ord, Read, Show, Generic)

instance Tokenizable Kind where
  tokens k = case k of
    KindStar -> [Hashable.Tag 0]
    KindArrow k1 k2 -> (Hashable.Tag 1 : Hashable.tokens k1) ++ Hashable.tokens k2
