module Unison.Merge.TwoOrThreeWay
  ( TwoOrThreeWay (..),
  )
where

import Unison.Prelude

data TwoOrThreeWay a = TwoOrThreeWay
  { lca :: Maybe a,
    alice :: a,
    bob :: a
  }
  deriving stock (Functor, Generic)
