module Unison.Merge.ThreeWay
  ( ThreeWay (..),
  )
where

import Unison.Prelude

data ThreeWay a = ThreeWay
  { lca :: !a,
    alice :: !a,
    bob :: !a
  }
  deriving stock (Functor, Generic)

