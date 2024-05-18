module Unison.Merge.Updated
  ( Updated (..),
  )
where

import Unison.Prelude

-- | An updated thing.
data Updated a = Updated
  { old :: a,
    new :: a
  }
  deriving stock (Functor, Generic, Show)
