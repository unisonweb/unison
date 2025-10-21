module Unison.Typed
  ( Typed (..),
  )
where

import Unison.Type (Type)

-- | A utility type that represents a thing along with its Unison type.
data Typed a v ann = Typed
  { thing :: !a,
    type_ :: !(Type v ann)
  }
  deriving stock (Eq, Show)
