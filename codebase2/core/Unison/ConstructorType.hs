module Unison.ConstructorType
  ( ConstructorType (..),
  )
where

import Unison.Prelude

-- | Whether a data declaration (or constructor of that declaration) is @data@ or @effect@.
data ConstructorType
  = Data
  | Effect
  deriving (Eq, Ord, Show, Generic)
