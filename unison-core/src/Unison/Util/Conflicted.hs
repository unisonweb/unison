module Unison.Util.Conflicted
  ( Conflicted (..),
  )
where

import Data.Set.NonEmpty (NESet)

-- | A conflicted thing.
data Conflicted n a
  = Conflicted !n !(NESet a)
