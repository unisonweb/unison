module Unison.Hashing.V2.NameSegment
  ( NameSegment (..),
  )
where

import qualified Unison.Hashing.V2.Tokenizable as H
import Unison.Prelude

-- | A name segment.
newtype NameSegment
  = NameSegment Text
  deriving stock (Eq, Ord, Show)

instance H.Tokenizable NameSegment where
  tokens (NameSegment t) = [H.Text t]
