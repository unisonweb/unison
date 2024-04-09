module Unison.Merge.Synhashed
  ( Synhashed (..),
  )
where

import Unison.Hash (Hash)

-- | A small utility type that represents a syntactic-hashed thing.
--
-- The `Eq` and `Ord` instances only compares syntactic hashes.
data Synhashed a = Synhashed
  { hash :: !Hash,
    value :: !a
  }
  deriving stock (Show)

instance Eq (Synhashed a) where
  Synhashed x _ == Synhashed y _ =
    x == y

instance Ord (Synhashed a) where
  compare (Synhashed x _) (Synhashed y _) =
    compare x y
