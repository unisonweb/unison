{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Unison.Util.Alphabetical where

import qualified Data.RFC5051 as RFC5051
import Data.Text (Text)

-- Alphabetical ordering used for sorting things to display to humans.
-- Should have 'A' and 'a' both come before 'B' and 'b', etc.
--
-- This need not coincide with the `Ord` instance for a type, which
-- is often an efficient yet arbitrary ordering that's used for
-- stashing the values in maps and sets.
class Eq n => Alphabetical n where
  compareAlphabetical :: n -> n -> Ordering

instance Alphabetical Text where
  compareAlphabetical = RFC5051.compareUnicode

-- newtype whose Ord instance uses alphabetical ordering
newtype OrderAlphabetically a = OrderAlphabetically a deriving (Functor, Traversable, Foldable, Eq)

instance (Eq a, Alphabetical a) => Ord (OrderAlphabetically a) where
  compare (OrderAlphabetically a) (OrderAlphabetically b) = compareAlphabetical a b

instance Alphabetical a => Alphabetical [a] where
  compareAlphabetical a1s a2s = compare (OrderAlphabetically <$> a1s) (OrderAlphabetically <$> a2s)

instance Alphabetical a => Alphabetical (Maybe a) where
  compareAlphabetical a1s a2s = compare (OrderAlphabetically <$> a1s) (OrderAlphabetically <$> a2s)
