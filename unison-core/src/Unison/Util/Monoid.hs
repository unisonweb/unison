{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Util.Monoid where

import Unison.Prelude hiding (whenM)

import Data.List (intersperse)

-- List.intercalate extended to any monoid
-- "The type that intercalate should have had to begin with."
intercalateMap :: (Foldable t, Monoid a) => a -> (b -> a) -> t b -> a
intercalateMap separator renderer elements =
  mconcat $ intersperse separator (renderer <$> toList elements)

fromMaybe :: Monoid a => Maybe a -> a
fromMaybe Nothing = mempty
fromMaybe (Just a) = a

whenM, unlessM :: Monoid a => Bool -> a -> a
whenM True a = a
whenM False _ = mempty
unlessM = whenM . not

isEmpty, nonEmpty :: (Eq a, Monoid a) => a -> Bool
isEmpty a = a == mempty
nonEmpty = not . isEmpty

foldMapM :: (Monad m, Foldable f, Monoid b) => (a -> m b) -> f a -> m b
foldMapM f as = foldM (\b a -> fmap (b <>) (f a)) mempty as
