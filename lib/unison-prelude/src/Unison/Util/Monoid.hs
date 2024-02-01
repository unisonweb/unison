module Unison.Util.Monoid
  ( foldMapM,
    ifoldMapM,
    Unison.Util.Monoid.fromMaybe,
    intercalateMap,
    intercalateMapM,
    isEmpty,
    nonEmpty,
    Unison.Util.Monoid.unlessM,
    whenM,
  )
where

import Control.Lens (TraversableWithIndex, ifor)
import Data.List (intersperse)
import Unison.Prelude hiding (whenM)

-- List.intercalate extended to any monoid
-- "The type that intercalate should have had to begin with."
intercalateMap :: (Foldable t, Monoid a) => a -> (b -> a) -> t b -> a
intercalateMap separator renderer elements =
  mconcat $ intersperse separator (renderer <$> toList elements)

intercalateMapM :: (Traversable t, Monad m, Monoid a) => a -> (b -> m a) -> t b -> m a
intercalateMapM separator renderer = traverse renderer >=> return . intercalateMap separator id

fromMaybe :: (Monoid a) => Maybe a -> a
fromMaybe Nothing = mempty
fromMaybe (Just a) = a

whenM, unlessM :: (Monoid a) => Bool -> a -> a
whenM True a = a
whenM False _ = mempty
unlessM = whenM . not

isEmpty, nonEmpty :: (Eq a, Monoid a) => a -> Bool
isEmpty a = a == mempty
nonEmpty = not . isEmpty

foldMapM :: (Monad m, Foldable f, Monoid b) => (a -> m b) -> f a -> m b
foldMapM f as = foldM (\b a -> fmap (b <>) (f a)) mempty as

ifoldMapM :: (Monoid r, Applicative f, Foldable t, TraversableWithIndex i t) => (i -> a -> f r) -> t a -> f r
ifoldMapM f xs = fold <$> ifor xs f
