module U.Util.Monoid where

import Control.Monad (foldM)
import Control.Monad.Extra ((>=>))
import Data.Foldable (toList)
import Data.List (intersperse)

-- List.intercalate extended to any monoid
-- "The type that intercalate should have had to begin with."
intercalateMap :: (Foldable t, Monoid a) => a -> (b -> a) -> t b -> a
intercalateMap separator renderer elements =
  mconcat $ intersperse separator (renderer <$> toList elements)

intercalateMapM :: (Traversable t, Monad m, Monoid a) => a -> (b -> m a) -> t b -> m a
intercalateMapM separator renderer = traverse renderer >=> return . intercalateMap separator id

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
foldMapM f = foldM (\b a -> fmap (b <>) (f a)) mempty
