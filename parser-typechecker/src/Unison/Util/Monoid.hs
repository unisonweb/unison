module Unison.Util.Monoid where

import Data.List (intersperse)
import Data.Foldable (toList)

-- List.intercalate extended to any monoid
-- "The type that intercalate should have had to begin with."
intercalateMap :: (Foldable t, Monoid a) => a -> (b -> a) -> t b -> a
intercalateMap separator renderer elements =
  mconcat $ intersperse separator (renderer <$> toList elements)

fromMaybe :: Monoid a => Maybe a -> a
fromMaybe Nothing = mempty
fromMaybe (Just a) = a

whenM :: Monoid a => Bool -> a -> a
whenM True a = a
whenM False _ = mempty
