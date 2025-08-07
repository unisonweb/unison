module Unison.Merge.Updated
  ( Updated,
    GUpdated (..),
    bimap,
    bitraverse,
    Unison.Merge.Updated.foldMap,
    fromPair,
    Unison.Merge.Updated.map,
    sequenceDefns,
    Unison.Merge.Updated.traverse,
    Unison.Merge.Updated.unzip,
    unzipWith,
    Unison.Merge.Updated.zipWith,
  )
where

import Unison.Prelude hiding (bimap)
import Unison.Util.Defns (Defns (..))

type Updated a =
  GUpdated a a

-- | An updated thing.
data GUpdated a b = Updated
  { old :: a,
    new :: b
  }
  deriving stock (Generic, Show)

bimap :: (a -> b) -> (c -> d) -> GUpdated a c -> GUpdated b d
bimap f g (Updated x y) =
  Updated (f x) (g y)

bitraverse :: (Applicative f) => (a -> f b) -> (c -> f d) -> GUpdated a c -> f (GUpdated b d)
bitraverse f g (Updated x y) =
  Updated <$> f x <*> g y

foldMap :: (Semigroup m) => (a -> m) -> Updated a -> m
foldMap f (Updated x y) =
  f x <> f y

fromPair :: (a, b) -> GUpdated a b
fromPair (x, y) =
  Updated x y

map :: (a -> b) -> Updated a -> Updated b
map f =
  bimap f f

sequenceDefns :: Updated (Defns terms types) -> Defns (Updated terms) (Updated types)
sequenceDefns (Updated (Defns oldTerms oldTypes) (Defns newTerms newTypes)) =
  Defns (Updated oldTerms newTerms) (Updated oldTypes newTypes)

traverse :: (Applicative f) => (a -> f b) -> Updated a -> f (Updated b)
traverse f =
  bitraverse f f

unzip :: GUpdated (a, b) (c, d) -> (GUpdated a c, GUpdated b d)
unzip (Updated (a, b) (c, d)) =
  (Updated a c, Updated b d)

unzipWith :: (a -> (b, c)) -> Updated a -> (Updated b, Updated c)
unzipWith f (Updated x y) =
  let (x1, x2) = f x
      (y1, y2) = f y
   in (Updated x1 y1, Updated x2 y2)

zipWith :: (a -> b -> c) -> Updated a -> Updated b -> Updated c
zipWith f (Updated a b) (Updated c d) =
  Updated (f a c) (f b d)
