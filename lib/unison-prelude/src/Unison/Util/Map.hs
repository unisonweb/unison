-- | @Map@ utilities.
module Unison.Util.Map
  ( bimap,
    bitraverse,
    bitraversed,
    deleteLookup,
    foldMapM,
    unionWithM,
    remap,
    traverseKeys,
    traverseKeysWith,
    swap,
    upsert,
    valuesVector,
  )
where

import Control.Lens hiding (bimap)
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as B
import qualified Data.Bitraversable as B
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Unison.Prelude

bimap :: Ord a' => (a -> a') -> (b -> b') -> Map a b -> Map a' b'
bimap fa fb = Map.fromList . map (B.bimap fa fb) . Map.toList

bitraverse :: (Applicative f, Ord a') => (a -> f a') -> (b -> f b') -> Map a b -> f (Map a' b')
bitraverse fa fb = fmap Map.fromList . traverse (B.bitraverse fa fb) . Map.toList

bitraversed :: (Ord a', Ord k') => Traversal k k' a a' -> Traversal v v' a a' -> Traversal (Map k v) (Map k' v') a a'
bitraversed keyT valT f m =
  bitraverse (keyT f) (valT f) m

-- | 'swap' throws away data if the input contains duplicate values
swap :: Ord b => Map a b -> Map b a
swap =
  Map.foldlWithKey' (\z a b -> Map.insert b a z) mempty

-- | Upsert an element into a map.
upsert :: Ord k => (Maybe v -> v) -> k -> Map k v -> Map k v
upsert f =
  Map.alter (Just . f)

valuesVector :: Map k v -> Vector v
valuesVector =
  Vector.fromList . Map.elems

-- | Like 'Map.delete', but returns the value as well.
deleteLookup :: Ord k => k -> Map k v -> (Maybe v, Map k v)
deleteLookup =
  Map.alterF (,Nothing)

-- | Construct a map from a foldable container by mapping each element to monadic action that returns a key and a value.
--
-- The map is constructed from the left: if two elements map to the same key, the second will overwrite the first.
foldMapM :: (Ord k, Monad m, Foldable t) => (a -> m (k, v)) -> t a -> m (Map k v)
foldMapM f =
  foldlM g Map.empty
  where
    g acc x = do
      (k, v) <- f x
      pure $! Map.insert k v acc

unionWithM ::
  forall m k a.
  (Monad m, Ord k) =>
  (a -> a -> m a) ->
  Map k a ->
  Map k a ->
  m (Map k a)
unionWithM f m1 m2 =
  Monad.foldM go m1 $ Map.toList m2
  where
    go :: Map k a -> (k, a) -> m (Map k a)
    go m1 (k, a2) = case Map.lookup k m1 of
      Just a1 -> do a <- f a1 a2; pure $ Map.insert k a m1
      Nothing -> pure $ Map.insert k a2 m1

-- | Reconstruct a map entirely, given a function from old key/value to new key/value.
--
-- @
-- remap f = Map.fromList . map f . Map.toList
-- @
remap :: Ord k1 => ((k0, v0) -> (k1, v1)) -> Map k0 v0 -> Map k1 v1
remap f =
  Map.fromList . map f . Map.toList

traverseKeys :: (Applicative f, Ord k') => (k -> f k') -> Map k v -> f (Map k' v)
traverseKeys f = bitraverse f pure

traverseKeysWith :: (Applicative f, Ord k') => (v -> v -> v) -> (k -> f k') -> Map k v -> f (Map k' v)
traverseKeysWith combine f m =
  Map.fromListWith combine <$> (Map.toList m & traversed . _1 %%~ f)
