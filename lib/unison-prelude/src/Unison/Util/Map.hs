-- | @Map@ utilities.
module Unison.Util.Map
  ( bimap,
    bitraverse,
    bitraversed,
    deleteLookup,
    elemsSet,
    foldM,
    foldMapM,
    for_,
    insertLookup,
    mergeMap,
    unionWithM,
    remap,
    traverseKeys,
    traverseKeysWith,
    swap,
    upsert,
    upsertF,
    upsertLookup,
    valuesVector,
  )
where

import Control.Lens hiding (bimap)
import Control.Monad qualified as Monad
import Data.Bifunctor qualified as B
import Data.Bitraversable qualified as B
import Data.Foldable (foldlM)
import Data.Map.Internal qualified as Map (Map (Bin, Tip))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Unison.Prelude hiding (bimap, foldM, for_)

bimap :: (Ord a') => (a -> a') -> (b -> b') -> Map a b -> Map a' b'
bimap fa fb = Map.fromList . map (B.bimap fa fb) . Map.toList

bitraverse :: (Applicative f, Ord a') => (a -> f a') -> (b -> f b') -> Map a b -> f (Map a' b')
bitraverse fa fb = fmap Map.fromList . traverse (B.bitraverse fa fb) . Map.toList

bitraversed :: (Ord a', Ord k') => Traversal k k' a a' -> Traversal v v' a a' -> Traversal (Map k v) (Map k' v') a a'
bitraversed keyT valT f m =
  bitraverse (keyT f) (valT f) m

-- | 'swap' throws away data if the input contains duplicate values
swap :: (Ord b) => Map a b -> Map b a
swap =
  Map.foldlWithKey' (\z a b -> Map.insert b a z) mempty

-- | Like 'Map.insert', but returns the value as well.
insertLookup :: Ord k => k -> v -> Map k v -> (Maybe v, Map k v)
insertLookup k v =
  upsertLookup (const v) k

-- | Upsert an element into a map.
upsert :: (Ord k) => (Maybe v -> v) -> k -> Map k v -> Map k v
upsert f =
  Map.alter (Just . f)

-- | Upsert an element into a map.
upsertF :: (Functor f, Ord k) => (Maybe v -> f v) -> k -> Map k v -> f (Map k v)
upsertF f =
  Map.alterF (fmap Just . f)

-- | Like 'upsert', but returns the value as well.
upsertLookup :: Ord k => (Maybe v -> v) -> k -> Map k v -> (Maybe v, Map k v)
upsertLookup f =
  upsertF (\v -> (v, f v))

valuesVector :: Map k v -> Vector v
valuesVector =
  Vector.fromList . Map.elems

-- | Like 'Map.delete', but returns the value as well.
deleteLookup :: (Ord k) => k -> Map k v -> (Maybe v, Map k v)
deleteLookup =
  Map.alterF (,Nothing)

-- | Like 'Map.elems', but return the values as a set.
elemsSet :: Ord v => Map k v -> Set v
elemsSet =
  Set.fromList . Map.elems

-- | Like 'Map.foldlWithKey'', but with a monadic accumulator.
foldM :: Monad m => (acc -> k -> v -> m acc) -> acc -> Map k v -> m acc
foldM f acc0 =
  go acc0
  where
    go !acc = \case
      Map.Tip -> pure acc
      Map.Bin _ k v xs ys -> do
        acc1 <- go acc xs
        acc2 <- f acc1 k v
        go acc2 ys

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

-- | Run a monadic action for each key/value pair in a map.
for_ :: Monad m => Map k v -> (k -> v -> m ()) -> m ()
for_ m f =
  go m
  where
    go = \case
      Map.Tip -> pure ()
      Map.Bin _ k v xs ys -> do
        go xs
        f k v
        go ys

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
remap :: (Ord k1) => ((k0, v0) -> (k1, v1)) -> Map k0 v0 -> Map k1 v1
remap f =
  Map.fromList . map f . Map.toList

traverseKeys :: (Applicative f, Ord k') => (k -> f k') -> Map k v -> f (Map k' v)
traverseKeys f = bitraverse f pure

traverseKeysWith :: (Applicative f, Ord k') => (v -> v -> v) -> (k -> f k') -> Map k v -> f (Map k' v)
traverseKeysWith combine f m =
  Map.fromListWith combine <$> (Map.toList m & traversed . _1 %%~ f)

-- | @mergeMap@ is like a @foldMap@ version of @merge@: summarize the merging of two maps together as a monoidal value.
mergeMap ::
  forall a b k m.
  (Monoid m, Ord k) =>
  -- | Function to apply when a key exists in the first map, but not the second.
  (k -> a -> m) ->
  -- | Function to apply when a key exists in the second map, but not the first.
  (k -> b -> m) ->
  -- | Function to apply when a key exists in both maps.
  (k -> a -> b -> m) ->
  Map k a ->
  Map k b ->
  m
mergeMap f g h =
  coerce @(Map k a -> Map k b -> Const m (Map k ())) do
    Map.mergeA
      (Map.traverseMissing (coerce f))
      (Map.traverseMissing (coerce g))
      (Map.zipWithAMatched (coerce h))
