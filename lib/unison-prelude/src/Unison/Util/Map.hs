-- | @Map@ utilities.
module Unison.Util.Map
  ( bimap,
    bitraverse,
    deleteLookup,
    foldMapM,
    unionWithM,
  )
where

import qualified Control.Monad as Monad
import qualified Data.Bifunctor as B
import qualified Data.Bitraversable as B
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as Map
import Unison.Prelude

bimap :: Ord a' => (a -> a') -> (b -> b') -> Map a b -> Map a' b'
bimap fa fb = Map.fromList . map (B.bimap fa fb) . Map.toList

bitraverse :: (Applicative f, Ord a') => (a -> f a') -> (b -> f b') -> Map a b -> f (Map a' b')
bitraverse fa fb = fmap Map.fromList . traverse (B.bitraverse fa fb) . Map.toList

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
