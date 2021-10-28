module Unison.Util.Map
  ( foldMapM,
    unionWithM,
  )
where

import qualified Control.Monad as Monad
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as Map
import Unison.Prelude

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
