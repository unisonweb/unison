module Unison.Util.Map
  ( unionWithM,
  )
where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import Unison.Prelude

unionWithM ::
  forall m k a.
  (Monad m, Ord k) =>
  (a -> a -> m a) ->
  Map k a ->
  Map k a ->
  m (Map k a)
unionWithM f m1 m2 = Monad.foldM go m1 $ Map.toList m2
  where
    go :: Map k a -> (k, a) -> m (Map k a)
    go m1 (k, a2) = case Map.lookup k m1 of
      Just a1 -> do a <- f a1 a2; pure $ Map.insert k a m1
      Nothing -> pure $ Map.insert k a2 m1
