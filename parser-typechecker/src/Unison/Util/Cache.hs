{-# LANGUAGE BlockArguments #-}

module Unison.Util.Cache where

import Prelude hiding (lookup)
import Unison.Prelude
import UnliftIO (MonadIO, newTVarIO, modifyTVar, atomically, readTVarIO)
import qualified Data.Map as Map

data Cache m k v =
  Cache { lookup :: k -> m (Maybe v)
        , insert :: k -> v -> m ()
        }

-- Create a cache
cache :: (MonadIO m, Ord k) => m (Cache m k v)
cache = do
  t <- newTVarIO Map.empty
  let
    lookup k = Map.lookup k <$> readTVarIO t
    insert k v = do
      m <- readTVarIO t
      case Map.lookup k m of
        Nothing -> atomically $ modifyTVar t (Map.insert k v)
        _ -> pure ()

  pure $ Cache lookup insert

-- Cached function application: if a key `k` is not in the cache,
-- calls `f` and inserts `f k` results in the cache.
apply :: Monad m => Cache m k v -> (k -> m v) -> k -> m v
apply c f k = lookup c k >>= \case
  Just v -> pure v
  Nothing -> do
    v <- f k
    insert c k v
    pure v

-- Cached function application which only caches values for
-- which `f k` is non-empty. For instance, if `g` is `Maybe`,
-- and `f x` returns `Nothing`, this won't be cached.
--
-- Useful when we think that missing results for `f` may be
-- later filled in so we don't want to cache missing results.
applyDefined :: (Monad m, Applicative g, Traversable g)
             => Cache m k v
             -> (k -> m (g v))
             -> k
             -> m (g v)
applyDefined c f k = lookup c k >>= \case
  Just v -> pure (pure v)
  Nothing -> do
    v <- f k
    -- only populate the cache if f returns a non-empty result
    for_ v $ \v -> insert c k v
    pure v
