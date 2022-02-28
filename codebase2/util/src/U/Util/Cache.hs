{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module U.Util.Cache where

import Prelude hiding (lookup)
import Control.Monad.IO.Class (liftIO)
import UnliftIO (MonadIO, newTVarIO, modifyTVar', writeTVar, atomically, readTVar, readTVarIO)
import qualified Data.Map as Map
import Data.Functor (($>))
import Control.Monad (when)
import Data.Foldable (for_)

data Cache k v =
  Cache { lookup_ :: k -> IO (Maybe v)
        , insert_ :: k -> v -> IO ()
        }

lookup :: MonadIO m => Cache k v -> k -> m (Maybe v)
lookup c k = liftIO (lookup_ c k)

insert :: MonadIO m => Cache k v -> k -> v -> m ()
insert c k v = liftIO (insert_ c k v)

-- Create a cache of unbounded size.
cache :: (MonadIO m, Ord k) => m (Cache k v)
cache = do
  t <- newTVarIO Map.empty
  let
    lookup k = Map.lookup k <$> readTVarIO t
    insert k v = do
      m <- readTVarIO t
      case Map.lookup k m of
        Nothing -> atomically $ modifyTVar' t (Map.insert k v)
        _ -> pure ()

  pure $ Cache lookup insert

nullCache :: Cache k v
nullCache = Cache (const (pure Nothing)) (\_ _ -> pure ())

-- Create a cache of bounded size. Once the cache
-- reaches a size of `maxSize`, older unused entries
-- are evicted from the cache. Unlike LRU caching,
-- where cache hits require updating LRU info,
-- cache hits here are read-only and contention free.
semispaceCache :: (MonadIO m, Ord k) => Word -> m (Cache k v)
semispaceCache 0 = pure nullCache
semispaceCache maxSize = do
  -- Analogous to semispace GC, keep 2 maps: gen0 and gen1
  -- `insert k v` is done in gen0
  --   if full, gen1 = gen0; gen0 = Map.empty
  -- `lookup k` is done in gen0; then gen1
  --   if found in gen0, return immediately
  --   if found in gen1, `insert k v`, then return
  -- Thus, older keys not recently looked up are forgotten
  gen0 <- newTVarIO Map.empty
  gen1 <- newTVarIO Map.empty
  let
    lookup k = readTVarIO gen0 >>= \m0 ->
      case Map.lookup k m0 of
        Nothing -> readTVarIO gen1 >>= \m1 ->
          case Map.lookup k m1 of
            Nothing -> pure Nothing
            Just v -> insert k v $> Just v
        just -> pure just
    insert k v = atomically $ do
      modifyTVar' gen0 (Map.insert k v)
      m0 <- readTVar gen0
      when (fromIntegral (Map.size m0) >= maxSize) $ do
        writeTVar gen1 m0
        writeTVar gen0 Map.empty
  pure $ Cache lookup insert

-- Cached function application: if a key `k` is not in the cache,
-- calls `f` and inserts `f k` results in the cache.
apply :: MonadIO m => Cache k v -> (k -> m v) -> k -> m v
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
applyDefined :: (MonadIO m, Applicative g, Traversable g)
             => Cache k v
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
