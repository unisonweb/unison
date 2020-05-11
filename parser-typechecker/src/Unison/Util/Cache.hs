{-# LANGUAGE BlockArguments #-}

module Unison.Util.Cache where

import Unison.Prelude
import UnliftIO (MonadIO, newTVarIO, modifyTVar, atomically, readTVarIO)
import qualified Data.Map as Map

cache :: (MonadIO m, Ord k) => (k -> m v) -> m (k -> m v)
cache f = do
  t <- newTVarIO Map.empty
  pure $ \k -> do
     m <- readTVarIO t
     case Map.lookup k m of
       Nothing -> do
         v <- f k
         atomically $ modifyTVar t (Map.insert k v)
         pure v
       Just v -> pure v

cacheDefined
  :: (MonadIO m, Applicative g, Traversable g, Ord k)
  => (k -> m (g v)) -> m (k -> m (g v))
cacheDefined f = do
  t <- newTVarIO Map.empty
  pure $ \k -> do
     m <- readTVarIO t
     case Map.lookup k m of
       Nothing -> do
         v <- f k
         -- we only populate the cache if f returns `Just`
         for_ v $ \v -> atomically $ modifyTVar t (Map.insert k v)
         pure v
       Just v -> pure (pure v)
