{-# LANGUAGE BlockArguments #-}

module Unison.Util.Cache where

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
