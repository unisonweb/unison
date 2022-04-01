module Unison.Sync.Class (MonadSync (..)) where

import Unison.Sync.Types

-- | A class representing operations we can perform during a sync.
-- Instances can be defined for different sync protocols, or for testing/benchmarking.
class Monad m => MonadSync m where
  getCausalHashByPath :: GetCausalHashByPathRequest -> m GetCausalHashByPathResponse
  updatePath :: UpdatePathRequest -> m UpdatePathResponse
  uploadEntities :: UploadEntitiesRequest -> m UploadEntitiesResponse
  downloadEntities :: DownloadEntitiesRequest -> m DownloadEntitiesResponse
