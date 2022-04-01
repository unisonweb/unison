module Unison.Sync.Class where

class Monad m => MonadSync m where
  getCausalHashForPath :: GetCausalHashByPathRequest -> m GetCausalHashByPathResponse
  pushForce :: PushRequest -> m PushResponse
  uploadToRepo :: UploadEntitiesRequest -> m (Maybe (NeedDependencies Sync.Hash))
  downloadEntities :: DownloadEntitiesRequest -> m DownloadEntitiesResponse
