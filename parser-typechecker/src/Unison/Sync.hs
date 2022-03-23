{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Unison.Sync where

import Control.Monad.Error (MonadError (..))
import qualified Data.Text as Text
import Unison.Codebase.Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.Prelude
import Unison.Sync.Types
import qualified UnliftIO

uploadBranch :: MonadSync m => Branch m -> Text -> m ()
uploadBranch branch path = do
  requiredDeps <-
    uploadToRepo (causalUpload branch) >>= \case
      UploadToRepoNeedDeps requiredDeps -> pure requiredDeps
      UploadToRepoFailure -> fail "UploadToRepoFailure"
  _

batchedUpload :: Set Hash -> m (Set Hash)
batchedUpload deps = do
  repo <- undefined
  entities <- undefined deps
  newDeps <-
    uploadToRepo (UploadToRepoRequest repo entities) >>= \case
      UploadToRepoNeedDeps newDeps -> batchedUpload newDeps
      UploadToRepoFailure -> _
      UploadToRepoSuccess -> pure ()

causalUpload :: Branch m -> UploadToRepoRequest
causalUpload = undefined

runParSync :: UnliftIO.Concurrently Sync a -> Sync a
runParSync = UnliftIO.runConcurrently

newtype Sync a = Sync (IO a)
  deriving newtype (Functor, Applicative, Monad)

newtype SyncFailure = SyncFailure Text
  deriving stock (Show)
  deriving anyclass (Exception)

class (MonadFail m) => MonadSync m where
  getCausalHashForPath :: GetCausalHashForPathRequest -> m GetCausalHashForPathResponse
  pushForce :: PushForceRequest -> m PushForceResponse
  uploadToRepo :: UploadToRepoRequest -> m UploadToRepoResponse
  downloadFromRepo :: DownloadFromRepoRequest -> m DownloadFromRepoResponse

-- instance MonadError SyncFailure Sync where
--   throwError = Sync . liftIO . UnliftIO.throwIO
--   catchError m h = undefined

instance MonadFail Sync where
  fail str = Sync $ UnliftIO.throwIO (SyncFailure $ Text.pack str)

instance MonadSync Sync where
  getCausalHashForPath :: GetCausalHashForPathRequest -> Sync GetCausalHashForPathResponse
  getCausalHashForPath = _
  pushForce :: PushForceRequest -> Sync PushForceResponse
  pushForce = _
  uploadToRepo :: UploadToRepoRequest -> Sync UploadToRepoResponse
  uploadToRepo = _
  downloadFromRepo :: DownloadFromRepoRequest -> Sync DownloadFromRepoResponse
  downloadFromRepo = _
