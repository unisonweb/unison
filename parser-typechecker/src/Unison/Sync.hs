{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Unison.Sync where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as Text
import Unison.Codebase.Type
import Unison.Prelude
import Unison.Sync.Types
import qualified Unison.Sync.Types as Sync
import Unison.Util.Monoid (foldMapM)
import qualified UnliftIO

-- Number of entities to fetch in a single request.
batchSize :: Int
batchSize = 100

-- Codebase action we may need.
missingHashes :: Codebase m v a -> (NESet HashJWT) -> m (Maybe (NESet HashJWT))
missingHashes = undefined

unpackHashJWT :: HashJWT -> TypedHash
unpackHashJWT = undefined

entityHash :: Sync.Hash -> Entity h oh t -> TypedHash
entityHash = undefined

saveAll :: MonadSync m => Codebase m v a -> Map Sync.Hash (Entity HashJWT Sync.Hash Text) -> m (Maybe (NESet HashJWT))
saveAll codebase entities =
  flip foldMapM (Map.toList entities) \(hash, entity) -> do
    tryToSave codebase hash entity

tryToSave :: Codebase m v a -> Sync.Hash -> Entity HashJWT Sync.Hash Text -> m (Maybe (NESet HashJWT))
tryToSave _codebase = undefined

-- uploadBranch :: MonadSync m => Branch m -> Text -> m ()
-- uploadBranch branch path = do
--   requiredDeps <-
--     uploadToRepo (causalUpload branch) >>= \case
--       UploadToRepoNeedDeps requiredDeps -> pure requiredDeps
--       UploadToRepoFailure -> fail "UploadToRepoFailure"
--   _

-- batchedUpload :: Codebase m v a -> Set Hash -> m (Set Hash)
-- batchedUpload codebase deps = do
--   repo <- undefined
--   entities <- undefined deps
--   newDeps <-
--     uploadToRepo (UploadToRepoRequest repo entities) >>= \case
--       UploadToRepoNeedDeps newDeps -> batchedUpload newDeps
--       UploadToRepoFailure -> _
--       UploadToRepoSuccess -> pure ()

downloadFromPath :: MonadSync m => RepoPath -> Codebase m v a -> m TypedHash
downloadFromPath repoPath@(RepoPath {repoName}) codebase = do
  GetCausalHashByPathResponse chJWT <- getCausalHashForPath (GetCausalHashByPathRequest repoPath)
  missingCH <- missingHashes codebase (NESet.singleton chJWT)
  for_ missingCH \chJWTs -> do
    downloadEntitiesRecursively codebase repoName chJWTs
  pure $ unpackHashJWT chJWT

-- DownloadEntitiesResponse {entities} <- downloadEntities (DownloadEntitiesRequest {repoName, hashes = chJWTs})
-- missingDeps <- saveNewEntities codebase entities
-- _

-- | Download in batches until we've saved all entities and dependencies from the given set.
downloadEntitiesRecursively :: forall m v a. MonadSync m => Codebase m v a -> RepoName -> NESet HashJWT -> m ()
downloadEntitiesRecursively codebase repoName hashJWTs = do
  void $ runStateT helper (NESet.toSet hashJWTs)
  where
    helper :: StateT (Set HashJWT) m ()
    helper = do
      (nextBatch, remaining) <- gets (Set.splitAt batchSize)
      put remaining
      case NESet.nonEmptySet nextBatch of
        Nothing -> pure ()
        Just hashJWTs -> do
          DownloadEntitiesResponse {entities} <- lift $ downloadEntities (DownloadEntitiesRequest {repoName, hashes = hashJWTs})
          lift (saveAll codebase entities) >>= \case
            Nothing -> pure ()
            Just missingDeps -> modify (<> NESet.toSet missingDeps)
          helper

runParSync :: UnliftIO.Concurrently Sync a -> Sync a
runParSync = UnliftIO.runConcurrently

newtype Sync a = Sync (IO a)
  deriving newtype (Functor, Applicative, Monad)

newtype SyncFailure = SyncFailure Text
  deriving stock (Show)
  deriving anyclass (Exception)

class (MonadFail m) => MonadSync m where
  getCausalHashForPath :: GetCausalHashByPathRequest -> m GetCausalHashByPathResponse
  pushForce :: PushRequest -> m PushResponse
  uploadToRepo :: UploadEntitiesRequest -> m (Maybe (NeedDependencies Sync.Hash))
  downloadEntities :: DownloadEntitiesRequest -> m DownloadEntitiesResponse

-- instance MonadError SyncFailure Sync where
--   throwError = Sync . liftIO . UnliftIO.throwIO
--   catchError m h = undefined

instance MonadFail Sync where
  fail str = Sync $ UnliftIO.throwIO (SyncFailure $ Text.pack str)

-- instance MonadSync Sync where
--   getCausalHashForPath = _
--   pushForce = _
--   uploadToRepo = _
--   downloadEntities = _
