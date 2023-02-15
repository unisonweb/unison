module Unison.Codebase.RootBranchCache
  ( RootBranchCache,
    newEmptyRootBranchCache,
    newEmptyRootBranchCacheIO,
    fetchRootBranch,
    withLock,
  )
where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Unison.Codebase.Branch.Type (Branch)
import qualified Unison.Sqlite as Sqlite
import UnliftIO (MonadUnliftIO, mask, onException)
import UnliftIO.STM
  ( STM,
    TVar,
    atomically,
    newTVar,
    readTVar,
    retrySTM,
    writeTVar,
  )

data RootBranchCacheVal
  = Empty
  | -- | Another thread is updating the cache. If this value is observed
    -- then the reader should wait until the value is Empty or Full. The
    -- api exposed from this module guarantees that a thread cannot exit
    -- and leave the cache in this state.
    ConcurrentModification
  | Full (Branch Sqlite.Transaction)

-- This is isomorphic to @TMVar (Maybe (Branch Sqlite.Transaction))@
newtype RootBranchCache = RootBranchCache (TVar RootBranchCacheVal)

newEmptyRootBranchCacheIO :: (MonadIO m) => m RootBranchCache
newEmptyRootBranchCacheIO = liftIO (coerce $ newTVarIO Empty)

newEmptyRootBranchCache :: STM RootBranchCache
newEmptyRootBranchCache = coerce (newTVar Empty)

readRbc :: RootBranchCache -> STM RootBranchCacheVal
readRbc (RootBranchCache v) = readTVar v

writeRbc :: RootBranchCache -> RootBranchCacheVal -> STM ()
writeRbc (RootBranchCache v) x = writeTVar v x

-- | Read the root branch cache, wait if the cache is currently being
-- updated
readRootBranchCache :: RootBranchCache -> STM (Maybe (Branch Sqlite.Transaction))
readRootBranchCache v =
  readRbc v >>= \case
    Empty -> pure Nothing
    ConcurrentModification -> retrySTM
    Full x -> pure (Just x)

fetchRootBranch :: forall m. (MonadUnliftIO m) => RootBranchCache -> m (Branch Sqlite.Transaction) -> m (Branch Sqlite.Transaction)
fetchRootBranch rbc getFromDb = mask \restore -> do
  join (atomically (fetch restore))
  where
    fetch :: (forall x. m x -> m x) -> STM (m (Branch Sqlite.Transaction))
    fetch restore = do
      readRbc rbc >>= \case
        Empty -> do
          writeRbc rbc ConcurrentModification
          pure do
            rootBranch <- restore getFromDb `onException` atomically (writeRbc rbc Empty)
            atomically (writeRbc rbc (Full rootBranch))
            pure rootBranch
        ConcurrentModification -> retrySTM
        Full x -> pure (pure x)

-- | Take a cache lock so that no other thread can read or write to
-- the cache, perform an action with the cached value, then restore
-- the cache to Empty or Full
withLock ::
  forall m r.
  (MonadUnliftIO m) =>
  RootBranchCache ->
  -- | Perform an action with the cached value
  ( -- restore masking state
    (forall x. m x -> m x) ->
    -- value retrieved from cache
    Maybe (Branch Sqlite.Transaction) ->
    m r
  ) ->
  -- | compute value to restore to the cache
  (r -> Maybe (Branch Sqlite.Transaction)) ->
  m r
withLock v f g = mask \restore -> do
  mbranch <- atomically (takeLock v)
  r <- f restore mbranch `onException` releaseLock mbranch
  releaseLock (g r)
  pure r
  where
    releaseLock :: Maybe (Branch Sqlite.Transaction) -> m ()
    releaseLock mbranch =
      let !val = case mbranch of
            Nothing -> Empty
            Just x -> Full x
       in atomically (writeRbc v val)

takeLock :: RootBranchCache -> STM (Maybe (Branch Sqlite.Transaction))
takeLock v = do
  res <- readRootBranchCache v
  writeRbc v ConcurrentModification
  pure res
