module Unison.Util.InternCache
  ( InternCache,
    newInternCache,
    lookupCached,
    insertCached,
    hoistInternCache,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import System.Mem.Weak
import UnliftIO.STM

data InternCache m k v = InternCache
  { lookupCached :: k -> m (Maybe v),
    insertCached :: k -> v -> m ()
  }

-- | Creates a 'BranchCache' which uses weak references to only keep branches in the cache for
-- as long as they're reachable by something else in the app.
--
-- This means you don't need to worry about a Branch not being GC'd because it's in the cache.
newInternCache :: forall m k v. (MonadIO m, Hashable k) => m (InternCache m k v)
newInternCache = do
  var <- newTVarIO mempty
  pure $
    InternCache
      { lookupCached = lookupCached' var,
        insertCached = insertCached' var
      }
  where
    lookupCached' :: TVar (HashMap k (Weak v)) -> k -> m (Maybe v)
    lookupCached' var ch = liftIO $ do
      cache <- readTVarIO var
      case HashMap.lookup ch cache of
        Nothing -> pure Nothing
        Just weakRef -> deRefWeak weakRef

    insertCached' :: TVar (HashMap k (Weak v)) -> k -> v -> m ()
    insertCached' var k v = liftIO $ do
      -- It's worth reading the semantics of these operations.
      -- We may in the future wish to instead keep the value alive for as long as the
      -- key is alive, this is easy to do with 'mkWeak', but we'll start with only
      -- keeping the value alive as long as it's directly referenced.
      wk <- mkWeakPtr v (Just $ removeDeadVal var k)
      atomically $ modifyTVar' var (HashMap.insert k wk)

    -- Use this as a finalizer to remove the key from the map when its value gets GC'd
    removeDeadVal :: TVar (HashMap k (Weak v)) -> k -> IO ()
    removeDeadVal var k = liftIO do
      atomically $ modifyTVar' var (HashMap.delete k)

hoistInternCache :: (forall x. m x -> n x) -> InternCache m k v -> InternCache n k v
hoistInternCache f (InternCache lookup' insert') =
  InternCache
    { lookupCached = f . lookup',
      insertCached = \k v -> f $ insert' k v
    }
