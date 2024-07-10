module Unison.Codebase.SqliteCodebase.Branch.Cache where

import Control.DeepSeq qualified as DeepSeq
import Data.Map qualified as Map
import System.Mem.Weak
import U.Codebase.HashTags qualified as V2
import Unison.Codebase.Branch qualified as V1.Branch
import Unison.Codebase.Branch.Type qualified as Branch
import Unison.Debug qualified as Debug
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import UnliftIO qualified
import UnliftIO.STM

-- | A cache of 'V1.Branch.Branch' by 'V2.CausalHash'es.
data BranchCache m = BranchCache
  { lookupCachedBranch :: V2.CausalHash -> m (Maybe (V1.Branch.Branch m)),
    insertCachedBranch :: V2.CausalHash -> V1.Branch.Branch m -> m ()
  }

-- | Creates a 'BranchCache' which uses weak references to only keep branches in the cache for
-- as long as they're reachable by something else in the app.
--
-- This means you don't need to worry about a Branch not being GC'd because it's in the cache.
newBranchCache :: forall m. (MonadIO m) => m (BranchCache Sqlite.Transaction)
newBranchCache = do
  var <- newTVarIO mempty
  pure $
    BranchCache
      { lookupCachedBranch = lookupCachedBranch' var,
        insertCachedBranch = insertCachedBranch' var
      }
  where
    lookupCachedBranch' :: TVar (Map V2.CausalHash (Weak (V1.Branch.Branch Sqlite.Transaction))) -> V2.CausalHash -> Sqlite.Transaction (Maybe (V1.Branch.Branch Sqlite.Transaction))
    lookupCachedBranch' var ch = Sqlite.unsafeIO do
      cache <- readTVarIO var
      case Map.lookup ch cache of
        Nothing -> pure Nothing
        Just weakRef -> do
          -- Debug.debugLogM Debug.Temp $ "Cache Hit" <> show ch
          deRefWeak weakRef

    insertCachedBranch' :: TVar (Map V2.CausalHash (Weak (V1.Branch.Branch Sqlite.Transaction))) -> V2.CausalHash -> (V1.Branch.Branch Sqlite.Transaction) -> Sqlite.Transaction ()
    insertCachedBranch' var ch b = Sqlite.unsafeIO do
      _ <- UnliftIO.evaluate $ DeepSeq.force (Branch.names . Branch.head $ b)
      -- It's worth reading the semantics of these operations.
      -- We may in the future wish to instead keep the branch object alive for as long as the
      -- CausalHash is alive, this is easy to do with 'mkWeak', but we'll start with only
      -- keeping the branch alive as long as it's directly referenced.
      wk <- mkWeakPtr b (Just $ removeDeadVal var ch)
      atomically $ modifyTVar' var (Map.insert ch wk)

    -- Use this as a finalizer to remove the key from the map when its value gets GC'd
    removeDeadVal :: TVar (Map V2.CausalHash (Weak (V1.Branch.Branch Sqlite.Transaction))) -> V2.CausalHash -> IO ()
    removeDeadVal var ch = liftIO do
      atomically $ modifyTVar' var (Map.delete ch)
      Debug.debugLogM Debug.Temp $ "Removed dead branch from cache, " <> show ch
