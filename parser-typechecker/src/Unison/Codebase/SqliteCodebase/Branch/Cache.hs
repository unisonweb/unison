module Unison.Codebase.SqliteCodebase.Branch.Cache where

import qualified Data.Map as Map
import System.Mem.Weak
import qualified U.Codebase.HashTags as V2
import qualified Unison.Codebase.Branch as V1.Branch
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import UnliftIO.STM

data BranchCache' m n = BranchCache
  { lookupCachedBranch :: V2.CausalHash -> m (Maybe (V1.Branch.Branch n)),
    insertCachedBranch :: V2.CausalHash -> V1.Branch.Branch n -> m ()
  }

type BranchCache m = BranchCache' m m

type TransactionBranchCache = BranchCache' Sqlite.Transaction Sqlite.Transaction

newTransactionBranchCache :: MonadIO m => m TransactionBranchCache
newTransactionBranchCache = liftIO do
  BranchCache {lookupCachedBranch, insertCachedBranch} <- newBranchCache
  pure $
    BranchCache
      { lookupCachedBranch = Sqlite.unsafeIO . lookupCachedBranch,
        insertCachedBranch = \ch b -> Sqlite.unsafeIO $ insertCachedBranch ch b
      }

newBranchCache :: forall m n. MonadIO m => m (BranchCache' m n)
newBranchCache = do
  var <- newTVarIO mempty
  pure $
    BranchCache
      { lookupCachedBranch = lookupCachedBranch' var,
        insertCachedBranch = insertCachedBranch' var
      }
  where
    lookupCachedBranch' :: TVar (Map V2.CausalHash (Weak (V1.Branch.Branch n))) -> V2.CausalHash -> m (Maybe (V1.Branch.Branch n))
    lookupCachedBranch' var ch = liftIO do
      cache <- readTVarIO var
      case Map.lookup ch cache of
        Nothing -> pure Nothing
        Just weakRef -> deRefWeak weakRef

    insertCachedBranch' :: TVar (Map V2.CausalHash (Weak (V1.Branch.Branch n))) -> V2.CausalHash -> (V1.Branch.Branch n) -> m ()
    insertCachedBranch' var ch b = liftIO do
      -- It's worth reading the semantics of these operations.
      -- We may in the future wish to instead keep the branch object alive for as long as the
      -- CausalHash is alive, this is easy to do with 'mkWeak', but we'll start with only
      -- keeping the branch alive as long as it's directly referenced.
      wk <- mkWeakPtr b (Just $ removeDeadVal var ch)
      atomically $ modifyTVar' var (Map.insert ch wk)

    removeDeadVal :: TVar (Map V2.CausalHash (Weak (V1.Branch.Branch n))) -> V2.CausalHash -> IO ()
    removeDeadVal var ch = liftIO do
      atomically $ modifyTVar' var (Map.delete ch)
