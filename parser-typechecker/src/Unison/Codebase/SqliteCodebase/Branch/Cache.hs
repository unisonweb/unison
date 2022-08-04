module Unison.Codebase.SqliteCodebase.Branch.Cache where

import qualified Data.Map as Map
import qualified U.Codebase.HashTags as V2
import qualified Unison.Codebase.Branch as V1.Branch
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import UnliftIO.STM

-- type BranchCache m = V2.Branch.CausalBranch m -> m (V1.Branch.UnwrappedBranch m)
data BranchCache' m n = BranchCache
  { lookupCachedBranch :: V2.CausalHash -> m (Maybe (V1.Branch.Branch n)),
    insertCachedBranch :: V2.CausalHash -> (V1.Branch.Branch n) -> m ()
  }

type BranchCache m = BranchCache' m m

type TransactionBranchCache = BranchCache' Sqlite.Transaction Sqlite.Transaction

newTransactionBranchCache :: Sqlite.Transaction TransactionBranchCache
newTransactionBranchCache = Sqlite.unsafeIO do
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
    lookupCachedBranch' :: TVar (Map V2.CausalHash (V1.Branch.Branch n)) -> V2.CausalHash -> m (Maybe (V1.Branch.Branch n))
    lookupCachedBranch' var ch = do
      cache <- readTVarIO var
      let result = Map.lookup ch cache
      when (isJust result) $ traceM "Branch Cache Hit"
      pure result

    insertCachedBranch' :: TVar (Map V2.CausalHash (V1.Branch.Branch n)) -> V2.CausalHash -> (V1.Branch.Branch n) -> m ()
    insertCachedBranch' var ch b = do
      atomically $ modifyTVar' var (Map.insert ch b)
