-- | Simple cache which just keeps the last n relevant project branches in memory.
-- The Branch Cache handles all the lookups of the actual branch data by hash, this cache serves only to keep the last
-- n accessed branches in memory so they don't get garbage collected. See the Branch Cache for more context.
--
-- This speeds up switching back and forth between project branches, and also serves to keep the current project branch
-- in memory so it won't be cleaned up by the Branch Cache, since the Branch Cache only keeps
-- a weak reference to the current branch and we no longer keep the actual branch in LoopState.
module Unison.Codebase.SqliteCodebase.ProjectRootCache
  ( newProjectRootCache,
    stashBranch,
  )
where

import Control.Concurrent.STM
import Unison.Codebase.Branch
import Unison.Prelude

data ProjectRootCache m = ProjectRootCache {capacity :: Int, cached :: TVar [Branch m]}

newProjectRootCache :: (MonadIO m) => Int -> m (ProjectRootCache n)
newProjectRootCache capacity = do
  var <- liftIO $ newTVarIO []
  pure (ProjectRootCache capacity var)

stashBranch :: (MonadIO n) => ProjectRootCache m -> Branch m -> n ()
stashBranch ProjectRootCache {capacity, cached} branch = do
  liftIO . atomically $ do
    modifyTVar cached $ \branches -> take capacity (branch : filter (/= branch) branches)
