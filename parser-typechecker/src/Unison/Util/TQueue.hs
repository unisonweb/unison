module Unison.Util.TQueue where

import Data.Word (Word64)
import qualified Control.Concurrent.STM.TQueue as T
import qualified Control.Concurrent.STM as STM

data TQueue a = TQueue (T.TQueue a) (STM.TVar Word64)

newTQueueIO :: IO (TQueue a)
newTQueueIO = TQueue <$> T.newTQueueIO <*> STM.newTVarIO 0

peekTQueue :: TQueue a -> STM.STM a
peekTQueue (TQueue q _) = T.peekTQueue q

readTQueue :: TQueue a -> STM.STM a
readTQueue (TQueue q _) = T.readTQueue q

writeCountTQueue :: TQueue a -> STM.STM Word64
writeCountTQueue (TQueue _ v) = STM.readTVar v

flushTQueue :: TQueue a -> STM.STM [a]
flushTQueue (TQueue q _) = T.flushTQueue q

writeTQueue :: TQueue a -> a -> STM.STM ()
writeTQueue (TQueue q v) a = do
  T.writeTQueue q a
  STM.modifyTVar' v (+1)
