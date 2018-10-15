module Unison.Util.TQueue where

import qualified Control.Concurrent.STM.TQueue as T
import qualified Control.Concurrent.STM as STM

data TQueue a = TQueue (T.TQueue a) (STM.TVar Int)

newTQueueIO :: IO (TQueue a)
newTQueueIO = TQueue <$> T.newTQueueIO <*> STM.newTVarIO 0

peekTQueue :: TQueue a -> STM.STM a
peekTQueue (TQueue q _) = T.peekTQueue q

writeCountTQueue :: TQueue a -> STM.STM Int
writeCountTQueue (TQueue _ v) = STM.readTVar v

flushTQueue :: TQueue a -> STM.STM [a]
flushTQueue (TQueue q _) = T.flushTQueue q
