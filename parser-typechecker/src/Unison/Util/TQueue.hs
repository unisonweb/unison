module Unison.Util.TQueue where

import Data.Sequence (Seq ((:<|)), (|>))
import Unison.Prelude
import UnliftIO.STM hiding (TQueue)

data TQueue a = TQueue (TVar (Seq a)) (TVar Word64)

newIO :: forall a m. (MonadIO m) => m (TQueue a)
newIO = TQueue <$> newTVarIO mempty <*> newTVarIO 0

peek :: TQueue a -> STM a
peek (TQueue v _) =
  readTVar v >>= \case
    a :<| _ -> pure a
    _ -> retrySTM

dequeue :: TQueue a -> STM a
dequeue (TQueue v _) =
  readTVar v >>= \case
    a :<| as -> writeTVar v as *> pure a
    _ -> retrySTM

undequeue :: TQueue a -> a -> STM ()
undequeue (TQueue v _) a =
  readTVar v >>= \as -> writeTVar v (a :<| as)

tryDequeue :: TQueue a -> STM (Maybe a)
tryDequeue (TQueue v _) =
  readTVar v >>= \case
    a :<| as -> writeTVar v as *> pure (Just a)
    _ -> pure Nothing

-- return the number of enqueues over the life of the queue
enqueueCount :: TQueue a -> STM Word64
enqueueCount (TQueue _ count) = readTVar count

flush :: TQueue a -> STM [a]
flush (TQueue v _) = do
  s <- readTVar v
  writeTVar v mempty
  pure . toList $ s

enqueue :: TQueue a -> a -> STM ()
enqueue (TQueue v count) a = do
  modifyTVar' v (|> a)
  modifyTVar' count (+ 1)
