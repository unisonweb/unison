{-# LANGUAGE LambdaCase #-}
module Unison.Util.TQueue where

import Data.Word (Word64)
import Data.Foldable (toList)
import Data.Functor (($>))
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM hiding (TQueue)
import qualified Data.Sequence as S
import Data.Sequence (Seq((:<|)), (|>))

data TQueue a = TQueue (TVar (Seq a)) (TVar Word64)

newIO :: IO (TQueue a)
newIO = TQueue <$> newTVarIO mempty <*> newTVarIO 0

peek :: TQueue a -> STM a
peek (TQueue v _) = readTVar v >>= \case
  a :<| _ -> pure a
  _ -> retry

dequeue :: TQueue a -> STM a
dequeue (TQueue v _) = readTVar v >>= \case
  a :<| as -> writeTVar v as *> pure a
  _ -> retry

dequeueN :: TQueue a -> Int -> STM [a]
dequeueN (TQueue v _) n = readTVar v >>= \s ->
  if length s >= n then writeTVar v (S.drop n s) $> toList (S.take n s)
  else retry

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
  modifyTVar' count (+1)

raceIO :: STM a -> STM b -> IO (Either a b)
raceIO a b = do
  aa <- Async.async $ atomically a
  ab <- Async.async $ atomically b
  Async.waitEitherCancel aa ab

-- take all elements up to but not including the first not satisfying cond
tryPeekWhile :: (a -> Bool) -> TQueue a -> STM [a]
tryPeekWhile cond (TQueue v _) = toList . S.takeWhileL cond <$> readTVar v

-- block until at least one element is enqueued not satisfying cond,
-- then return the prefix before that
takeWhile :: (a -> Bool) -> TQueue a -> STM [a]
takeWhile cond (TQueue v _) = readTVar v >>= \s -> let
  (left, right) = S.spanl cond s in
  if null right then retry
  else writeTVar v right $> toList left

peekWhile :: (a -> Bool) -> TQueue a -> STM [a]
peekWhile cond (TQueue v _) = readTVar v >>= \s -> let
  (left, right) = S.spanl cond s in
  if null right then retry
  else pure $ toList left
