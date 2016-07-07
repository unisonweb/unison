module Unison.Runtime.Queue (Queue, empty, enqueue, dequeue) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TMVar

newtype Queue a = Q (TQueue (TMVar a))

empty :: STM (Queue a)
empty = Q <$> newTQueue

enqueue :: Queue a -> STM a -> STM ()
enqueue (Q q) a = do
  va <- newEmptyTMVar
  writeTQueue q va
  putTMVar va =<< a

dequeue :: Queue a -> STM a
dequeue (Q q) = takeTMVar =<< readTQueue q
