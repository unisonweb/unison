module Unison.Runtime.ResourcePool where

import Control.Applicative
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TVar (TVar)
import Data.Functor
import Data.Map (Map)
import Data.Maybe
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import qualified Data.IORef as IORef
import qualified Data.Map as Map

-- | `acquire` returns the resource, and the cleanup action ("finalizer") for that resource
data ResourcePool p r = ResourcePool { acquire :: p -> IO (r, IO ()) }

{-
The `ResourcePool p r` is backed by a mutable `Map p (RecycleQueue r)`.
The `RecycleQueue r` is a queue of resources that have been logically
'released' but are available for recycling. After their expiration, a
separate 'reaper thread' will remove them from the queue.

The basic logic for `acquire` is:

1.  Try to obtain a recycled resource for the requested key.
2a. If that succeeds, the resource is taken from the recycle queue,
    preventing multiple threads from accessing the same resource.
2b. If that fails, acquire a fresh resource.
3.  The returned finalizer just adds an entry to the recycle queue,
    for the recycled or newly acquired resource

Thus, multiple resources may be acquired for the same key simultaneously,
and these resources will be recycled if another acquisition occurs before
expiration.
-}

data Cache p r =
  Cache { count :: IORef.IORef PoolSize
        , recycleQueues :: TVar (Map p (RecycleQueue r)) }

type RecycleQueue r = (TVar QueueSize, TQ.TQueue (TMVar r, CC.ThreadId, IO ()))
type MaxPoolSize = Int
type Seconds = Int
type PoolSize = Int
type QueueSize = Int

incrementCount :: Cache p r -> IO ()
incrementCount c = IORef.atomicModifyIORef' (count c) (\a -> (a+1, ()))

decrementCount :: Cache p r -> IO ()
decrementCount c = IORef.atomicModifyIORef' (count c) (\a -> (a-1, ()))

getCount :: Cache p r -> IO Int
getCount c = IORef.readIORef (count c)

lookupQueue :: Ord p => p -> Cache p r -> IO (RecycleQueue r)
lookupQueue p (Cache _ m) = do
  q <- Map.lookup p <$> TVar.readTVarIO m
  case q of
    Nothing -> do
      q <- STM.atomically TQ.newTQueue
      qSize <- STM.atomically (TVar.newTVar 0)
      let tweak old = Just $ fromMaybe (qSize,q) old
      STM.atomically $ TVar.modifyTVar' m (\m -> Map.alter tweak p  m)
      pure (qSize, q)
    Just q -> pure q

recycleOrReacquire :: (p -> IO r) -> (r -> IO ()) -> Cache p q -> RecycleQueue r -> p -> IO (r, IO ())
recycleOrReacquire acquire release cache (n,q) p = do
  avail <- STM.atomically $ TQ.tryReadTQueue q
  case avail of
    Nothing -> do
      r <- acquire p
      r' <- STM.atomically $ TMVar.newTMVar (Just r)
      pure (r, release r)
    Just (r, id, release') -> do
      decrementCount cache
      r' <- STM.atomically $ TMVar.tryTakeTMVar r
      case r' of
        -- a reaper thread has claimed this resource for finalization, keep looking
        Nothing -> recycleOrReacquire acquire release cache (n,q) p
        Just r -> do
          STM.atomically $ TVar.modifyTVar' n (\n -> n - 1)
          CC.killThread id
          pure (r, release')

_acquire :: (Ord p) => (p -> IO r) -> (r -> IO ()) -> Cache p r -> Seconds -> MaxPoolSize -> p
         -> IO (r, IO ())
_acquire acquire release cache waitInSeconds maxPoolSize p = do
  q@(qn,qe) <- lookupQueue p cache
  (r, release) <- recycleOrReacquire acquire release cache q p
  delayedRelease <- pure $ do
    currentSize <- IORef.readIORef (count cache)
    case currentSize of
      n | n >= maxPoolSize -> release
        | otherwise        -> do
          incrementCount cache
          STM.atomically (TVar.modifyTVar' qn (1+))
          r' <- STM.atomically (TMVar.newTMVar r)
          id <- CC.forkIO $ do
            CC.threadDelay (1000000 * waitInSeconds)
            -- if an acquire succeeds at the same time, the TMVar will be empty, so noop
            msg <- STM.atomically $ TMVar.tryTakeTMVar r'
            case msg of
              Nothing -> pure ();
              Just _ -> release >> STM.atomically (TVar.modifyTVar' qn (\n -> n - 1))
            STM.atomically $ do -- GC empty queues
              n <- TVar.readTVar qn
              case n == 0 of
                True -> TVar.modifyTVar' (recycleQueues cache) (Map.delete p)
                False -> pure ()
          STM.atomically $ TQ.writeTQueue qe (r', id, release)
  pure (r, delayedRelease)

make :: Ord p => Seconds -> MaxPoolSize -> (p -> IO r) -> (r -> IO ()) -> IO (ResourcePool p r)
make waitInSeconds maxPoolSize acquire release = do
  ps <- IORef.newIORef 0
  recycleQueues <- TVar.newTVarIO Map.empty
  let cache = Cache ps recycleQueues
  pure $ ResourcePool { acquire = _acquire acquire release cache waitInSeconds maxPoolSize }
