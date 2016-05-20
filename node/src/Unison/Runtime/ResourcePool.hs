module Unison.Runtime.ResourcePool where

import Control.Concurrent.MVar (MVar)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Map as M
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Monad.STM as STM
import qualified Data.Hashable as H
import qualified Data.IORef as IORef

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
    for the recycled or newly acquired resource, with a new expiration
    based a delta plus the time the finalizer is called.

Thus, multiple resources may be acquired for the same key simultaneously,
and these resources will be recycled if another acquisition occurs before
expiration.
-}

data Cache p r =
  Cache { count :: IORef.IORef PoolSize
        , lock :: MVar ()  -- used as a lock when allocating new RecycleQueue
        , recycleQueues :: M.Map p (RecycleQueue r) }

type RecycleQueue r = TQ.TQueue (r, UTCTime, IO ())
type ReaperQueue p = TQ.TQueue p
type MaxPoolSize = Int
type Seconds = Int
type PoolSize = Int

incrementCount :: Cache p r -> IO ()
incrementCount (Cache count _ _) = IORef.atomicModifyIORef' count (\a -> (a+1, ()))

decrementCount :: Cache p r -> IO ()
decrementCount (Cache count _ _) = IORef.atomicModifyIORef' count (\a -> (a-1, ()))

getCount :: Cache p r -> IO Int
getCount (Cache count _ _) = IORef.readIORef count

drainQueue :: RecycleQueue r -> IO ()
drainQueue q = do
  (_, _, release) <- STM.atomically $ TQ.readTQueue q
  release
  drainQueue q

deleteQueue :: (Ord p, H.Hashable p) => p -> Cache p r -> IO ()
deleteQueue p (Cache count lock m) = do
  q <- M.lookup p m
  case q of
    Nothing -> pure ()
    Just q -> do
      M.delete p m
      -- ensures we don't miss calling any finalizers, drain thread will get GC'd
      _ <- CC.forkIO $ drainQueue q
      pure ()

lookupQueue :: (Ord p, H.Hashable p) => p -> Cache p r -> IO (RecycleQueue r)
lookupQueue p (Cache count lock m) = do
  q <- M.lookup p m
  case q of
    Nothing -> do
      MVar.takeMVar lock
      q <- STM.atomically TQ.newTQueue
      M.insert p q m
      MVar.putMVar lock ()
      pure q
    Just q -> pure q

_acquire :: (Ord p, H.Hashable p)
         => (p -> IO r)
         -> (r -> IO ())
         -> Cache p r
         -> Seconds
         -> MaxPoolSize
         -> ReaperQueue p
         -> p
         -> IO (r, IO ())
_acquire acquire release cache waitInSeconds maxPoolSize reaper p = do
  q <- lookupQueue p cache
  avail <- STM.atomically $ TQ.tryReadTQueue q
  (r, release) <- case avail of
    Nothing -> do
      r <- acquire p
      incrementCount cache
      pure (r, release r)
    Just (r, _, release) -> pure (r, release)
  delayedRelease <- pure $ do
    now <- getCurrentTime
    currentSize <- IORef.readIORef (count cache)
    case currentSize of
      n | n >= maxPoolSize -> release
        | otherwise        -> STM.atomically $ TQ.writeTQueue q (r, expiry, release)
          where expiry = addUTCTime (fromIntegral waitInSeconds) now
  STM.atomically $ TQ.writeTQueue reaper p
  pure (r, delayedRelease)

reaper :: (Ord p, H.Hashable p) => Cache p r -> ReaperQueue p -> IO ()
reaper cache queue = do
  key <- STM.atomically $ TQ.readTQueue queue
  valueQueue <- lookupQueue key cache
  value <- STM.atomically $ TQ.tryReadTQueue valueQueue
  now <- getCurrentTime
  case value of
    Just (_, expiry, releaser) | expiry < now -> do
      empty <- STM.atomically (TQ.isEmptyTQueue valueQueue)
      case empty of
        True -> deleteQueue key cache
        False -> pure ()
      decrementCount cache
      releaser
      reaper cache queue
    Just r@(_, expiry, _) -> do
      let nextExpiry = round ((realToFrac $ diffUTCTime now expiry) :: Double)
      STM.atomically $ TQ.unGetTQueue valueQueue r
      STM.atomically $ TQ.unGetTQueue queue key
      CC.threadDelay (1000000 * nextExpiry)
      reaper cache queue
    Nothing -> reaper cache queue

make :: (Ord p, H.Hashable p)
     => Seconds -> MaxPoolSize -> (p -> IO r) -> (r -> IO ()) -> IO (ResourcePool p r)
make waitInSeconds maxPoolSize acquire release = do
  ps <- IORef.newIORef 0
  lock <- MVar.newMVar ()
  recycleQueues <- M.empty
  reaperQueue <- STM.atomically TQ.newTQueue
  let cache = Cache ps lock recycleQueues
  _ <- CC.forkIO (reaper cache reaperQueue)
  pure $ ResourcePool { acquire = _acquire acquire release cache waitInSeconds maxPoolSize reaperQueue }
