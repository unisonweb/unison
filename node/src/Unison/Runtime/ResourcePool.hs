module Unison.Runtime.ResourcePool where

import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Map as M
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Data.Hashable as H
import qualified Control.Monad.STM as STM
import qualified Data.IORef as IORef

-- acquire returns the resource, and the cleanup action ("finalizer") for that resource
data Pool p r = Pool { acquire :: p -> IO (r, IO ()) }

type ResourceKey p = (p,CC.ThreadId)
type Cache p r = M.Map (ResourceKey p) (r, UTCTime, IO ())
type ReaperQueue p = TQ.TQueue (ResourceKey p)
type MaxPoolSize = Int
type Seconds = Int
type PoolSize = Int

incRef :: Num t => t -> (t, ())
incRef a = (a+1,())
decRef :: Num t => t -> (t, ())
decRef a = (a-1,())

addResourceToMap :: (Ord p, H.Hashable p) =>  (r -> IO ()) -> Cache p r -> p -> r -> Seconds -> MaxPoolSize -> IO CC.ThreadId -> IORef.IORef PoolSize -> ReaperQueue p -> IO ()
addResourceToMap release pool p r waitInSeconds maxPoolSize getThread poolSizeR queue = do
  now <- getCurrentTime
  poolSize <- IORef.readIORef poolSizeR
  threadId <- getThread
  let expiry = addUTCTime (fromIntegral waitInSeconds) now
  let key = (p,threadId)
  if waitInSeconds > 0 && (poolSize < maxPoolSize) then
    -- add the resource to the pool with the release
    M.insert key (r, expiry, (release r)) pool
      >> IORef.atomicModifyIORef poolSizeR incRef
      >> STM.atomically (TQ.writeTQueue queue key)
    else release r

_acquire :: (Ord p, H.Hashable p) => (p -> IO r) -> (r -> IO ()) -> Cache p r -> Seconds -> MaxPoolSize -> IORef.IORef PoolSize -> ReaperQueue p -> IO CC.ThreadId -> p -> IO (r, IO ())
_acquire acquire release pool waitInSeconds maxPoolSize poolSizeR queue getThread p = do
  threadId <- getThread
  m <- M.lookup (p,threadId) pool
  r <- case m of
        Just (r, _, _) -> M.delete (p,threadId) pool
          >> IORef.atomicModifyIORef poolSizeR decRef
          >> return r
        Nothing -> acquire p
  return (r, (addResourceToMap release pool p r waitInSeconds maxPoolSize getThread poolSizeR queue))

cleanPool :: (Ord p, H.Hashable p) => Cache p r -> ReaperQueue p -> IO ()
cleanPool pool queue = do
  key <- STM.atomically $ TQ.peekTQueue queue
  now <- getCurrentTime
  value <- M.lookup key pool
  case value of
    Just (_, expiry, releaser) ->
      if expiry < now then
        (STM.atomically $ TQ.readTQueue queue)
          >> M.delete key pool
          >> releaser
          >> cleanPool pool queue
      else
        let nextExpiry = round ((realToFrac $ diffUTCTime now expiry)::Double)
        in CC.threadDelay (1000000*nextExpiry)
             >> cleanPool pool queue
    Nothing -> return ()

pool :: (Ord p, H.Hashable p) => Seconds -> MaxPoolSize -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
pool waitInSeconds maxPoolSize acquire release = do
  pool <- M.empty
  q <- STM.atomically TQ.newTQueue
  ps  <- IORef.newIORef 0
  _ <- CC.forkIO (cleanPool pool q)
  return Pool { acquire = _acquire acquire release pool waitInSeconds maxPoolSize ps q CC.myThreadId }

poolWithoutGC :: (Ord p, H.Hashable p) => Seconds -> MaxPoolSize -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
poolWithoutGC waitInSeconds maxPoolSize acquire release = do
  pool <- M.empty
  q <- STM.atomically TQ.newTQueue
  ps  <- IORef.newIORef 0
  return Pool { acquire = _acquire acquire release pool waitInSeconds maxPoolSize ps q CC.myThreadId }
