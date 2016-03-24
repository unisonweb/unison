module Unison.Runtime.ResourcePool where

import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Map as M
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Data.Hashable as H
import qualified Control.Monad.STM as STM
import qualified Data.IORef as IORef

-- acquire returns the resource, and the cleanup action ("finalizer") for that resource
data Pool p r = Pool { acquire :: p -> Int -> IO (r, IO ()) }

type ResourceKey p = (p,CC.ThreadId)
type Cache p r = M.Map (ResourceKey p) (r, UTCTime, IO ())
type ReaperQueue p = TQ.TQueue (ResourceKey p)

incRef :: Num t => t -> (t, ())
incRef a = (a+1,())
decRef :: Num t => t -> (t, ())
decRef a = (a-1,())

addResourceToMap :: (Ord p, H.Hashable p) =>  (r -> IO ()) -> Cache p r -> p -> r -> Int -> Int -> IO CC.ThreadId -> IORef.IORef Int -> ReaperQueue p -> IO ()
addResourceToMap release pool p r wait maxPoolSize getThread poolSizeR queue = do
  now <- getCurrentTime
  poolSize <- IORef.readIORef poolSizeR
  threadId <- getThread
  let expiry = addUTCTime (fromIntegral wait) now
  let key = (p,threadId)
  if wait > 0 && (poolSize < maxPoolSize) then
    -- add the resource to the pool with the release
    M.insert key (r, expiry, (release r)) pool
        >> IORef.atomicModifyIORef poolSizeR incRef
        >> STM.atomically (TQ.writeTQueue queue key)
    else release r

_acquire :: (Ord p, H.Hashable p) => (p -> IO r) -> (r -> IO ()) -> Cache p r -> Int -> IORef.IORef Int -> ReaperQueue p -> IO CC.ThreadId -> p -> Int -> IO (r, IO ())
_acquire acquire release pool maxPoolSize poolSizeR queue getThread p wait = do
  threadId <- getThread
  m <- M.lookup (p,threadId) pool
  r <- case m of
        Just (r, _, _) -> M.delete (p,threadId) pool
          >> IORef.atomicModifyIORef poolSizeR decRef
          >> return r
        Nothing -> acquire p
  return (r, (addResourceToMap release pool p r wait maxPoolSize getThread poolSizeR queue))

cleanPool :: (Ord p, H.Hashable p) => Cache p r -> ReaperQueue p -> IO ()
cleanPool pool queue = do
  next <- STM.atomically $ TQ.tryPeekTQueue queue
  now <- getCurrentTime
  case next of
    Just key -> do
        value <- M.lookup key pool
        msg <- STM.atomically $ TQ.readTQueue queue
        case value of
          Just (_, expiry, releaser) ->
            if expiry < now then
              M.delete key pool >> releaser
                 >> cleanPool pool queue
            else (STM.atomically $ TQ.writeTQueue queue msg)
          Nothing -> return ()
    Nothing -> return ()

cleanPoolLoop :: (Ord p, H.Hashable p) => Cache p r -> ReaperQueue p -> IO b
cleanPoolLoop mVarCache q =
  cleanPool mVarCache q >> CC.threadDelay 1000000 >> cleanPoolLoop mVarCache q

pool :: (Ord p, H.Hashable p) => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
pool maxPoolSize acquire release = do
  pool <- M.empty
  q <- STM.atomically TQ.newTQueue
  ps  <- IORef.newIORef 0
  _ <- CC.forkIO (cleanPoolLoop pool q)
  return Pool { acquire = _acquire acquire release pool maxPoolSize ps q CC.myThreadId }

poolWithoutGC :: (Ord p, H.Hashable p) => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
poolWithoutGC maxPoolSize acquire release = do
  pool <- M.empty
  q <- STM.atomically TQ.newTQueue
  ps  <- IORef.newIORef 0
  return Pool { acquire = _acquire acquire release pool maxPoolSize ps q CC.myThreadId }
