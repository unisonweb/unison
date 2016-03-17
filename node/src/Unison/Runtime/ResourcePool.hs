module Unison.Runtime.ResourcePool where

import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Map as M
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Hashable as H

-- acquire returns the resource, and the cleanup action ("finalizer") for that resource
data Pool p r = Pool { acquire :: p -> Int -> IO (r, IO ()) }

type ResourceKey p = (p,CC.ThreadId)
type Cache p r = M.Map (ResourceKey p) (r, UTCTime, IO ())

addResourceToMap :: (Ord p, H.Hashable p) =>  (r -> IO ()) -> Cache p r -> p -> r -> Int -> Int -> IO CC.ThreadId -> MVar.MVar Int -> IO ()
addResourceToMap releaser map p r wait maxPoolSize getThread ps = do
  now <- getCurrentTime
  poolSize <- MVar.takeMVar poolSize
  threadId <- getThread
  let expiry = addUTCTime (fromIntegral wait) now
  if wait > 0 && (poolSize < maxPoolSize) then
    -- add the resource to the map with the releaser
    M.insert (p,threadId) (r,expiry, (releaser r)) map
      >> MVar.putMVar ps (poolSize + 1)
  else -- immedately release
    releaser r

_acquire :: (Ord p, H.Hashable p) => (p -> IO r) -> (r -> IO ()) -> Cache p r -> Int -> IO CC.ThreadId -> MVar.MVar Int -> p -> Int -> IO (r, IO ())
_acquire acquirer releaser map maxPoolSize getThread ps p wait = do
  threadId <- getThread
  m <- M.lookup (p,threadId) map
  poolSize <- MVar.takeMVar poolSize
  r <- case m of
        Just (r, _, _) -> M.delete (p,threadId) map
          >> MVar.putMVar ps (poolSize - 1)
          >> return r
        Nothing -> acquirer p
  return (r, (addResourceToMap releaser map p r wait maxPoolSize getThread))

cleanCache :: (Ord p, H.Hashable p) => Cache p r -> IO ()
cleanCache map = do
  return ()
  -- now <- getCurrentTime
  -- let keysNReleasers = M.foldrWithKey (\k _ knrs ->
  --                                     case M.lookup k map of
  --                                         Just (_, expiry, releaser) ->
  --                                           if expiry < now then (k, releaser) : knrs
  --                                           else knrs
  --                                         Nothing -> knrs) [] map
  --     newMap = foldr (\(k,_) m -> M.delete k m) map keysNReleasers
  -- (sequence $ map snd keysNReleasers)

cleanCacheLoop :: (Ord p, H.Hashable p) => Cache p r -> IO b
cleanCacheLoop mVarCache =
  cleanCache mVarCache >> CC.threadDelay 1000000 >> cleanCacheLoop mVarCache

pool :: (Ord p, H.Hashable p) => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
pool maxPoolSize acquirer releaser = do
  map <- M.empty
  ps  <- MVar.newMVar 0
  _ <- CC.forkIO (cleanCacheLoop map)
  return Pool { acquire = _acquire acquirer releaser map maxPoolSize CC.myThreadId ps }

poolWithoutGC :: (Ord p, H.Hashable p) => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
poolWithoutGC maxPoolSize acquirer releaser = do
  map <- M.empty
  ps  <- MVar.newMVar 0
  return Pool { acquire = _acquire acquirer releaser map maxPoolSize CC.myThreadId ps }
