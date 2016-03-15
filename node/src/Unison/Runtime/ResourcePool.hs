module Unison.Runtime.ResourcePool where

import qualified Data.Map as M
import qualified Control.Concurrent.MVar as MVar
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Control.Concurrent as CC

-- acquire returns the resource, and the cleanup action ("finalizer") for that resource
data Pool p r = Pool { acquire :: p -> Int -> IO (r, IO ()) }

type ResourceKey p = (p,CC.ThreadId)
type Cache p r = MVar.MVar (M.Map (ResourceKey p) (r, UTCTime, IO ()))

addResourceToMap :: Ord p =>  (r -> IO ()) -> Cache p r -> p -> r -> Int -> Int -> IO CC.ThreadId -> IO ()
addResourceToMap releaser mVarCache p r wait maxPoolSize getThread = do
  now <- getCurrentTime
  threadId <- getThread
  let expiry = addUTCTime (fromIntegral wait) now
  cacheMap <- MVar.takeMVar mVarCache
  newCacheMap <- if wait > 0 && ((length cacheMap) < maxPoolSize) then
                   -- add the resource to the mVarCache with the releaser
                   return $ M.insert (p,threadId) (r,expiry, (releaser r)) cacheMap
                 else -- immedately release and dont add to mVarCache
                   releaser r >> return cacheMap
  MVar.putMVar mVarCache newCacheMap

_acquire :: Ord p => (p -> IO r) -> (r -> IO ()) -> Cache p r -> Int -> IO CC.ThreadId -> p -> Int -> IO (r, IO ())
_acquire acquirer releaser mVarCache maxPoolSize getThread p wait = do
  threadId <- getThread
  cacheMap <- MVar.takeMVar mVarCache
  (newMap, r) <- case M.lookup (p,threadId) cacheMap of
                  Just (r, _, _) -> return (M.delete (p,threadId) cacheMap, r)
                  Nothing -> ((,) cacheMap) <$> acquirer p
  MVar.putMVar mVarCache newMap
    >> return (r, (addResourceToMap releaser mVarCache p r wait maxPoolSize getThread))

cleanCache :: Ord p => Cache p r -> IO ()
cleanCache mVarCache = do
  now <- getCurrentTime
  cacheMap <- MVar.takeMVar mVarCache
  let keysNReleasers = M.foldrWithKey (\k _ knrs ->
                                      case M.lookup k cacheMap of
                                          Just (_, expiry, releaser) ->
                                            if expiry < now then (k, releaser) : knrs
                                            else knrs
                                          Nothing -> knrs) [] cacheMap
      newMap = foldr (\(k,_) m -> M.delete k m) cacheMap keysNReleasers
  (sequence $ map snd keysNReleasers)
    >> MVar.putMVar mVarCache newMap

cleanCacheLoop :: Ord p => Cache p r -> IO b
cleanCacheLoop mVarCache =
  cleanCache mVarCache >> CC.threadDelay 1000000 >> cleanCacheLoop mVarCache

pool :: Ord p => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
pool maxPoolSize acquirer releaser = do
  mVarCache <- MVar.newMVar M.empty
  _ <- CC.forkIO (cleanCacheLoop mVarCache)
  return Pool { acquire = _acquire acquirer releaser mVarCache maxPoolSize CC.myThreadId }

poolWithoutGC :: Ord p => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
poolWithoutGC maxPoolSize acquirer releaser = do
  mVarCache <- MVar.newMVar M.empty
  return Pool { acquire = _acquire acquirer releaser mVarCache maxPoolSize CC.myThreadId }
