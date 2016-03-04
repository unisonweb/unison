module Unison.Runtime.ResourcePool where

import qualified Data.Map as M
import qualified Control.Concurrent.MVar as MVar
import Data.Time (UTCTime, getCurrentTime, addUTCTime,secondsToDiffTime)
import qualified Control.Concurrent as CC
import qualified Control.Monad

-- acquire returns the resource, and the cleanup action ("finalizer") for that resource
data Pool p r = Pool { acquire :: p -> Int -> IO (r, IO ()) }

type Cache p r = MVar.MVar (M.Map p (r, UTCTime, IO ()))

addResourceToMap :: (Ord p) =>  (r -> IO()) -> Cache p r -> p -> r -> Int -> Int -> IO ()
addResourceToMap releaser cache p r wait maxPoolSize = do
  now <- getCurrentTime
  let expiry = addUTCTime (fromIntegral wait) now
  cachemap <- MVar.takeMVar cache
  newCacheMap <- if wait > 0 && ((length cachemap) < maxPoolSize) then
                   -- add the resource to the cache with the releaser
                   return $ M.insert p (r,expiry, (releaser r)) cachemap
                 else -- immedately release and dont add to cache
                   releaser r >> return cachemap
  MVar.putMVar cache newCacheMap

_acquire :: (Ord p) => (p -> IO r) -> (r -> IO()) -> Cache p r -> Int -> p -> Int -> IO (r, IO ())
_acquire acquirer releaser cache maxPoolSize p wait = do
  cachemap <- MVar.takeMVar cache
  r <- case M.lookup p cachemap of
          Just (r, _, _) -> return r
          Nothing -> acquirer p
  MVar.putMVar cache cachemap
    >> return (r, (addResourceToMap releaser cache p r wait maxPoolSize))

cleanCache :: (Ord p) => Cache p r -> IO ()
cleanCache cache = do
  now <- getCurrentTime
  cachemap <- MVar.takeMVar cache
  let emptyKeys = [] :: [(p, IO())]
  let keysNReleasers = M.foldrWithKey (\k _ knrs ->
                                      case M.lookup k cachemap of
                                          Just (_, expiry, releaser) ->
                                            if expiry < now then (k, releaser) : knrs
                                            else knrs
                                          Nothing -> knrs) emptyKeys cachemap
      newMap = foldr (\(k,_) m -> M.delete k m) cachemap keysNReleasers
  id <- CC.myThreadId
  (sequence $ map snd keysNReleasers)
    >> MVar.putMVar cache newMap

cleanCacheLoop cache =
  cleanCache cache >> CC.threadDelay 1000000 >> cleanCacheLoop cache

pool :: Ord p => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
pool maxPoolSize acquirer releaser = do
  cache <- MVar.newMVar M.empty
  id <- CC.forkIO (cleanCacheLoop cache)
  return Pool { acquire = _acquire acquirer releaser cache maxPoolSize }

poolWithoutGC :: Ord p => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
poolWithoutGC maxPoolSize acquirer releaser = do
  cache <- MVar.newMVar M.empty
  return Pool { acquire = _acquire acquirer releaser cache maxPoolSize }
