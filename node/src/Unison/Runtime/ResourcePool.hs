module Unison.Runtime.ResourcePool where

import qualified Data.Map as M
import qualified Control.Concurrent.MVar as MVar
import Data.Time (UTCTime,getCurrentTime)
import Control.Concurrent

-- acquire returns the resource, and the cleanup action ("finalizer") for that resource
data Pool p r = Pool { acquire :: p -> Int -> IO (r, IO ()) }

type Cache p r = MVar.MVar (M.Map p (r, UTCTime))

_acquire :: (Ord p) => (p -> IO r) -> (r -> IO()) -> Cache p r -> p -> Int -> IO (r, IO ())
_acquire acquirer releaser cache p wait = do
  now <- getCurrentTime
  cachemap <- MVar.takeMVar cache
  r <- case M.lookup p cachemap of
          Just (r, _) -> return r
          Nothing -> acquirer p
  let newCacheMap =
        if wait > 0 then M.insert p (r,now) cachemap
        else cachemap
  MVar.putMVar cache newCacheMap >> return (r, (releaser r))

cleanCache :: (Ord p) => Cache p r -> IO ()
cleanCache cache = do
  now <- getCurrentTime
  cachemap <- MVar.takeMVar cache
  let keys = M.keys cachemap
      newmap = foldr (\k m ->
                    case M.lookup k cachemap of
                        Just (_, expiry) ->
                          if expiry < now then M.delete k m else m
                        Nothing -> m) cachemap keys
  MVar.putMVar cache newmap


pool :: Ord p => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
pool maxPoolSize acquirer releaser = do
  cache <- MVar.newMVar M.empty
  return Pool { acquire = _acquire acquirer releaser cache }
