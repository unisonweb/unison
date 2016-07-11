module Unison.Runtime.SharedResourceMap where

import Control.Concurrent.MVar
import Control.Exception (finally)
import Data.Hashable (Hashable)
import Prelude hiding (lookup)
import qualified Unison.Runtime.ExpiringMap as M

data SharedResourceMap k v
  = SharedResourceMap { acquiring :: M.ExpiringMap k (MVar ())
                      , resources :: M.ExpiringMap k v }

new :: (Hashable k, Eq k) => M.Seconds -> M.Seconds -> IO (SharedResourceMap k v)
new acquisition ttl = SharedResourceMap <$> M.new acquisition <*> M.new ttl

lookup :: (Eq k, Hashable k) => k -> SharedResourceMap k v -> IO (Maybe v)
lookup k m = M.lookup k (resources m)

lookupOrReplenish :: (Eq k, Hashable k) => k -> IO v -> SharedResourceMap k v -> IO v
lookupOrReplenish k replenish m = do
  v <- lookup k m
  case v of
    Nothing -> M.lookup k (acquiring m) >>= \sem -> case sem of
      Nothing -> do
        sem <- newMVar ()
        _ <- M.insert k sem (acquiring m)
        lookupOrReplenish k replenish m
      Just sem -> do
        takeMVar sem
        v <- (replenish >>= \v -> v <$ M.insert k v (resources m)) `finally` putMVar sem ()
        pure v
    Just v -> pure v
