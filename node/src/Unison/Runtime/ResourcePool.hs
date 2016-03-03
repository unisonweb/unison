module Unison.Runtime.ResourcePool where

import qualified Data.Map as M

-- acquire returns the resource, and the cleanup action ("finalizer") for that resource
data Pool p r = Pool { acquire :: p -> IO (r, IO ()) }

iacquire :: (p -> IO r) -> (r -> IO()) -> p -> IO (r, IO ())
iacquire acquirer releaser p = do
  r <- acquirer p
  return (r, (releaser r))

pool :: Ord p => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
pool maxPoolSize acquirer releaser =
  return $ Pool { acquire = iacquire acquirer releaser }
