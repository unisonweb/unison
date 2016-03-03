module Unison.Runtime.ResourcePool where

import qualified Data.Map as M

-- acquire returns the resource, and the cleanup action ("finalizer") for that resource
data Pool p r = Pool { acquire :: p -> IO (r, IO ()) }

iacquire :: (p -> IO r) -> p -> IO (r, IO ())
iacquire a p = do
  r <- a p
  return (r, return ())

pool :: Ord p => Int -> (p -> IO r) -> (r -> IO ()) -> IO (Pool p r)
pool maxPoolSize a release =
  return $ Pool { acquire = iacquire a }
