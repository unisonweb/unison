module Unison.Runtime.Lock where

-- | Autoreleasing lock type. `tryAcquire`, if successful, returns a `Lease`
-- which will be valid for some period of time, during which it may be assumed
-- that other calls to `tryAcquire` on the same `Lock` will fail.
data Lock = Lock { tryAcquire :: IO (Maybe Lease) }

-- | A `Lease` should be released under normal operation, but in the
-- event of an untimely crash, it will also become invalid on its own
-- at some point.
data Lease = Lease { valid :: IO Bool, release :: IO () }
