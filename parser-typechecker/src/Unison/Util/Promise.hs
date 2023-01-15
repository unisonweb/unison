module Unison.Util.Promise where

import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryReadMVar, tryPutMVar)

newtype Promise a = Promise { state :: MVar a}

-- create an empty promise
new :: IO (Promise a)
new  = fmap Promise newEmptyMVar

-- readsthe value of the promise
-- return immediately if the promise if full, block if empty
read :: Promise a -> IO a
read Promise { state } = readMVar state

-- try to read the value of the promise
-- immediately return Nothing if the promise is empty
tryRead :: Promise a -> IO (Maybe a)
tryRead Promise { state } = tryReadMVar state

-- if the promise is empty, write the value, awake all readers and return True
-- if full, ignore the write and return False
write :: a -> Promise a -> IO Bool
write value Promise { state } = tryPutMVar state value
