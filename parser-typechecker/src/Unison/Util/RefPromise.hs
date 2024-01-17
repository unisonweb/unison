module Unison.Util.RefPromise
  ( Ticket,
    peekTicket,
    readForCAS,
    casIORef,
    Promise,
    newPromise,
    readPromise,
    tryReadPromise,
    writePromise,
  )
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar, tryReadMVar)
import Data.Atomics (Ticket, casIORef, peekTicket, readForCAS)

newtype Promise a = Promise {state :: MVar a}

-- create an empty promise
newPromise :: IO (Promise a)
newPromise = fmap Promise newEmptyMVar

-- read the value of the promise
-- return immediately if the promise if full, block if empty
readPromise :: Promise a -> IO a
readPromise Promise {state} = readMVar state

-- try to read the value of the promise
-- immediately return Nothing if the promise is empty
tryReadPromise :: Promise a -> IO (Maybe a)
tryReadPromise Promise {state} = tryReadMVar state

-- if the promise is empty, write the value, awake all readers and return True
-- if full, ignore the write and return False
writePromise :: Promise a -> a -> IO Bool
writePromise Promise {state} value = tryPutMVar state value
