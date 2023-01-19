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
    foo -- TODO remove
  )
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryReadMVar, tryPutMVar)
import Data.Atomics (Ticket, peekTicket, readForCAS, casIORef)
import Data.IORef -- TODO remove

newtype Promise a = Promise { state :: MVar a}

-- create an empty promise
newPromise :: IO (Promise a)
newPromise  = fmap Promise newEmptyMVar

-- read the value of the promise
-- return immediately if the promise if full, block if empty
readPromise :: Promise a -> IO a
readPromise Promise { state } = readMVar state

-- try to read the value of the promise
-- immediately return Nothing if the promise is empty
tryReadPromise :: Promise a -> IO (Maybe a)
tryReadPromise Promise { state } = tryReadMVar state

-- if the promise is empty, write the value, awake all readers and return True
-- if full, ignore the write and return False
writePromise :: a -> Promise a -> IO Bool
writePromise value Promise { state } = tryPutMVar state value

-- TODO remove
foo :: IO (Bool, Bool)
foo = do
  state <- newIORef (0 :: Integer)
  ticket <- readForCAS state
  success <- fmap fst $ casIORef state ticket 5
  writeIORef state 10
  failure <- fmap fst $ casIORef state ticket 5
  pure (success, failure)
