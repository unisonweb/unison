module Unison.Runtime.Journal where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TSem
import Data.Functor
import Unison.BlockStore (BlockStore)
import Unison.Runtime.Block (Block)
import qualified Unison.Runtime.Block as Block

-- | `flush` flushes the updates to the store
data Updates u = Updates { flush :: IO (), append :: u -> STM () }

-- | `record` produces a new checkpoint and clears `updates`
data Journal a u = Journal { get :: STM a, updates :: Updates u, record :: IO () }

-- update :: u -> Journal a u -> IO ()
-- update u j = append (updates j) u

-- | Updates the journal; invariant here is that when the `STM ()` is run, updates are durable
-- and also visible (in memory). Updates _may_ be durable and visible before
-- that but this isn't guaranteed.
updateAsync :: u -> Journal a u -> IO (STM ())
updateAsync u j = undefined

-- | Updates the journal; updates are visible immediately, but aren't necessarily
-- durable until the `STM ()` is run.
updateNowAsyncFlush :: u -> Journal a u -> IO (STM ())
updateNowAsyncFlush u j = undefined

-- | Updates the journal; updates are visible immediately, and are durable when
-- this function returns.
update :: u -> Journal a u -> IO ()
update u j = do
  force <- updateNowAsyncFlush u j
  atomically force

fromBlocks :: Eq h => BlockStore h -> (a -> [u] -> a) -> Block a -> Block u -> IO (Journal a u)
fromBlocks bs apply checkpoint us =
  undefined
  {-
  do
  sem <- atomically $ newTSem 1
  let j = Journal apply (Block.get bs checkpoint) (Updates (pure ()) append gets) (record j sem)
  pure j
  where
  append u = void $ Block.append bs us u
  gets = sequenceA =<< Block.gets bs us
  record j sem = bracket (atomically $ waitTSem sem) (\_ -> atomically $ signalTSem sem) $ \_ -> do
    aOld <- Block.get bs checkpoint
    log <- currentLog (updates j)
    when (not . null $ log) $ do
      let commitUpdates = init log
          stickBack = last log
      _ <- Block.modify' bs checkpoint (const $ apply aOld commitUpdates)
      _ <- Block.modify' bs us (const stickBack)
      pure ()
  -}

-- TODO implement buffering and auto checkpointing
-- idea - keep a bounded queue of updates
-- buffer :: Journal a u -> IO (Journal a u)
