{-# Language RankNTypes #-}
module Unison.Runtime.Journal where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TSem
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Unison.BlockStore (BlockStore)
import Unison.Runtime.Block (Block)
import qualified Unison.Runtime.Block as Block

type VisibleNow = Bool

-- | `flush` flushes the updates to the store
data Updates u = Updates { flush :: STM (), append :: VisibleNow -> u -> STM (STM ()) }

-- | A sequentially-updated, persistent `a` value. `get` obtains the current value.
-- `updates` can be used to append updates to the value. `recordAsync` produces a new
-- checkpoint and clears `updates`. It is guaranteed durable when the inner `STM ()` completes.
data Journal a u = Journal { get :: STM a
                           , updates :: Updates u
                           , recordAsync :: STM (STM ())
                           }

-- | Record a new checkpoint synchronously. When the returned `STM` completes,
-- the checkpoint is durable.
record :: Journal a u -> IO ()
record j = atomically (recordAsync j) >>= atomically

-- | Updates the journal; invariant here is that after inner `STM ()` is run, updates are durable
-- and also visible in memory. Updates _may_ be durable and visible before
-- that but this isn't guaranteed.
updateAsync :: u -> Journal a u -> STM (STM ())
updateAsync u j = append (updates j) False u

-- | Updates the journal; updates are visible immediately, but aren't necessarily
-- durable until the `STM ()` is run.
updateNowAsyncFlush :: u -> Journal a u -> STM (STM ())
updateNowAsyncFlush u j = append (updates j) True u

-- | Updates the journal; updates are visible and durable when this function returns.
update :: u -> Journal a u -> IO ()
update u j = do
  force <- atomically $ updateNowAsyncFlush u j
  atomically force

-- | Create a Journal from two blocks, an identity update, and a function for applying
-- an update to the state.
fromBlocks :: Eq h => BlockStore h -> (u -> a -> a) -> Block a -> Block (Maybe u) -> IO (Journal a u)
fromBlocks bs apply checkpoint us = do
  current <- Block.get bs checkpoint
  us' <- (pure . catMaybes) =<< sequenceA =<< Block.gets bs us
  current <- atomically $ newTVar (foldl' (flip apply) current us')
  updateQ <- atomically $ newTQueue
  latestEnqueue <- atomically $ newTVar (pure ())
  err <- atomically $ newTVar Nothing
  get <- pure $ readTVar err >>= maybe (readTVar current) fail
  let flush = join $ get >> readTVar latestEnqueue
  append <- pure $ \vnow u -> do
    cur <- get
    done <- newTSem 0
    writeTVar latestEnqueue (waitTSem done)
    writeTQueue updateQ (Just (u, vnow), signalTSem done)
    let cur' = apply u cur --maybe cur (`apply` cur) u
    waitTSem done <$
      if vnow then cur' `seq` writeTVar current cur'
      else pure ()
  _ <- forkIO . forever $ do
    -- write updates to BlockStore asynchronously, notifying of completion or errors
    (uvnow, done) <- atomically $ readTQueue updateQ -- will die when the `Journal` gets GC'd
    id $
      let
        handle :: SomeException -> IO ()
        handle e = atomically $ modifyTVar' err (const (Just . show $ e))
      in case uvnow of
        Nothing -> do
          now <- atomically get
          _ <- catch (Block.modify' bs checkpoint (const now) >>
                      Block.modify' bs us (const Nothing) >>
                      pure ())
                     handle
          atomically done
        Just (u, vnow) ->
          catch (Block.append bs us (Just u) >> update) handle
          where
          update | not vnow  = atomically $ done >> modifyTVar' current (apply u)
                 | otherwise = atomically done
  pure $ Journal get (Updates flush append) (record updateQ)
  where
  record updateQ = do
    done <- newTSem 0
    writeTQueue updateQ (Nothing, signalTSem done)
    pure $ waitTSem done

-- | Log a new checkpoint every `updateCount` updates made to the returned journal
checkpointEvery :: Int -> Journal a u -> STM (Journal a u)
checkpointEvery updateCount (Journal get updates record) = tweak <$> newTVar 0 where
  tweak count =
    let
      record' = writeTVar count (0 :: Int) >> record
      append' vnow u = do
        done <- modifyTVar' count (1+) >> append updates vnow u
        count' <- readTVar count
        case count' of
          _ | count' >= updateCount -> record'
            | otherwise -> pure done
    in Journal get (Updates (flush updates) append') record'
