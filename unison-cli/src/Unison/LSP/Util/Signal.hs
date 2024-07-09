-- | A transactional signal type.
-- Similar to a broadcast channel, but with better memory characteristics when you only care about the latest value.
--
-- Allows multiple consumers to detect the latest value of a signal, and to be notified when the signal changes.
module Unison.LSP.Util.Signal
  ( newSignalIO,
    writeSignal,
    writeSignalIO,
    subscribe,
    Signal,
  )
where

import Control.Monad.STM qualified as STM
import Unison.Prelude
import UnliftIO.STM

newtype Signal a = Signal (TVar (Maybe a, Int))

-- | Create a new signal with an optional initial value.
newSignalIO :: (MonadIO m) => Maybe a -> m (Signal a)
newSignalIO a = do
  tvar <- newTVarIO (a, 0)
  pure (Signal tvar)

-- | Update the value of a signal, notifying all subscribers (even if the value didn't change)
writeSignal :: Signal a -> a -> STM ()
writeSignal (Signal signalVar) a = do
  (_, n) <- readTVar signalVar
  writeTVar signalVar (Just a, succ n)

-- | Update the value of a signal, notifying all subscribers (even if the value didn't change)
writeSignalIO :: (MonadIO m) => Signal a -> a -> m ()
writeSignalIO signal a = liftIO $ STM.atomically (writeSignal signal a)

-- | Subscribe to a signal, returning an STM action which will read the latest NEW value,
-- after successfully reading a new value, subsequent reads will retry until there's a new value written to the signal.
--
-- Each independent reader should have its own subscription.
--
-- >>> signal <- newSignalIO (Just "initial")
-- >>> subscriber1 <- subscribe signal
-- >>> subscriber2 <- subscribe signal
-- >>> -- Should return the initial value
-- >>> atomically (optional subscriber1)
-- >>> -- Should retry, since the signal hasn't changed.
-- >>> atomically (optional subscriber1)
-- >>> writeSignalIO signal "new value"
-- >>> -- Each subscriber should return the newest value
-- >>> ("sub1",) <$> atomically (optional subscriber1)
-- >>> ("sub2",) <$> atomically (optional subscriber2)
-- >>> -- Both should now retry
-- >>> ("sub1",) <$> atomically (optional subscriber1)
-- >>> ("sub2",) <$> atomically (optional subscriber2)
-- Just "initial"
-- Nothing
-- ("sub1",Just "new value")
-- ("sub2",Just "new value")
-- ("sub1",Nothing)
-- ("sub2",Nothing)
subscribe :: (MonadIO m) => Signal a -> m (STM a)
subscribe (Signal signalVar) = do
  (_, n) <- readTVarIO signalVar
  -- Start with a different n, so the subscriber will trigger on its first read.
  latestNVar <- newTVarIO (pred n)
  pure $ do
    (mayA, newN) <- readTVar signalVar
    latestN <- readTVar latestNVar
    guard (newN /= latestN)
    writeTVar latestNVar newN
    -- Retry until we have a value.
    case mayA of
      Nothing -> STM.retry
      Just a -> pure a
