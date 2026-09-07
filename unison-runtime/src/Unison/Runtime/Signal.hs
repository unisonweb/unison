{-# LANGUAGE CPP #-}

-- | Process-wide POSIX signal subscriptions. No Unison code runs in an OS
-- signal handler: the RTS schedules a Haskell action which publishes to STM.
module Unison.Runtime.Signal
  ( Signal,
    Subscription,
    available,
    subscribe,
    await,
    close,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Exception (throwIO)
import Data.Unique (Unique)

#if !defined(mingw32_HOST_OS)
import Control.Concurrent.MVar
import Control.Exception (mask_, onException)
import Control.Monad (forM, forM_, unless, when)
import Data.Map.Strict qualified as Map
import Data.Unique (newUnique)
import Foreign.C.Error (throwErrnoIfMinus1_, throwErrnoIfNull)
import Foreign.C.Types (CInt (..))
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals qualified as Posix
#endif

newtype Signal = Signal Int deriving (Eq, Ord)

-- The registration retains only the notification cell, not this handle.
-- Nothing means closed; Just True means at least one notification is pending.
data Subscription = Subscription !Signal !Unique !(STM.TVar (Maybe Bool))

instance Eq Subscription where
  Subscription _ a _ == Subscription _ b _ = a == b

instance Ord Subscription where
  compare (Subscription _ a _) (Subscription _ b _) = compare a b

await :: Subscription -> IO ()
await (Subscription _ _ pending) = STM.atomically do
  STM.readTVar pending >>= \case
    Nothing -> STM.throwSTM (userError "Signal subscription is closed")
    Just False -> STM.retry
    Just True -> STM.writeTVar pending (Just False)

#if defined(mingw32_HOST_OS)

available :: [(String, Signal)]
available = []

subscribe :: Signal -> IO Subscription
subscribe _ = throwIO (userError "POSIX signal subscriptions are not supported on Windows")

close :: Subscription -> IO ()
close _ = pure ()

#else

data Registration = Registration
  { previousHandler :: !Posix.Handler,
    previousAction :: !(Ptr ()),
    listeners :: !(STM.TVar (Map.Map Unique (STM.TVar (Maybe Bool))))
  }

-- Separate registrations per installation prevent an already queued callback
-- from an old installation from reaching a later generation of subscribers.
{-# NOINLINE registrations #-}
registrations :: MVar (Map.Map Signal Registration)
registrations = unsafePerformIO (newMVar Map.empty)

-- Synchronous hardware faults cannot be delivered safely as asynchronous
-- notifications. SIGPIPE is used by GHC to interrupt blocking foreign calls;
-- the RTS timer signal is excluded through reservedSignals as well.
-- Real-time signals (queued payloads and ordering) need a different contract.
available :: [(String, Signal)]
available = unsafePerformIO do
  count <- signalCount
  entries <- forM [0 .. count - 1] \index -> do
    name <- peekCString =<< signalName index
    number <- signalNumber index
    pure (name, number)
  pure
    [ (name, Signal (fromIntegral number))
      | (name, number) <- entries,
        not (Posix.inSignalSet number Posix.reservedSignals)
    ]
{-# NOINLINE available #-}

subscribe :: Signal -> IO Subscription
subscribe signal@(Signal number) = mask_ do
  unless (signal `elem` map snd available) $
    throwIO (userError "Unsupported POSIX signal")
  ident <- newUnique
  pending <- STM.newTVarIO (Just False)
  modifyMVar_ registrations \table -> do
    registration <- case Map.lookup signal table of
      Just registration -> pure registration
      Nothing -> do
        listeners <- STM.newTVarIO Map.empty
        saved <- throwErrnoIfNull "save signal handler" (saveAction (fromIntegral number))
        previous <-
          Posix.installHandler (fromIntegral number) (Posix.Catch (notify listeners)) Nothing
            `onException` free saved
        pure (Registration previous saved listeners)
    STM.atomically $ STM.modifyTVar' (listeners registration) (Map.insert ident pending)
    pure (Map.insert signal registration table)
  pure (Subscription signal ident pending)

notify :: STM.TVar (Map.Map Unique (STM.TVar (Maybe Bool))) -> IO ()
notify listeners = STM.atomically do
  cells <- STM.readTVar listeners
  forM_ cells \pending -> STM.writeTVar pending (Just True)

close :: Subscription -> IO ()
close (Subscription signal@(Signal number) ident pending) = mask_ $
  modifyMVar_ registrations \table -> case Map.lookup signal table of
    Nothing -> pure table
    Just registration -> do
      cells <- STM.readTVarIO (listeners registration)
      if Map.notMember ident cells
        then pure table
        else do
          let remaining = Map.delete ident cells
          when (Map.null remaining) do
            -- Restore the Haskell handler table and then the complete native
            -- action, including flags and mask which unix does not preserve.
            _ <- Posix.installHandler (fromIntegral number) (previousHandler registration) Nothing
            throwErrnoIfMinus1_ "restore signal handler" $
              restoreAction (fromIntegral number) (previousAction registration)
            free (previousAction registration)
          STM.atomically do
            STM.writeTVar pending Nothing
            STM.writeTVar (listeners registration) remaining
          pure if Map.null remaining then Map.delete signal table else table

foreign import ccall unsafe "unison_signal_save"
  saveAction :: CInt -> IO (Ptr ())

foreign import ccall unsafe "unison_signal_restore"
  restoreAction :: CInt -> Ptr () -> IO CInt

foreign import ccall unsafe "unison_signal_count"
  signalCount :: IO CInt

foreign import ccall unsafe "unison_signal_name"
  signalName :: CInt -> IO CString

foreign import ccall unsafe "unison_signal_number"
  signalNumber :: CInt -> IO CInt

#endif
