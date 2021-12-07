{-# LANGUAGE CPP #-}

module Compat where

import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import System.Mem.Weak (deRefWeak)
import Unison.Prelude
import qualified UnliftIO

#if defined(mingw32_HOST_OS)
import qualified GHC.ConsoleHandler as WinSig
#else
import qualified System.Posix.Signals as Sig
#endif

-- | Constructs a default interrupt handler which builds an interrupt handler which throws a
-- UserInterrupt exception to the thread in which the setup was initially called.
defaultInterruptHandler :: IO (IO ())
defaultInterruptHandler = do
  main_thread <- myThreadId
  wtid <- mkWeakThreadId main_thread
  let interrupt = do
        r <- deRefWeak wtid
        case r of
          Nothing -> return ()
          Just t -> throwTo t UserInterrupt
  pure interrupt

-- | Replaces any existing interrupt handlers with the provided IO action while the provided
-- action is running, restoring any existing handlers afterwards.
withInterruptHandler :: IO () -> IO a -> IO a
withInterruptHandler handler action = do
#if defined(mingw32_HOST_OS)
  let sig_handler WinSig.ControlC = handler
      sig_handler WinSig.Break    = handler
      sig_handler _               = return ()
  oldHandler <- WinSig.installHandler (WinSig.Catch sig_handler)
  a <- action `UnliftIO.finally` (void $ WinSig.installHandler oldHandler)
#else
  oldQuitHandler <- Sig.installHandler Sig.sigQUIT  (Sig.Catch handler) Nothing
  oldInterruptHandler <- Sig.installHandler Sig.sigINT   (Sig.Catch handler) Nothing
  a <- action `UnliftIO.finally` do
    void $ Sig.installHandler Sig.sigQUIT oldQuitHandler Nothing
    void $ Sig.installHandler Sig.sigINT oldInterruptHandler Nothing
#endif
  pure a
