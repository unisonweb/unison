{-# LANGUAGE CPP #-}

module Unison.Test.Runtime.Signal (test) where

import EasyTest
import Unison.Runtime.Signal qualified as Signal

#if !defined(mingw32_HOST_OS)
import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import System.Posix.Signals qualified as Posix
import System.Posix.Signals.Exts qualified as Posix
import System.Timeout (timeout)
#endif

test :: Test ()
test = scope "signals" do
#if defined(mingw32_HOST_OS)
  scope "unavailable on Windows" $ expect (null Signal.available)
#else
  scope "catalog includes HUP and WINCH, excludes unsafe signals" do
    let names = map fst Signal.available
    expect (all (`elem` names) ["SIGHUP", "SIGWINCH", "SIGUSR1", "SIGCHLD"])
    expect (all (`notElem` names) ["SIGKILL", "SIGSTOP", "SIGSEGV", "SIGPIPE"])

  scope "notification can arrive before await" do
    result <- io $ withSubscription "SIGWINCH" \subscription -> do
      Posix.raiseSignal Posix.sigWINCH
      timeout 2000000 (Signal.await subscription)
    expectEqual (Just ()) result

  scope "independent subscribers receive the same signal" do
    result <- io $ withSubscription "SIGUSR1" \first ->
      withSubscription "SIGUSR1" \second -> do
        Posix.raiseSignal Posix.sigUSR1
        timeout 2000000 (Signal.await first >> Signal.await second)
    expectEqual (Just ()) result

  scope "closing one subscriber leaves the other active" do
    result <- io $ withSubscription "SIGUSR1" \first ->
      withSubscription "SIGUSR1" \second -> do
        Signal.close first
        Signal.close first
        Posix.raiseSignal Posix.sigUSR1
        timeout 2000000 (Signal.await second)
    expectEqual (Just ()) result

  scope "close wakes await with an IO error" do
    result <- io $ withSubscription "SIGWINCH" \subscription -> do
      started <- newEmptyMVar
      finished <- newEmptyMVar
      bracket
        (forkIO $ putMVar started () >> try @IOException (Signal.await subscription) >>= putMVar finished)
        killThread
        \_ -> do
          takeMVar started
          Signal.close subscription
          timeout 2000000 (takeMVar finished)
    expect case result of
      Just (Left _) -> True
      _ -> False

  scope "cancelled await does not cancel the subscription" do
    result <- io $ withSubscription "SIGWINCH" \subscription -> do
      first <- timeout 10000 (Signal.await subscription)
      Posix.raiseSignal Posix.sigWINCH
      second <- timeout 2000000 (Signal.await subscription)
      pure (first, second)
    expectEqual (Nothing, Just ()) result

  scope "signals are routed separately" do
    result <- io $ withSubscription "SIGUSR1" \first ->
      withSubscription "SIGUSR2" \second -> do
        Posix.raiseSignal Posix.sigUSR1
        received <- timeout 2000000 (Signal.await first)
        unrelated <- timeout 10000 (Signal.await second)
        pure (received, unrelated)
    expectEqual (Just (), Nothing) result

  scope "SIGHUP is handled without terminating the process" do
    result <- io $ withSubscription "SIGHUP" \subscription -> do
      Posix.raiseSignal Posix.sigHUP
      timeout 2000000 (Signal.await subscription)
    expectEqual (Just ()) result

  scope "previous Haskell handler is restored" do
    result <- io do
      notified <- newEmptyMVar
      bracket
        (Posix.installHandler Posix.sigUSR1 (Posix.Catch (putMVar notified ())) Nothing)
        (\old -> void $ Posix.installHandler Posix.sigUSR1 old Nothing)
        \_ -> do
          withSubscription "SIGUSR1" (const (pure ()))
          Posix.raiseSignal Posix.sigUSR1
          timeout 2000000 (takeMVar notified)
    expectEqual (Just ()) result

  scope "native handler, flags and mask survive multiple subscriptions" do
    result <- io $ bracket (nativeBegin Posix.sigUSR2) nativeEnd \saved ->
      if saved == nullPtr
        then fail "Could not install native test signal handler"
        else do
          withSubscription "SIGUSR2" \first ->
            withSubscription "SIGUSR2" \second -> do
              Posix.raiseSignal Posix.sigUSR2
              received <- timeout 2000000 (Signal.await first >> Signal.await second)
              if received == Just () then pure () else fail "Signal notification timed out"
          restored <- nativeRestored Posix.sigUSR2
          Posix.raiseSignal Posix.sigUSR2
          count <- nativeCount
          pure (restored, count)
    expectEqual (1, 1) result

withSubscription :: String -> (Signal.Subscription -> IO a) -> IO a
withSubscription name action = case lookup name Signal.available of
  Nothing -> fail ("Signal not available: " <> name)
  Just signal -> bracket (Signal.subscribe signal) Signal.close action

foreign import ccall unsafe "unison_test_signal_begin"
  nativeBegin :: CInt -> IO (Ptr ())
foreign import ccall unsafe "unison_test_signal_end"
  nativeEnd :: Ptr () -> IO ()
foreign import ccall unsafe "unison_test_signal_restored"
  nativeRestored :: CInt -> IO CInt
foreign import ccall unsafe "unison_test_signal_count"
  nativeCount :: IO CInt
#endif
