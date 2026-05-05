{-# LANGUAGE CPP #-}

module Unison.Test.Runtime.Process (test) where

#if defined(linux_HOST_OS) || defined(darwin_HOST_OS)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
#endif
import EasyTest
#ifdef mingw32_HOST_OS
import System.Environment (getEnv)
#endif
import System.Exit (ExitCode (ExitSuccess))
import System.IO (Handle)
import System.Process (ProcessHandle, waitForProcess)
#if defined(linux_HOST_OS) || defined(darwin_HOST_OS)
import System.Process (readCreateProcessWithExitCode, shell)
#endif
import Unison.Runtime.Foreign.Function (ForeignConvention (decodeVal, encodeVal), foreignCall)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (IO_process_start))
import Unison.Runtime.MCode (Args (VArg2))
import Unison.Runtime.Stack (alloc, bumpn, exStackIOToIO, peek, pokeOff, unpackXStack)
import Unison.Util.Text qualified as Util.Text

test :: Test ()
test =
  scope "process" do
    scope "explicit wait" do
      (_, _, _, ph) <- io $ startInteractiveProcessViaForeignCall =<< successfulCommand
      exitCode <- io $ waitForProcess ph
      expectEqual ExitSuccess exitCode

#if defined(linux_HOST_OS) || defined(darwin_HOST_OS)
    scope "dropped handles are reaped" do
      cmd <- io successfulCommand
      io $ replicateM_ 10 $ startInteractiveProcessViaForeignCall cmd
      io $ threadDelay 1000000
      count <- io zombieChildCount
      expectEqual 0 count
#endif

startInteractiveProcessViaForeignCall :: (FilePath, [String]) -> IO (Handle, Handle, Handle, ProcessHandle)
startInteractiveProcessViaForeignCall (exe, args) = do
  stk0 <- alloc >>= \stk -> bumpn stk 2
  pokeOff stk0 1 (encodeVal exe)
  pokeOff stk0 0 (encodeVal (Util.Text.pack <$> args))
  (_, stk1) <- exStackIOToIO $ foreignCall IO_process_start (VArg2 1 0) (unpackXStack stk0)
  decodeVal =<< peek stk1

successfulCommand :: IO (FilePath, [String])
#ifdef mingw32_HOST_OS
successfulCommand = do
  systemRoot <- getEnv "SystemRoot"
  pure (systemRoot <> "\\System32\\whoami.exe", [])
#else
successfulCommand = pure ("/bin/sh", ["-c", "exit 0"])
#endif

#if defined(linux_HOST_OS) || defined(darwin_HOST_OS)
zombieChildCount :: IO Int
zombieChildCount = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode
      (shell "printf '%s\\n' \"$PPID\"; ps -axo ppid=,stat=")
      ""
  case exitCode of
    ExitSuccess ->
      case lines stdout of
        parentLine : childLines ->
          case reads parentLine of
            [(parentPid, "")] -> pure . length $ filter (isZombieChild parentPid) childLines
            _ -> fail $ "Could not parse parent pid from: " <> show parentLine
        [] -> fail "Could not parse process table output"
    _ -> fail $ "Could not count zombie children: " <> stderr

isZombieChild :: Int -> String -> Bool
isZombieChild parentPid line =
  case words line of
    ppidText : stat : _ ->
      case reads ppidText of
        [(ppid, "")] -> ppid == parentPid && take 1 stat == "Z"
        _ -> False
    _ -> False
#endif
