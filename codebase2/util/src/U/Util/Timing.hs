{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE BangPatterns #-}

module U.Util.Timing where

import System.CPUTime (getCPUTime)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (MonadIO, liftIO)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import Data.Time.Clock (picosecondsToDiffTime)

enabled :: Bool
enabled = False

time :: MonadIO m => String -> m a -> m a
time _ ma | not enabled = ma
time label ma = do
  systemStart <- liftIO getSystemTime
  cpuPicoStart <- liftIO getCPUTime
  liftIO $ putStrLn $ "Timing " ++ label ++ "..."
  a <- ma
  cpuPicoEnd <- liftIO getCPUTime
  systemEnd <- liftIO getSystemTime
  let systemDiff = diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart)
  let cpuDiff = picosecondsToDiffTime (cpuPicoEnd - cpuPicoStart)
  liftIO $ putStrLn $ "Finished " ++ label ++ " in " ++ show cpuDiff ++ " (cpu), " ++ show systemDiff ++ " (system)"
  pure a

unsafeTime :: Monad m => String -> m a -> m a
unsafeTime _ ma | not enabled = ma
unsafeTime label ma = do
  let !systemStart = unsafePerformIO getSystemTime
      !cpuPicoStart = unsafePerformIO getCPUTime
      !_ = unsafePerformIO $ putStrLn $ "Timing " ++ label ++ "..."
  a <- ma
  let !cpuPicoEnd = unsafePerformIO getCPUTime
      !systemEnd = unsafePerformIO getSystemTime
  let systemDiff = diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart)
  let cpuDiff = picosecondsToDiffTime (cpuPicoEnd - cpuPicoStart)
  let !_ = unsafePerformIO $ putStrLn $ "Finished " ++ label ++ " in " ++ show cpuDiff ++ " (cpu), " ++ show systemDiff ++ " (system)"
  pure a
