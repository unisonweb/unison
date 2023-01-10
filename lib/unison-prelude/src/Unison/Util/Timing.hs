module Unison.Util.Timing
  ( time,
    unsafeTime,
  )
where

import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import System.CPUTime (getCPUTime)
import System.IO.Unsafe (unsafePerformIO)
import qualified Unison.Debug as Debug
import UnliftIO (MonadIO, liftIO)

time :: MonadIO m => String -> m a -> m a
time label ma =
  if Debug.shouldDebug Debug.Timing
    then do
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
    else ma

-- Mitchell says: this function doesn't look like it would work at all; let's just delete it
unsafeTime :: Monad m => String -> m a -> m a
unsafeTime label ma =
  if Debug.shouldDebug Debug.Timing
    then do
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
    else ma
