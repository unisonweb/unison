module Unison.Util.Timing
  ( time,
  )
where

import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import System.CPUTime (getCPUTime)
import Unison.Debug qualified as Debug
import UnliftIO (MonadIO, liftIO)

time :: (MonadIO m) => String -> m a -> m a
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
