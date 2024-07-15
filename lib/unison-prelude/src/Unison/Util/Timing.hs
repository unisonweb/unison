module Unison.Util.Timing
  ( time,
    deepTime,
  )
where

import Control.DeepSeq (NFData)
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import System.CPUTime (getCPUTime)
import Unison.Debug qualified as Debug
import UnliftIO (MonadIO, evaluate, liftIO)

-- | Time how long it takes to run an action, including evaluating the returned result to WHNF.
time :: (MonadIO m) => String -> m a -> m a
time label ma =
  if Debug.shouldDebug Debug.Timing
    then do
      systemStart <- liftIO getSystemTime
      cpuPicoStart <- liftIO getCPUTime
      liftIO $ putStrLn $ "Timing " ++ label ++ "..."
      a <- ma >>= UnliftIO.evaluate
      cpuPicoEnd <- liftIO getCPUTime
      systemEnd <- liftIO getSystemTime
      let systemDiff = diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart)
      let cpuDiff = picosecondsToDiffTime (cpuPicoEnd - cpuPicoStart)
      liftIO $ putStrLn $ "Finished " ++ label ++ " in " ++ show cpuDiff ++ " (cpu), " ++ show systemDiff ++ " (system)"
      pure a
    else ma

-- | Time how long it takes to run an action, including fully evaluating the returned result to normal form.
deepTime :: (MonadIO m) => (NFData a) => String -> m a -> m a
deepTime label ma =
  if Debug.shouldDebug Debug.Timing
    then do
      systemStart <- liftIO getSystemTime
      cpuPicoStart <- liftIO getCPUTime
      liftIO $ putStrLn $ "Timing " ++ label ++ "..."
      a <- ma >>= Debug.deepEvaluate
      cpuPicoEnd <- liftIO getCPUTime
      systemEnd <- liftIO getSystemTime
      let systemDiff = diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart)
      let cpuDiff = picosecondsToDiffTime (cpuPicoEnd - cpuPicoStart)
      liftIO $ putStrLn $ "Finished " ++ label ++ " in " ++ show cpuDiff ++ " (cpu), " ++ show systemDiff ++ " (system)"
      pure a
    else ma
