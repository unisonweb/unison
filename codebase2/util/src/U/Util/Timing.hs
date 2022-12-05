{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module U.Util.Timing where

import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import System.CPUTime (getCPUTime)
import qualified Unison.Debug as Debug
import UnliftIO (MonadIO, liftIO)

-- | Time an action
time :: MonadIO m => String -> m a -> m a
time = timeM liftIO

-- | Like 'time', but allows timing within monads that don't implement MonadIO (such as Transaction) by passing
-- a way to lift the IO action into the monad.
timeM :: Monad m => (forall x. IO x -> m x) -> String -> m a -> m a
timeM ioLift label ma =
  if Debug.shouldDebug Debug.Timing
    then do
      systemStart <- ioLift getSystemTime
      cpuPicoStart <- ioLift getCPUTime
      ioLift $ putStrLn $ "Timing " ++ label ++ "..."
      a <- ma
      cpuPicoEnd <- ioLift getCPUTime
      systemEnd <- ioLift getSystemTime
      let systemDiff = diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart)
      let cpuDiff = picosecondsToDiffTime (cpuPicoEnd - cpuPicoStart)
      ioLift $ putStrLn $ "Finished " ++ label ++ " in " ++ show cpuDiff ++ " (cpu), " ++ show systemDiff ++ " (system)"
      pure a
    else ma
