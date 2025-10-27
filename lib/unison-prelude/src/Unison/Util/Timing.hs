module Unison.Util.Timing
  ( time,
    startTiming,
    stopTiming,
  )
where

import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Unison.Debug qualified as Debug
import UnliftIO (MonadIO, liftIO)

time :: (MonadIO m) => Text -> m a -> m a
time label action =
  if Debug.shouldDebug Debug.Timing
    then do
      startTime <- liftIO startTiming
      result <- action
      liftIO (stopTiming label startTime)
      pure result
    else action

startTiming :: IO (Word64, Integer)
startTiming = (,) <$> getMonotonicTimeNSec <*> getCPUTime

stopTiming :: Text -> (Word64, Integer) -> IO ()
stopTiming label (systemTimeStart, cpuTimeStart) = do
  (systemTimeEnd, cpuTimeEnd) <- startTiming
  let systemDiff = realToFrac @Word64 @Double (systemTimeEnd - systemTimeStart)
  let cpuDiff = realToFrac @Integer @Double (cpuTimeEnd - cpuTimeStart) / 1_000
  printf "%s: %s (cpu), %s (system)\n" label (renderNanos cpuDiff) (renderNanos systemDiff)
  where
    -- Render nanoseconds, trying to fit into 4 characters.
    renderNanos :: Double -> String
    renderNanos ns
      | ns < 0.5 = "0 ns"
      | ns < 995 = printf "%.0f ns" ns
      | ns < 9_950 = printf "%.2f µs" us
      | ns < 99_500 = printf "%.1f µs" us
      | ns < 995_000 = printf "%.0f µs" us
      | ns < 9_950_000 = printf "%.2f ms" ms
      | ns < 99_500_000 = printf "%.1f ms" ms
      | ns < 995_000_000 = printf "%.0f ms" ms
      | ns < 9_950_000_000 = printf "%.2f s" s
      | ns < 99_500_000_000 = printf "%.1f s" s
      | otherwise = printf "%.0f s" s
      where
        us = ns / 1_000
        ms = ns / 1_000_000
        s = ns / 1_000_000_000
