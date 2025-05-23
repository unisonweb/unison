module Unison.Codebase.Watch
  ( watchDirectory,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM qualified as STM
import Control.Exception (MaskingState (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Time.Clock (UTCTime, diffUTCTime)
import GHC.Conc (registerDelay)
import GHC.IO (unsafeUnmask)
import Ki qualified
import System.FSNotify (Event (..))
import System.FSNotify qualified as FSNotify
import Unison.Prelude
import UnliftIO.Exception (finally, tryAny)
import UnliftIO.STM (atomically)

watchDirectory :: Ki.Scope -> FSNotify.WatchManager -> FilePath -> (FilePath -> Bool) -> IO (IO (FilePath, Text))
watchDirectory scope mgr dir allow = do
  eventQueue <- forkDirWatcherThread scope mgr dir allow

  -- Await an event from the event queue with the following simple debounce logic, which is intended to work around the
  -- tendency for modern editors to create a flurry of rapid filesystem events when a file is saved:
  --
  -- 1. Block until an event arrives.
  -- 2. Keep consuming events until 50ms elapse without an event.
  -- 3. Return only the last event.
  --
  -- Note we don't have any smarts here for a flurry of events that are related to more than one file; we just throw
  -- everything away except the last event. In practice, this has seemed to work fine.
  let awaitEvent0 :: IO (FilePath, UTCTime)
      awaitEvent0 = do
        let go :: (FilePath, UTCTime) -> IO (FilePath, UTCTime)
            go event0 = do
              var <- registerDelay 50_000
              (join . atomically . asum)
                [ do
                    event1 <- STM.readTQueue eventQueue
                    pure (go event1),
                  do
                    STM.readTVar var >>= STM.check
                    pure (pure event0)
                ]
        event <- atomically (STM.readTQueue eventQueue)
        go event

  -- Enhance the previous "await event" action with a small file cache that serves as a second debounce implementation.
  -- We keep in memory the file contents of previously-saved files, so that we can avoid emitting events for files that
  -- last changed less than 500ms ago, and whose contents haven't changed.
  previousFilesRef <- newIORef Map.empty
  let awaitEvent1 :: IO (FilePath, Text)
      awaitEvent1 = do
        (file, t) <- awaitEvent0
        tryAny (readUtf8 file) >>= \case
          -- Somewhat-expected read error from a file that was just written. Just ignore the event and try again.
          Left _ -> awaitEvent1
          Right contents -> do
            previousFiles <- readIORef previousFilesRef
            case Map.lookup file previousFiles of
              Just (contents0, t0) | contents == contents0 && (t `diffUTCTime` t0) < 0.5 -> awaitEvent1
              _ -> do
                writeIORef previousFilesRef $! Map.insert file (contents, t) previousFiles
                pure (file, contents)

  -- Enhance the previous "await" event action by first clearing the whole event queue (tossing old filesystem events
  -- we may have accumulated while e.g. running a long-running IO action), and *then* waiting.
  let awaitEvent2 :: IO (FilePath, Text)
      awaitEvent2 = do
        _ <- STM.atomically (STM.flushTQueue eventQueue)
        awaitEvent1

  pure awaitEvent2

-- | `forkDirWatcherThread scope mgr dir allow` forks a background thread into `scope` that, using "file watcher
-- manager" `mgr` (just a boilerplate argument the caller is responsible for creating), watches directory `dir` for
-- all "added" and "modified" filesystem events that occur on files that pass the `allow` predicate. It returns a queue
-- of such event that is (obviously) meant to be read or flushed, never written.
forkDirWatcherThread :: Ki.Scope -> FSNotify.WatchManager -> FilePath -> (FilePath -> Bool) -> IO (STM.TQueue (FilePath, UTCTime))
forkDirWatcherThread scope mgr dir allow = do
  queue <- STM.newTQueueIO

  let handler :: Event -> IO ()
      handler = \case
        Added fp t FSNotify.IsFile | allow fp -> atomically (STM.writeTQueue queue (fp, t))
        CloseWrite fp t FSNotify.IsFile | allow fp -> atomically (STM.writeTQueue queue (fp, t))
        Modified fp t FSNotify.IsFile | allow fp -> atomically (STM.writeTQueue queue (fp, t))
        _ -> pure ()

  -- A bit of a "one too many threads" situation but there's not much we can easily do about it. The `fsnotify` API
  -- doesn't expose any synchronous API; the only option is to fork a background thread with a callback. So, we spawn
  -- a thread that spawns *that* thread, then waits forever. The purpose here is to simply leverage `ki` exception
  -- propagation machinery to ensure that the `fsnotify` thread is properly cleaned up.
  Ki.forkWith_ scope Ki.defaultThreadOptions {Ki.maskingState = MaskedUninterruptible} do
    -- The goal here is to prevent spawning this background watching thread before installing an exception handler that
    -- guarantees it's killed. Unfortunately the fsnotify API doesn't seem to make that possible (hence the first
    -- `unsafeUnmask` here), since we do need the thread *it* spawns to be killable, and (at least as of version
    -- 0.4.2.0) they don't take care to guarantee that; it just inherits the masking state.
    stopListening <- unsafeUnmask (FSNotify.watchDir mgr dir (const True) handler) <|> pure (pure ())
    unsafeUnmask (forever (threadDelay maxBound)) `finally` stopListening

  pure queue
