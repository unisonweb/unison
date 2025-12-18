module Unison.Codebase.Watch
  ( watchPath,
    WatchState (..),
    newWatchState,
    awaitEvent,
    unwatchPath,
    unwatchAllPaths,
    getWatchedPaths,
  )
where

import Control.Concurrent.STM (STM, TVar)
import Control.Concurrent.STM qualified as STM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Time.Clock (UTCTime, diffUTCTime)
import GHC.Conc (registerDelay)
import Ki qualified
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (splitFileName)
import System.FSNotify (Event (Added, Modified))
import System.FSNotify qualified as FSNotify
import Unison.Prelude
import UnliftIO.Exception (tryAny)
import UnliftIO.STM (atomically)

-- | State for managing multiple watched paths.
data WatchState = WatchState
  { -- | The Ki scope for managing watcher threads
    scope :: Ki.Scope,
    -- | The FSNotify watch manager
    watchManager :: FSNotify.WatchManager,
    -- | TVar containing the latest event from any watcher
    latestEventVar :: TVar (Maybe (FilePath, UTCTime)),
    -- | Map from watched paths to their stop-watching actions
    watchedPathsVar :: TVar (Map FilePath (IO ())),
    -- | Predicate for filtering files (e.g., .u files only)
    allowPredicate :: FilePath -> Bool,
    -- | Cache for debouncing file contents
    previousFilesRef :: IORef (Map FilePath (Text, UTCTime))
  }

-- | Create a new watch state. The Ki scope is used for structured concurrency -
-- when the scope exits, all watcher threads are automatically cleaned up.
newWatchState :: Ki.Scope -> FSNotify.WatchManager -> (FilePath -> Bool) -> IO WatchState
newWatchState scope mgr allow = do
  latestEventVar <- STM.newTVarIO Nothing
  watchedPathsVar <- STM.newTVarIO Map.empty
  previousFilesRef <- newIORef Map.empty
  pure
    WatchState
      { scope = scope,
        watchManager = mgr,
        latestEventVar = latestEventVar,
        watchedPathsVar = watchedPathsVar,
        allowPredicate = allow,
        previousFilesRef = previousFilesRef
      }

-- | Add a file or directory to be watched. Returns the canonical path if successful, Nothing otherwise.
--
-- Each watched path spawns a background thread via Ki that manages the FSNotify watcher.
-- When the Ki scope exits, all watcher threads are automatically cleaned up.
watchPath :: WatchState -> FilePath -> IO (Maybe FilePath)
watchPath ws path = do
  canonPath <- canonicalizePath path
  isDir <- doesDirectoryExist canonPath
  isFile <- doesFileExist canonPath
  if not (isDir || isFile)
    then pure Nothing
    else do
      alreadyWatched <- atomically $ Map.member canonPath <$> STM.readTVar ws.watchedPathsVar
      if alreadyWatched
        then pure (Just canonPath) -- Already watching, consider it a success
        else do
          -- Create the handler that writes to our shared TVar
          let handler :: Event -> IO ()
              handler = \case
                Added fp t FSNotify.IsFile | ws.allowPredicate fp -> atomically (STM.writeTVar ws.latestEventVar (Just (fp, t)))
                Modified fp t FSNotify.IsFile | ws.allowPredicate fp -> atomically (STM.writeTVar ws.latestEventVar (Just (fp, t)))
                _ -> pure ()

          -- Determine what to watch
          let watchAction =
                if isDir
                  then FSNotify.watchDir ws.watchManager canonPath (const True) handler
                  else do
                    -- For a single file, we watch the parent directory and filter for our file
                    let (parentDir, _) = splitFileName canonPath
                    FSNotify.watchDir ws.watchManager parentDir (\e -> eventPath e == canonPath) handler

          -- Start watching with FSNotify
          stopListening <- watchAction

          -- Record that we're watching this path, with the actual stop action
          atomically $ STM.modifyTVar ws.watchedPathsVar (Map.insert canonPath stopListening)
          pure (Just canonPath)
  where
    eventPath :: Event -> FilePath
    eventPath = \case
      Added p _time _isDir -> p
      Modified p _time _isDir -> p
      FSNotify.Removed p _time _isDir -> p
      FSNotify.ModifiedAttributes p _time _isDir -> p
      FSNotify.WatchedDirectoryRemoved p _time _isDir -> p
      FSNotify.CloseWrite p _time _isDir -> p
      FSNotify.Unknown p _time _isDir _eventString -> p

-- | Await an event from any watched source.
--
-- This function implements debouncing with the following logic, intended to work around the tendency
-- for modern editors to create a flurry of rapid filesystem events when a file is saved:
--
-- 1. Block until an event arrives.
-- 2. Keep consuming events until 50ms elapse without an event.
-- 3. Return only the last event.
--
-- Note we don't have any smarts here for a flurry of events that are related to more than one file;
-- we just throw everything away except the last event. In practice, this has seemed to work fine.
--
-- Additionally, we keep in memory the file contents of previously-saved files, so that we can avoid
-- emitting events for files that last changed less than 500ms ago, and whose contents haven't changed.
awaitEvent :: WatchState -> IO (FilePath, Text)
awaitEvent ws = do
  let awaitEvent0 :: IO (FilePath, UTCTime)
      awaitEvent0 = do
        let go :: (FilePath, UTCTime) -> IO (FilePath, UTCTime)
            go event0 = do
              var <- registerDelay 50_000
              (join . atomically . asum)
                [ do
                    event1 <- readLatestEvent
                    pure (go event1),
                  do
                    STM.readTVar var >>= STM.check
                    pure (pure event0)
                ]
        event <- atomically readLatestEvent
        go event

      readLatestEvent :: STM (FilePath, UTCTime)
      readLatestEvent =
        STM.readTVar ws.latestEventVar >>= \case
          Nothing -> STM.retry
          Just event -> do
            STM.writeTVar ws.latestEventVar Nothing
            pure event

  -- Apply debouncing based on file contents cache
  let awaitEvent1 :: IO (FilePath, Text)
      awaitEvent1 = do
        (file, t) <- awaitEvent0
        tryAny (readUtf8 file) >>= \case
          -- Somewhat-expected read error from a file that was just written. Just ignore the event and try again.
          Left _ -> awaitEvent1
          Right contents -> do
            previousFiles <- readIORef ws.previousFilesRef
            case Map.lookup file previousFiles of
              Just (contents0, t0) | contents == contents0 && (t `diffUTCTime` t0) < 0.5 -> awaitEvent1
              _ -> do
                writeIORef ws.previousFilesRef $! Map.insert file (contents, t) previousFiles
                pure (file, contents)

  awaitEvent1

-- | Stop watching a path. Returns True if the path was being watched.
unwatchPath :: WatchState -> FilePath -> IO Bool
unwatchPath ws path = do
  canonPath <- canonicalizePath path
  maybeStop <- atomically $ do
    paths <- STM.readTVar ws.watchedPathsVar
    case Map.lookup canonPath paths of
      Nothing -> pure Nothing
      Just stopAction -> do
        STM.writeTVar ws.watchedPathsVar (Map.delete canonPath paths)
        pure (Just stopAction)
  case maybeStop of
    Nothing -> pure False
    Just stopAction -> do
      stopAction
      pure True

-- | Stop watching all paths.
unwatchAllPaths :: WatchState -> IO ()
unwatchAllPaths ws = do
  stopActions <- atomically $ do
    paths <- STM.readTVar ws.watchedPathsVar
    STM.writeTVar ws.watchedPathsVar Map.empty
    pure (Map.elems paths)
  sequence_ stopActions

-- | Get the list of currently watched paths.
getWatchedPaths :: WatchState -> IO (Set FilePath)
getWatchedPaths ws = atomically $ Map.keysSet <$> STM.readTVar ws.watchedPathsVar
