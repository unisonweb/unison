{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Watch where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import qualified Data.Text.IO
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
  )
import System.FSNotify (Event (Added, Modified))
import qualified System.FSNotify as FSNotify
import Unison.Prelude
import Unison.Util.TQueue (TQueue)
import qualified Unison.Util.TQueue as TQueue
import UnliftIO
  ( MonadUnliftIO,
    unliftIO,
    withRunInIO,
  )
import qualified UnliftIO as UnliftIO
import UnliftIO.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
  )
import UnliftIO.Directory
  ( doesPathExist,
    getModificationTime,
    listDirectory,
  )
import UnliftIO.Exception (catch)
import UnliftIO.IORef
  ( newIORef,
    readIORef,
    writeIORef,
  )
import UnliftIO.MVar
  ( newEmptyMVar,
    putMVar,
    takeMVar,
    tryPutMVar,
    tryTakeMVar,
  )
import UnliftIO.STM (atomically)

untilJust :: Monad m => m (Maybe a) -> m a
untilJust act = act >>= maybe (untilJust act) return

watchDirectory' ::
  forall m. MonadUnliftIO m => FilePath -> m (m (), m (FilePath, UTCTime))
watchDirectory' d = do
  mvar <- newEmptyMVar
  let handler :: Event -> m ()
      handler e = case e of
        Added fp t False -> doIt fp t
        Modified fp t False -> doIt fp t
        _ -> pure ()
        where
          doIt fp t = do
            _ <- tryTakeMVar mvar
            putMVar mvar (fp, t)
  -- janky: used to store the cancellation action returned
  -- by `watchDir`, which is created asynchronously
  cleanupRef <- newEmptyMVar
  -- we don't like FSNotify's debouncing (it seems to drop later events)
  -- so we will be doing our own instead
  let config = FSNotify.defaultConfig {FSNotify.confDebounce = FSNotify.NoDebounce}
  cancel <- forkIO $
    withRunInIO $ \inIO ->
      FSNotify.withManagerConf config $ \mgr -> do
        cancelInner <- FSNotify.watchDir mgr d (const True) (inIO . handler) <|> (pure (pure ()))
        putMVar cleanupRef $ liftIO cancelInner
        forever $ threadDelay 1000000
  let cleanup :: m ()
      cleanup = join (takeMVar cleanupRef) >> killThread cancel
  pure (cleanup, takeMVar mvar)

collectUntilPause :: forall m a. MonadIO m => TQueue a -> Int -> m [a]
collectUntilPause queue minPauseµsec = do
  -- 1. wait for at least one element in the queue
  void . atomically $ TQueue.peek queue

  let go :: MonadIO m => m [a]
      go = do
        before <- atomically $ TQueue.enqueueCount queue
        threadDelay minPauseµsec
        after <- atomically $ TQueue.enqueueCount queue
        -- if nothing new is on the queue, then return the contents
        if before == after
          then atomically $ TQueue.flush queue
          else go
  go

watchDirectory ::
  forall m.
  MonadUnliftIO m =>
  FilePath ->
  (FilePath -> Bool) ->
  m (m (), m (FilePath, Text))
watchDirectory dir allow = do
  previousFiles <- newIORef Map.empty
  (cancelWatch, watcher) <- watchDirectory' dir
  let existingFiles :: MonadIO m => m [(FilePath, UTCTime)]
      existingFiles = do
        files <- listDirectory dir
        filtered <- filterM doesPathExist files
        let withTime file = (file,) <$> getModificationTime file
        sortOn snd <$> mapM withTime filtered
      process :: MonadIO m => FilePath -> UTCTime -> m (Maybe (FilePath, Text))
      process file t =
        if allow file
          then
            let handle :: IOException -> m ()
                handle e = do
                  liftIO $ putStrLn $ "‼  Got an exception while reading: " <> file
                  liftIO $ print (e :: IOException)
                go :: MonadUnliftIO m => m (Maybe (FilePath, Text))
                go = liftIO $ do
                  contents <- Data.Text.IO.readFile file
                  prevs <- readIORef previousFiles
                  case Map.lookup file prevs of
                    -- if the file's content's haven't changed and less than .5s
                    -- have elapsed, wait for the next update
                    Just (contents0, t0)
                      | contents == contents0 && (t `diffUTCTime` t0) < 0.5 ->
                        return Nothing
                    _ ->
                      Just (file, contents)
                        <$ writeIORef previousFiles (Map.insert file (contents, t) prevs)
             in catch go (\e -> Nothing <$ handle e)
          else return Nothing
  queue <- TQueue.newIO
  gate <- liftIO newEmptyMVar
  ctx <- UnliftIO.askUnliftIO
  -- We spawn a separate thread to siphon the file change events
  -- into a queue, which can be debounced using `collectUntilPause`
  enqueuer <- liftIO . forkIO $ do
    takeMVar gate -- wait until gate open before starting
    forever $ do
      event@(file, _) <- UnliftIO.unliftIO ctx watcher
      when (allow file) $
        STM.atomically $ TQueue.enqueue queue event
  pending <- newIORef =<< existingFiles
  let await :: MonadIO m => m (FilePath, Text)
      await =
        untilJust $
          readIORef pending >>= \case
            [] -> do
              -- open the gate
              tryPutMVar gate ()
              -- this debounces the events, waits for 50ms pause
              -- in file change events
              events <- collectUntilPause queue 50000
              -- traceM $ "Collected file change events" <> show events
              case events of
                [] -> pure Nothing
                -- we pick the last of the events within the 50ms window
                -- TODO: consider enqueing other events if there are
                -- multiple events for different files
                _ -> uncurry process $ last events
            ((file, t) : rest) -> do
              writeIORef pending rest
              process file t
      cancel = cancelWatch >> killThread enqueuer
  pure (cancel, await)
