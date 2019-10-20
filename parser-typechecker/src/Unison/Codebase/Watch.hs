{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Watch where

import Unison.Prelude

import           UnliftIO.Concurrent            ( forkIO
                                                , threadDelay
                                                , killThread
                                                )
import           UnliftIO                       ( MonadUnliftIO
                                                , withRunInIO )
import           UnliftIO.Directory             ( getModificationTime
                                                , listDirectory
                                                )
import           UnliftIO.MVar                  ( newEmptyMVar, takeMVar
                                                , tryTakeMVar, putMVar )
import           UnliftIO.STM                   ( atomically )
import           UnliftIO.Exception             ( catch, IOException)
import           UnliftIO.IORef                 ( newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Data.List                      ( sortOn )
import qualified Data.Map                      as Map
import qualified Data.Text.IO
import           Data.Time.Clock                ( UTCTime
                                                , diffUTCTime
                                                )
import           System.FSNotify                ( Event(Added, Modified)
                                                , watchDir
                                                , withManager
                                                )
import           Unison.Util.TQueue             ( TQueue )
import qualified Unison.Util.TQueue            as TQueue

untilJust :: Monad m => m (Maybe a) -> m a
untilJust act = act >>= maybe (untilJust act) return

watchDirectory'
  :: forall m. MonadUnliftIO m => FilePath -> m (m (), m (FilePath, UTCTime))
watchDirectory' d = do
    mvar <- newEmptyMVar
    let handler :: Event -> m ()
        handler e = case e of
          Added    fp t False -> doIt fp t
          Modified fp t False -> doIt fp t
          _                   -> pure ()
          where doIt fp t = do
                  _ <- tryTakeMVar mvar
                  putMVar mvar (fp, t)
    -- janky: used to store the cancellation action returned
    -- by `watchDir`, which is created asynchronously
    cleanupRef <- newEmptyMVar
    cancel <- forkIO $ withRunInIO $ \inIO ->
      withManager $ \mgr -> do
        cancelInner <- watchDir mgr d (const True) (inIO . handler) <|> (pure (pure ()))
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

watchDirectory :: forall m. MonadUnliftIO m
  => FilePath -> (FilePath -> Bool) -> m (m (), m (FilePath, Text))
watchDirectory dir allow = do
  previousFiles <- newIORef Map.empty
  (cancel, watcher) <- watchDirectory' dir
  let
    existingFiles :: MonadIO m => m [(FilePath, UTCTime)]
    existingFiles = do
      files <- listDirectory dir
      let withTime file = (file,) <$> getModificationTime file
      sortOn snd <$> mapM withTime files
    process :: MonadIO m => FilePath -> UTCTime -> m (Maybe (FilePath, Text))
    process file t =
      if allow file then let
        handle :: IOException -> m ()
        handle e = do
          liftIO $ putStrLn $ "‼  Got an exception while reading: " <> file
          liftIO $ print (e :: IOException)
        go :: MonadUnliftIO m => m (Maybe (FilePath, Text))
        go = liftIO $ do
          contents <- Data.Text.IO.readFile file
          prevs    <- readIORef previousFiles
          case Map.lookup file prevs of
            -- if the file's content's haven't changed and less than a second has passed,
            -- wait for the next update
            Just (contents0, t0)
              | contents == contents0 && (t `diffUTCTime` t0) < 1 ->
                return Nothing
            _ ->
              Just (file, contents) <$
                writeIORef previousFiles (Map.insert file (contents, t) prevs)
        in catch go (\e -> Nothing <$ handle e)
      else return Nothing
  pending <- newIORef =<< existingFiles
  let
    await :: MonadIO m => m (FilePath, Text)
    await = untilJust $ readIORef pending >>= \case
      [] -> uncurry process =<< watcher
      ((file, t):rest) -> do
        writeIORef pending rest
        process file t
  pure (cancel, await)
