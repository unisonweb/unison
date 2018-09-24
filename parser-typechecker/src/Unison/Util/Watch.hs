{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Unison.Util.Watch where

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (forever)
import           System.FSNotify

watch :: FilePath -> IO (IO FilePath)
watch d = do
  mvar <- newEmptyMVar
  let doIt fp = do
        _ <- tryTakeMVar mvar
        putMVar mvar fp
      handler e = case e of
                Added fp _ False -> doIt fp
                Modified fp _ False -> doIt fp
                _ -> pure ()
  _ <- forkIO $ withManager $ \mgr -> do
    _ <- watchDir mgr d (const True) handler
    forever $ threadDelay 1000000
  pure $ takeMVar mvar

main :: IO ()
main = do
  d <- watch "."
  forever $ do
    fp <- d
    putStrLn $ show fp

