module Unison.Runtime.ExpiringMap where

import Data.Functor
import Control.Concurrent as C
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TSem
import qualified STMContainers.Map as M
import Data.Hashable (Hashable)

type ExpiringMap k v = M.Map k (TSem, v)
type Microseconds = Int

new :: IO (ExpiringMap k v)
new = atomically M.new

insert :: (Eq k, Hashable k) => Microseconds -> k -> v -> ExpiringMap k v -> IO ()
insert ttl k v m = do
  sem' <- atomically $ newTSem 1
  o <- atomically $ M.lookup k m <* M.insert (sem',v) k m
  case o of
    Nothing -> pure ()
    Just (sem,_) -> atomically $ waitTSem sem `orElse` pure ()
  _ <- C.forkIO $ do
    C.threadDelay ttl
    atomically $ do
      waitTSem sem' -- will die if key gets reused before expiration
      M.delete k m
  pure ()

lookup :: (Eq k, Hashable k) => Microseconds -> k -> ExpiringMap k v -> IO (Maybe v)
lookup ttl k m = do
  o <- atomically $ M.lookup k m
  action <- case o of
    Nothing -> pure (pure ())
    Just (sem,_) -> do
      sem' <- atomically $ newTSem 1
      atomically $ waitTSem sem `orElse` pure ()
      pure . void . C.forkIO $ do
        C.threadDelay ttl
        atomically $ do
          waitTSem sem' -- will die if key gets reused before expiration
          M.delete k m
  (snd <$> o) <$ action

delete :: (Eq k, Hashable k) => k -> ExpiringMap k v -> IO ()
delete k m = do
  o <- atomically $ M.lookup k m <* M.delete k m
  case o of
    Nothing -> pure ()
    Just (sem, _) -> atomically $ waitTSem sem `orElse` pure ()
