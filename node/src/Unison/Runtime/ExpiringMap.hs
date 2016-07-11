module Unison.Runtime.ExpiringMap where

import Control.Concurrent as C
import Control.Concurrent.STM (atomically, retry)
import Control.Monad
import Data.Hashable (Hashable)
import qualified Data.Time.Clock as Clock
import qualified ListT
import qualified STMContainers.Map as M

type ExpiringMap k v = (Seconds, M.Map k (Clock.UTCTime, v))

type Seconds = Clock.NominalDiffTime

new :: (Hashable k, Eq k) => Seconds -> IO (ExpiringMap k v)
new ttl = do
  m <- atomically M.new
  let m' = (ttl,m)
  _ <- C.forkIO . forever $ do
    now <- Clock.getCurrentTime
    expired <- id $
      let
        ensureNonempty s = ListT.uncons s >>= \o -> case o of
          Nothing -> retry -- will kill the thread if nothing else has reference to map
          Just (_,_) -> pure ()
        loop s = atomically (ListT.uncons s) >>= \o -> case o of
          Nothing -> pure []
          Just ((k,(expiration,_)), tl) ->
            if expiration >= now then (k:) <$> loop tl
            else loop tl
      in atomically (ensureNonempty (M.stream m)) >> loop (M.stream m)
    forM_ expired $ \k -> deleteIfExpired k m'
    C.threadDelay (floor ttl * 1000000)
  pure m'

insert :: (Eq k, Hashable k) => k -> v -> ExpiringMap k v -> IO Bool
insert k v (ttl,m) = do
  now <- Clock.getCurrentTime
  expiration <- pure $ Clock.addUTCTime ttl now
  atomically $ do
    curv <- M.lookup k m
    case curv of
      Nothing -> pure True <* M.insert (expiration,v) k m
      Just _ -> pure False

lookup :: (Eq k, Hashable k) => k -> ExpiringMap k v -> IO (Maybe v)
lookup k (ttl,m) = do
  now <- Clock.getCurrentTime
  expiration' <- pure $ Clock.addUTCTime ttl now
  atomically $ do
    v <- M.lookup k m
    case v of
      Nothing -> pure Nothing
      Just (expiration,v) ->
        if expiration >= now then Nothing <$ M.delete k m
        else Just v <$ M.insert (expiration',v) k m

delete :: (Eq k, Hashable k) => k -> ExpiringMap k v -> IO ()
delete k (_,m) = atomically $ M.delete k m

deleteIfExpired :: (Eq k, Hashable k) => k -> ExpiringMap k v -> IO ()
deleteIfExpired k (_,m) = do
  now <- Clock.getCurrentTime
  atomically $ do
    v <- M.lookup k m
    case v of
      Just (expiration,_) | expiration >= now -> M.delete k m
      _ -> pure ()
