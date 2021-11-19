module U.Codebase.Sync2
  ( -- * Sync monad
    Sync (..),
    runSync,

    -- * Sync action
    sync,
    Store (..),
    makeMapStore,

    -- * Dependent sync action
    syncD,
    StoreD (..),
    makeDependentMapStore,

    -- * Misc.
    ThreadSafety (..),
  )
where

import Control.Concurrent.MVar
import qualified Data.Dependent.Map as Dependent.Map
import Data.Functor.Identity
import Data.GADT.Compare (GCompare)
import Data.IORef
import qualified Data.Map.Strict as Map
import Unison.Prelude
import Prelude hiding (lookup)

newtype Sync m a = Sync
  { unSync :: forall r. (a -> m r) -> m r
  }

instance Functor (Sync m) where
  fmap f (Sync m) =
    Sync (\k -> m (k . f))

instance Applicative (Sync m) where
  pure x = Sync \k -> k x
  (<*>) = ap

instance Monad (Sync m) where
  return = pure
  Sync m >>= f =
    Sync \k ->
      m \a -> unSync (f a) k

instance MonadIO m => MonadIO (Sync m) where
  liftIO action =
    Sync (liftIO action >>=)

instance MonadTrans Sync where
  lift action =
    Sync (action >>=)

runSync :: Applicative m => Sync m a -> m a
runSync (Sync action) =
  action pure

sync ::
  forall k m v.
  (Ord k, MonadIO m) =>
  Store k v ->
  ((k -> Sync m v) -> k -> Sync m v) ->
  k ->
  m v
sync (Store fetch) f =
  let go :: k -> Sync m v
      go k = fetch k (f go k)
   in runSync . go

syncD ::
  forall k m v.
  (GCompare k, MonadIO m) =>
  StoreD k ->
  ((forall v. k v -> Sync m v) -> (forall v. k v -> Sync m v)) ->
  k v ->
  m v
syncD (StoreD fetch) f =
  let go :: forall v. k v -> Sync m v
      go k = fetch k (f go k)
   in runSync . go

newtype Store k v
  = Store (forall m. MonadIO m => k -> m v -> m v)

newtype StoreD k
  = StoreD (forall m v. MonadIO m => k v -> m v -> m v)

data ThreadSafety
  = NotThreadSafe
  | ThreadSafe

makeMapStore :: forall k m v. (MonadIO m, Ord k) => ThreadSafety -> m (Store k v)
makeMapStore = \case
  NotThreadSafe -> do
    storeRef <- liftIO (newIORef Map.empty)
    let fetch :: forall m. MonadIO m => k -> m v -> m v
        fetch k mv = do
          store <- liftIO (readIORef storeRef)
          case Map.lookup k store of
            Nothing -> do
              v <- mv
              liftIO (writeIORef storeRef $! Map.insert k v store)
              pure v
            Just v -> pure v
    pure (Store fetch)
  ThreadSafe -> do
    storeVar <- liftIO (newMVar Map.empty)
    let fetch :: forall m. MonadIO m => k -> m v -> m v
        fetch k mv = do
          store0 <- liftIO (readMVar storeVar)
          case Map.lookup k store0 of
            Nothing -> do
              store0 <- liftIO (takeMVar storeVar)
              case Map.lookup k store0 of
                Nothing -> do
                  vvar <- liftIO newEmptyMVar
                  liftIO (putMVar storeVar $! Map.insert k vvar store0)
                  v <- mv
                  liftIO (putMVar vvar v)
                  pure v
                Just vvar -> liftIO do
                  putMVar storeVar store0
                  readMVar vvar
            Just v -> liftIO (readMVar v)
    pure (Store fetch)

makeDependentMapStore :: forall k m. (GCompare k, MonadIO m) => ThreadSafety -> m (StoreD k)
makeDependentMapStore = \case
  NotThreadSafe -> do
    storeRef <- liftIO (newIORef Dependent.Map.empty)
    let fetch :: forall m v. MonadIO m => k v -> m v -> m v
        fetch k mv = do
          store <- liftIO (readIORef storeRef)
          case Dependent.Map.lookup k store of
            Nothing -> do
              v <- mv
              liftIO (writeIORef storeRef $! Dependent.Map.insert k (Identity v) store)
              pure v
            Just (Identity v) -> pure v
    pure (StoreD fetch)
  ThreadSafe -> do
    storeVar <- liftIO (newMVar Dependent.Map.empty)
    let fetch :: forall m v. MonadIO m => k v -> m v -> m v
        fetch k mv = do
          store0 <- liftIO (readMVar storeVar)
          case Dependent.Map.lookup k store0 of
            Nothing -> do
              store0 <- liftIO (takeMVar storeVar)
              case Dependent.Map.lookup k store0 of
                Nothing -> do
                  vvar <- liftIO newEmptyMVar
                  liftIO (putMVar storeVar $! Dependent.Map.insert k vvar store0)
                  v <- mv
                  liftIO (putMVar vvar v)
                  pure v
                Just vvar -> liftIO do
                  putMVar storeVar store0
                  readMVar vvar
            Just v -> liftIO (readMVar v)
    pure (StoreD fetch)
