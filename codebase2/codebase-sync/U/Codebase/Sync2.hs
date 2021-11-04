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

sync :: forall k m v. (Ord k, MonadIO m) => Store m k v -> ((k -> Sync m v) -> k -> Sync m v) -> k -> m v
sync Store {readStore, writeStore} f k0 =
  runSync do
    let go :: k -> Sync m v
        go k =
          lift (readStore k) >>= \case
            Nothing -> do
              v <- f go k
              lift (writeStore k v)
              pure v
            Just v -> pure v
    go k0

syncD ::
  forall k m v.
  (GCompare k, MonadIO m) =>
  StoreD m k ->
  ((forall v. k v -> Sync m v) -> (forall v. k v -> Sync m v)) ->
  k v ->
  m v
syncD StoreD {readStoreD, writeStoreD} f k0 =
  runSync do
    let go :: forall v. k v -> Sync m v
        go k =
          lift (readStoreD k) >>= \case
            Nothing -> do
              v <- f go k
              lift (writeStoreD k v)
              pure v
            Just v -> pure v
    go k0

data Store m k v = Store
  { readStore :: k -> m (Maybe v),
    writeStore :: k -> v -> m ()
  }

data StoreD m k = StoreD
  { readStoreD :: forall v. k v -> m (Maybe v),
    writeStoreD :: forall v. k v -> v -> m ()
  }

data ThreadSafety
  = NotThreadSafe

makeMapStore :: forall k m v. (MonadIO m, Ord k) => ThreadSafety -> m (Store m k v)
makeMapStore NotThreadSafe = do
  storeRef <- liftIO (newIORef Map.empty)
  let readStore :: k -> m (Maybe v)
      readStore k = do
        store <- liftIO (readIORef storeRef)
        pure (Map.lookup k store)
  let writeStore :: k -> v -> m ()
      writeStore k v =
        liftIO (modifyIORef' storeRef (Map.insert k v))
  pure Store {readStore, writeStore}

makeDependentMapStore :: forall k m. (GCompare k, MonadIO m) => ThreadSafety -> m (StoreD m k)
makeDependentMapStore NotThreadSafe = do
  storeRef <- liftIO (newIORef Dependent.Map.empty)
  let readStoreD :: k v -> m (Maybe v)
      readStoreD k = do
        store <- liftIO (readIORef storeRef)
        pure (runIdentity <$> Dependent.Map.lookup k store)
  let writeStoreD :: k v -> v -> m ()
      writeStoreD k v =
        liftIO (modifyIORef' storeRef (Dependent.Map.insert k (Identity v)))
  pure StoreD {readStoreD, writeStoreD}
