{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RankNTypes #-}
module U.Codebase.Sync where

import Data.Foldable (traverse_)

data TrySyncResult h = Missing [h] | Done | PreviouslyDone | NonFatalError
  deriving Show

data Sync m h = Sync { trySync :: h -> m (TrySyncResult h) }

transformSync :: (forall a. m a -> n a) -> Sync m h -> Sync n h
transformSync f (Sync t) = Sync (f . t)

data Progress m h = Progress
  { need :: h -> m (),
    done :: h -> m (),
    error :: h -> m (),
    allDone :: m ()
  }

transformProgress :: (forall a. m a -> n a) -> Progress m h -> Progress n h
transformProgress f (Progress a b c d) = Progress (f . a) (f . b) (f . c) (f d)

sync :: forall m h. Monad m => Sync m h -> Progress m h -> [h] -> m ()
sync Sync {..} Progress {..} roots = go roots
  where
    go :: [h] -> m ()
    go (h : hs) =
      trySync h >>= \case
        Missing deps -> traverse_ need deps >> go (deps ++ h : hs)
        Done -> done h >> go hs
        PreviouslyDone -> go hs
        NonFatalError -> error h >> go hs
    go [] = allDone