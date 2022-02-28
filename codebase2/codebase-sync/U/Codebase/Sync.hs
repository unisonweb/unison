{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module U.Codebase.Sync where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Debug.Trace (traceM)

debug :: Bool
debug = False

data TrySyncResult h = Missing [h] | Done | PreviouslyDone | NonFatalError
  deriving (Show)

data Sync m h = Sync {trySync :: h -> m (TrySyncResult h)}

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

-- the Show constraint is just for debugging
sync, sync' :: forall m h. (Monad m, Show h) => Sync m h -> Progress m h -> [h] -> m ()

-- | Calls `allDone` at the end
sync s p roots = sync' s p roots >> allDone p

-- | Doesn't call `allDone` at the end, in case you plan to call sync more than once.
sync' Sync {..} Progress {..} roots = go roots
  where
    go :: [h] -> m ()
    go (h : hs) = do
      when debug (traceM $ "Sync.sync.go " ++ (show $ h : hs))
      trySync h >>= \case
        Missing deps -> traverse_ need deps >> go (deps ++ h : hs)
        Done -> done h >> go hs
        PreviouslyDone -> go hs
        NonFatalError -> error h >> go hs
    go [] = pure ()
