{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RankNTypes #-}
module U.Codebase.Sync where

-- localSyncFile
--   (srcPath :: CodebasePath)
--   (destPath :: CodebasePath)
--   (root :: Maybe ShortBranchHash)
--   (path :: UnisonPath)
--   = error "todo"

-- localSyncSql
--   (srcDb :: Connection)
--   (destDb :: Connection)
--   (root :: Maybe ShortBranchHash)
--   (path :: UnisonPath)
--   = error "todo"

-- data Reference t h = Builtin t | Derived h Pos
-- -- |The 0-based index in a definition component/cycle
-- newtype Pos = Pos { unPos :: Word64 }
-- data RefId h = RefId h Pos
-- data TermRef t h = TermRef (Reference t h) | TermCon (Reference t h) ConstructorId
-- newtype ConstructorId = ConstructorId { unConstructorId :: Word64 }
-- data TermRefId h = TermRefId (RefId h) | TermConId (RefId h) ConstructorId

import Data.Foldable (traverse_)

data TrySyncResult h = Missing [h] | Done | PreviouslyDone
  deriving Show

data Sync m h = Sync { trySync :: h -> m (TrySyncResult h) }

transformSync :: (forall a. m a -> n a) -> Sync m h -> Sync n h
transformSync f (Sync t) = Sync (f . t)

data Progress m h = Progress
  { need :: h -> m (),
    done :: h -> m (),
    allDone :: m ()
  }

transformProgress :: (forall a. m a -> n a) -> Progress m h -> Progress n h
transformProgress f (Progress a b c) = Progress (f . a) (f . b) (f c)

sync :: forall m h. Monad m => Sync m h -> Progress m h -> [h] -> m ()
sync Sync {..} Progress {..} roots = go roots
  where
    go :: [h] -> m ()
    go (h : hs) =
      trySync h >>= \case
        Missing deps -> traverse_ need deps >> go (deps ++ h : hs)
        Done -> done h >> go hs
        PreviouslyDone -> go hs
    go [] = allDone