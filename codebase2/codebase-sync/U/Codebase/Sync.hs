{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data TrySyncResult h = Missing [h] | Done [h] | PreviouslyDone

data Sync m h = Sync
  { roots :: m [h],
    trySync :: h -> m (TrySyncResult h)
  }

data Progress m h = Progress
  { need :: h -> m (),
    done :: h -> m (),
    allDone :: m ()
  }

sync :: forall m h. Monad m => Sync m h -> Progress m h -> m ()
sync Sync{..} Progress{..} = do roots >>= go where
  go :: [h] -> m ()
  go (h : hs) = trySync h >>= \case
    Missing deps -> traverse_ need deps >> go (deps ++ h : hs)
    Done externalDeps -> done h >> go (externalDeps ++ hs)
    PreviouslyDone -> go hs
  go [] = allDone