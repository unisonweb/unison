{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module U.Codebase.Sync.V2V2 where

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
import Control.Monad.Extra (ifM)
import Data.Foldable (Foldable(foldl'))
data TypeDependency y = YType y
data TermDependency e y = ETerm e | EType y
data PatchDependency e y = PTerm e | PType y
data BranchDependency b e y p = Branch b | BTerm e | BType y | BPatch p

data Sync m b e y p = Sync
  { rootBranch :: m b
  , termMissingDependencies :: e -> m [TermDependency e y]
  , typeMissingDependencies :: y -> m [TypeDependency y]
  , patchMissingDependencies :: p -> m [PatchDependency e y]
  , branchMissingDependencies :: b -> m [BranchDependency b e y p]
  -- returns True if it does some real work, False if it skips / short circuits
  -- It should be expected that these functions can be called multiple times
  -- for the same arguments.
  , syncTerm :: e -> m Bool
  , syncType :: y -> m Bool
  , syncPatch :: p -> m Bool
  , syncBranch :: b -> m Bool
  }

-- | Progress callbacks.
-- There's no notion of "work remaining" captured here, because that would require
-- this algorithm to keep dependencies in memory, which may be intractable.
-- An implementation, however, can use the `need*` callbacks to track this in `m`.
data Progress m b e y p = Progress
  { needBranch :: b -> m ()
  , needTerm :: e -> m ()
  , needType :: y -> m ()
  , needPatch :: p -> m ()
  , doneBranch :: b -> m ()
  , doneTerm :: e -> m ()
  , doneType :: y -> m ()
  , donePatch :: p -> m ()
  , allDone :: m ()
  }

sync :: forall m b e y p. Monad m => Sync m b e y p -> Progress m b e y p -> m ()
sync Sync{..} Progress{..} = do b <- rootBranch; go ([], [], [], [b]) where
  go :: ([y],[e],[p],[b]) -> m ()
  go (y : ys, es, ps, bs) =
    typeMissingDependencies y >>= \case
      [] -> ifM (syncType y) (doneType y) (go (ys, es, ps, bs))
      ydeps -> do
        let ys' = [y | YType y <- ydeps]
        traverse_ needType ys'
        go (ys' ++ y : ys, es, ps, bs)

  go ([], (e : es), ps, bs) =
    termMissingDependencies e >>= \case
      [] -> ifM (syncTerm e) (doneTerm e) (go ([], es, ps, bs))
      edeps -> do
        let (ys', es') = foldl' partitionTermDeps mempty edeps
        traverse_ needType ys'
        traverse_ needTerm es'
        go (ys', es' ++ e : es, ps, bs)

  go ([], [], (p : ps), bs) =
    patchMissingDependencies p >>= \case
      [] -> ifM (syncPatch p) (donePatch p) (go ([], [], ps, bs))
      pdeps -> do
        let (ys', es') = foldl' partitionPatchDeps mempty pdeps
        traverse_ needType ys'
        traverse_ needTerm es'
        go (ys', es', p : ps, bs)

  go ([], [], [], (b : bs)) = error "todo"
    branchMissingDependencies b >>= \case
      [] -> ifM (syncBranch b) (doneBranch b) (go ([], [], [], bs))
      bdeps -> do
        let (ys', es', ps', bs') = foldl' partitionBranchDeps mempty bdeps
        traverse_ needType ys'
        traverse_ needTerm es'
        traverse_ needPatch ps'
        traverse_ needBranch bs'
        go (ys', es', ps', bs' ++ b : bs)

  go ([], [], [], []) = allDone

  partitionTermDeps (ys, es) = \case
    EType y -> (y : ys, es)
    ETerm e -> (ys, e : es)

  partitionPatchDeps (ys, es) = \case
    PType y -> (y : ys, es)
    PTerm e -> (ys, e : es)

  partitionBranchDeps (ys, es, ps, bs) = \case
    BType y -> (y : ys, es, ps, bs)
    BTerm e -> (ys, e : es, ps, bs)
    BPatch p -> (ys, es, p :ps, bs)
    Branch b -> (ys, es, ps, b : bs)
