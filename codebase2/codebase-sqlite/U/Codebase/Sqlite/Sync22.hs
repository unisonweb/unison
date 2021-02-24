{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module U.Codebase.Sqlite.Sync22 where

import Control.Monad (filterM, (<=<))
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Extra (ifM)
import Control.Monad.RWS (MonadIO, MonadReader (reader))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Except (withExceptT)
import Data.Functor ((<&>))
import Data.Word (Word64)
import Database.SQLite.Simple (Connection)
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sync
import qualified U.Codebase.Sync as Sync
import U.Util.Cache (Cache)
import qualified U.Util.Cache as Cache
import Data.Foldable (toList)

data Entity = O ObjectId | C CausalHashId

data Error = SrcDB Q.Integrity | DestDB Q.Integrity

data Env = Env
  { srcDB :: Connection,
    destDB :: Connection
  }

-- data Mappings

-- We load an object from the source; it has a bunch of dependencies.
-- Some of these dependencies exist at the defination, some don't.
-- For the ones that do, look up their ids, and update the thingie as you write it
-- For the ones that don't, copy them (then you will know their ids), and update the thingie.
-- If you want, you can try to cache that lookup.

-- sync22 ::
--   ( MonadIO m,
--     MonadError Error m,
--     MonadReader TwoConnections m
--   ) =>
--   Sync m Entity
-- sync22 = Sync roots trySync
--   where
--     roots = runSrc $ fmap (\h -> [C h]) Q.loadNamespaceRoot

trySync ::
  (MonadIO m, MonadError Error m, MonadReader Env m) =>
  Cache m TextId TextId ->
  Cache m HashId HashId ->
  Cache m CausalHashId CausalHashId ->
  Cache m ObjectId ObjectId ->
  Generation ->
  Entity ->
  m (TrySyncResult Entity)
trySync tCache hCache cCache oCache gc = \case
  -- for causals, we need to get the value_hash_id of the thingo
  -- - maybe enqueue their parents
  -- - enqueue the self_ and value_ hashes
  -- - enqueue the namespace object, if present
  C chId -> do
    chId' <- syncCausalHash chId
    -- we're going to assume that if the dest has this in its
    -- causal table, then it's safe to short-circuit
    ifM
      (runDest $ Q.isCausalHash chId')
      (pure Sync.PreviouslyDone)
      ( do
          missingParents <- (fmap . fmap) C $
            runSrc (Q.loadCausalParents chId)
              >>= filterM (\p -> syncCausalHash p >>= runDest . Q.isCausalHash)
          -- optionally get branch object id for this causal
          mayBoId <- (fmap . fmap) O $
            runSrc (Q.loadCausalValueHashId chId)
            >>= runSrc . Q.maybeObjectIdForAnyHashId . unBranchHashId
          -- the parents should probably be real like CausalIds, that "statically"
          -- know they're in that Causal table; not just reuse the hash id.
          -- that would make the missingParents real dependencies instead of weakrefs
          pure (Sync.Done (toList mayBoId ++ toList missingParents))
      )

  -- objects are the hairiest. obviously, if they
  -- exist, we're done; otherwise we do some fancy stuff
  O oId -> error "todo"
  -- O oId -> Cache.lookup oCache oId >>= \case
  --   Just{} -> pure Sync.PreviouslyDone
  --   Nothing -> do

  --     error "todo"
  --     pure Sync.Done
  where
    syncTextLiteral = Cache.apply tCache \tId -> do
      t <- runSrc $ Q.loadTextById tId
      runDest $ Q.saveText t

    syncHashLiteral = Cache.apply hCache \hId -> do
      b32hex <- runSrc $ Q.loadHashById hId
      runDest $ Q.saveHash b32hex

    syncCausalHash = syncHashLiteral . unCausalHashId
    syncBranchHashId = fmap BranchHashId . syncHashLiteral . unBranchHashId

    syncObject = error "todo"

-- syncCausal chId = do
--   value

-- Q: Do we want to cache corresponding ID mappings?
-- A: Yes, but not yet

runSrc ::
  (MonadError Error m, MonadReader Env m) =>
  ReaderT Connection (ExceptT Q.Integrity m) a ->
  m a
runSrc = error "todo" -- withExceptT SrcDB . (reader fst >>=)

runDest ::
  (MonadError Error m, MonadReader Env m) =>
  ReaderT Connection (ExceptT Q.Integrity m) a ->
  m a
runDest = error "todo" -- withExceptT SrcDB . (reader fst >>=)

-- syncs coming from git:
--  - pull a specified remote causal (Maybe CausalHash) into the local database
--  - and then maybe do some stuff
-- syncs coming from