{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module U.Codebase.Sqlite.Sync22 where

import Control.Monad (filterM, join, (<=<))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Extra (ifM)
import Control.Monad.RWS (MonadIO, MonadReader (reader))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Except (withExceptT)
import Data.Foldable (toList, traverse_)
import Data.Functor ((<&>))
import Data.List.Extra (nubOrd)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Word (Word64)
import Database.SQLite.Simple (Connection)
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sync
import qualified U.Codebase.Sync as Sync
import U.Util.Cache (Cache)
import qualified U.Util.Cache as Cache

data Entity = O ObjectId | C CausalHashId

data Error
  = SrcDB Q.Integrity
  | DestDB Q.Integrity
  | -- | hashes corresponding to a single object in source codebase
    --  correspond to multiple objects in destination codebase
    HashObjectCorrespondence ObjectId [HashId] [ObjectId]

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
  forall m.
  (MonadIO m, MonadError Error m, MonadReader Env m) =>
  Cache m TextId TextId ->
  Cache m HashId HashId ->
  Cache m ObjectId ObjectId ->
  Generation ->
  Entity ->
  m (TrySyncResult Entity)
trySync tCache hCache oCache gc = \case
  -- for causals, we need to get the value_hash_id of the thingo
  -- - maybe enqueue their parents
  -- - enqueue the self_ and value_ hashes
  -- - enqueue the namespace object, if present
  C chId -> do
    chId' <- syncCausalHash chId
    -- we're going to assume that if the dest has this in its
    -- causal table, then it's safe to short-circuit
    ifM
      (runDest $ Q.isCausalHash $ unCausalHashId chId')
      (pure Sync.PreviouslyDone)
      ( do
          bhId <- runSrc $ Q.loadCausalValueHashId chId
          bhId' <- syncBranchHashId bhId

          mayBoId <-
            runSrc . Q.maybeObjectIdForAnyHashId $
              unBranchHashId bhId
          mayBoId' <- join <$> traverse (isSyncedObject) mayBoId

          findMissingParents chId >>= \case
            [] ->
              -- if branch object is present at src and dest,
              -- or absent from both src and dest
              -- then we are done
              if isJust mayBoId == isJust mayBoId'
                then do
                  runDest $ Q.saveCausal chId' bhId'
                  pure Sync.Done
                else -- else it's present at src but not at dest.,
                -- so request it be copied, and revisit later
                  pure $ Missing [O $ fromJust mayBoId]
            missingParents ->
              -- if branch object is present at src and dest,
              -- or absent from both src and dest
              -- but there are parents missing,
              -- then request the parents be copied, and revisit later
              if isJust mayBoId == isJust mayBoId'
                then pure $ Missing missingParents
                else -- otherwise request the branch and the parents be copied
                  pure $ Missing $ (O $ fromJust mayBoId) : missingParents
      )
  -- objects are the hairiest. obviously, if they
  -- exist, we're done; otherwise we do some fancy stuff
  O oId ->
    isSyncedObject oId >>= \case
      Just {} -> pure Sync.PreviouslyDone
      Nothing -> do
        (hId, objType, bytes) <- runSrc $ Q.loadObjectWithHashIdAndTypeById oId
        hId' <- syncHashLiteral hId
        bytes' <- case objType of
          OT.TermComponent -> error "todo"
          OT.DeclComponent -> error "todo"
          OT.Namespace -> error "todo"
          OT.Patch -> error "todo"
        oId' <- runDest $ Q.saveObject hId' objType bytes'
        syncSecondaryHashes oId oId'
        Cache.insert oCache oId oId'
        pure Sync.Done
  where
    syncTextLiteral :: TextId -> m TextId
    syncTextLiteral = Cache.apply tCache \tId -> do
      t <- runSrc $ Q.loadTextById tId
      runDest $ Q.saveText t

    syncHashLiteral :: HashId -> m HashId
    syncHashLiteral = Cache.apply hCache \hId -> do
      b32hex <- runSrc $ Q.loadHashById hId
      runDest $ Q.saveHash b32hex

    syncCausalHash :: CausalHashId -> m CausalHashId
    syncCausalHash = fmap CausalHashId . syncHashLiteral . unCausalHashId

    syncBranchHashId :: BranchHashId -> m BranchHashId
    syncBranchHashId = fmap BranchHashId . syncHashLiteral . unBranchHashId

    findMissingParents :: CausalHashId -> m [Entity]
    findMissingParents chId = do
      runSrc (Q.loadCausalParents chId)
        >>= filterM isMissing
        <&> fmap C
      where
        isMissing p =
          syncCausalHash p
            >>= runDest . Q.isCausalHash . unCausalHashId

    syncSecondaryHashes oId oId' =
      runSrc (Q.hashIdWithVersionForObject oId) >>= traverse_ (go oId')
      where
        go oId' (hId, hashVersion) = do
          hId' <- syncHashLiteral hId
          runDest $ Q.saveHashObject hId' oId' hashVersion

    isSyncedObject :: ObjectId -> m (Maybe ObjectId)
    isSyncedObject = Cache.applyDefined oCache \oId -> do
      hIds <- toList <$> runSrc (Q.hashIdsForObject oId)
      ( nubOrd . catMaybes
          <$> traverse (runDest . Q.maybeObjectIdForAnyHashId) hIds
        )
        >>= \case
          [oId'] -> pure $ Just oId'
          [] -> pure $ Nothing
          oIds' -> throwError (HashObjectCorrespondence oId hIds oIds')

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

-- applyDefined

-- syncs coming from git:
--  - pull a specified remote causal (Maybe CausalHash) into the local database
--  - and then maybe do some stuff
-- syncs coming from