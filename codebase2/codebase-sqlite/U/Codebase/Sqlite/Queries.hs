{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeOperators #-}
module U.Codebase.Sqlite.Queries (
  -- * Constraint kinds
  DB, EDB, Err,

  -- * Error types
  Integrity(..),

  -- * text table
  saveText,
  loadText,
  loadTextById,

  -- * hash table
  saveHash,
  saveHashHash,
  loadHashId,
  loadHashById,
  loadHashHashById,
  loadHashIdByHash,
  expectHashIdByHash,
  saveCausalHash,
  loadCausalHash,
  saveBranchHash,

  -- * hash_object table
  saveHashObject,
  hashIdsForObject,
  hashIdWithVersionForObject,
  expectObjectIdForPrimaryHashId,
  expectObjectIdForAnyHashId,
  maybeObjectIdForPrimaryHashId,
  maybeObjectIdForAnyHashId,
  recordObjectRehash,

  -- * object table
  saveObject,
  loadObjectById,
  loadPrimaryHashByObjectId,
  loadObjectWithTypeById,
  loadObjectWithHashIdAndTypeById,
  updateObjectBlob, -- unused

  -- * namespace_root table
  loadMaybeNamespaceRoot,
  setNamespaceRoot,
  loadNamespaceRoot,

  -- * causals
  -- ** causal table
  saveCausal,
  isCausalHash,
  loadCausalHashIdByCausalHash,
  loadCausalValueHashId,
  loadCausalByCausalHash,
  loadBranchObjectIdByCausalHashId,

  -- ** causal_parent table
  saveCausalParents,
  loadCausalParents,
  before,
  lca,

  -- * watch table
  saveWatch,
  loadWatch,
  loadWatchesByWatchKind,
  loadWatchKindsByReference,
  clearWatches,

  -- * indexes
  -- ** dependents index
  addToDependentsIndex,
  getDependentsForDependency,
  getDependentsForDependencyComponent,
  getDependenciesForDependent,
  getDependencyIdsForDependent,
  -- ** type index
  addToTypeIndex,
  getReferentsByType,
  getTypeReferenceForReferent,
  getTypeReferencesForComponent,
  -- ** type mentions index
  addToTypeMentionsIndex,
  getReferentsByTypeMention,
  getTypeMentionsReferencesForComponent,

  -- * hash prefix lookup
  objectIdByBase32Prefix,
  namespaceHashIdByBase32Prefix,
  causalHashIdByBase32Prefix,

  -- * garbage collection
  vacuum,
  garbageCollectObjectsWithoutHashes,
  garbageCollectWatchesWithoutObjects,

  -- migrations
  countObjects,
  countCausals,
  countWatches,
  getCausalsWithoutBranchObjects,
  removeHashObjectsByHashingVersion,
  addHashObjectConstraintToCausalTable,

  -- * db misc
  createSchema,
  schemaVersion,
  setSchemaVersion,
  setFlags,

  DataVersion,
  dataVersion,

  savepoint,
  release,
  rollbackRelease,
  rollbackTo,
  withSavepoint,
  withSavepoint_,

  vacuumInto,
  setJournalMode,
  traceConnectionFile,
) where

import qualified Control.Exception as Exception
import Control.Monad (when)
import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (ask))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Control.Monad.Writer as Writer
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Int (Int8)
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.String.Here.Uninterpolated (here, hereFile)
import Data.Text (Text)
import Database.SQLite.Simple
  ( FromRow,
    Only (..),
    ToRow (..),
    (:.) (..),
  )
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Debug.Trace (trace, traceM)
import GHC.Stack (HasCallStack)
import Safe (headMay)
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference (Reference' (..))
import qualified U.Codebase.Reference as C.Reference
import U.Codebase.Sqlite.Connection (Connection)
import qualified U.Codebase.Sqlite.Connection as Connection
import U.Codebase.Sqlite.DbId
  ( BranchHashId (..),
    BranchObjectId (..),
    CausalHashId (..),
    HashId (..),
    ObjectId (..),
    SchemaVersion,
    TextId, HashVersion
  )
import U.Codebase.Sqlite.JournalMode (JournalMode)
import qualified U.Codebase.Sqlite.JournalMode as JournalMode
import U.Codebase.Sqlite.ObjectType (ObjectType)
import qualified U.Codebase.Sqlite.Reference as Reference
import qualified U.Codebase.Sqlite.Referent as Referent
import U.Codebase.WatchKind (WatchKind)
import qualified U.Codebase.WatchKind as WatchKind
import qualified U.Util.Alternative as Alternative
import U.Util.Base32Hex (Base32Hex (..))
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import UnliftIO (MonadUnliftIO, throwIO, try, tryAny, withRunInIO)
import qualified UnliftIO
import UnliftIO.Concurrent (myThreadId)
-- * types

type DB m = (MonadIO m, MonadReader Connection m)

type EDB m = (DB m, Err m)

type Err m = (MonadError Integrity m, HasCallStack)

debugQuery, debugThread, debugConnection :: Bool
debugQuery = False
debugThread = False
debugConnection = False

alwaysTraceOnCrash :: Bool
alwaysTraceOnCrash = True

crashOnError :: Bool
crashOnError = False

throwError :: Err m => Integrity -> m c
throwError = if crashOnError then error . show else Except.throwError

data Integrity
  = UnknownHashId HashId
  | UnknownTextId TextId
  | UnknownObjectId ObjectId
  | UnknownCausalHashId CausalHashId
  | UnknownHash Hash
  | NoObjectForHashId HashId
  | NoObjectForPrimaryHashId HashId
  | NoNamespaceRoot
  | MultipleNamespaceRoots [CausalHashId]
  | NoSchemaVersion
  | MultipleSchemaVersions [SchemaVersion]
  | NoTypeIndexForTerm Referent.Id
  deriving (Show)

orError :: Err m => Integrity -> Maybe b -> m b
orError e = maybe (throwError e) pure

-- * main squeeze
createSchema :: (DB m, MonadUnliftIO m) => m ()
createSchema = do
  withImmediateTransaction . traverse_ (execute_ . fromString) $
    List.splitOn ";" [hereFile|sql/schemas/v3.sql|]

setJournalMode :: DB m => JournalMode -> m ()
setJournalMode m =
  let s = Char.toLower <$> show m
  in map (fromOnly @String)
    <$> query_ (fromString $ "PRAGMA journal_mode = " ++ s) >>= \case
      [y] | y == s -> pure ()
      y ->
        liftIO . putStrLn $
          "I couldn't set the codebase journal mode to " ++ s ++
          "; it's set to " ++ show y ++ "."

setFlags :: DB m => m ()
setFlags = do
  execute_ "PRAGMA foreign_keys = ON;"
  setJournalMode JournalMode.WAL

-- | Copy the database into the specified location, performing a VACUUM in the process.
vacuumInto :: DB m => FilePath -> m ()
vacuumInto dest = do
  execute "VACUUM INTO ?" [dest]

{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
schemaVersion :: DB m => m SchemaVersion
schemaVersion = queryAtoms_ sql >>= \case
  [] -> error $ show NoSchemaVersion
  [v] -> pure v
  vs -> error $ show (MultipleSchemaVersions vs)
  where sql = "SELECT version from schema_version;"

setSchemaVersion :: DB m => SchemaVersion -> m ()
setSchemaVersion schemaVersion = execute sql (Only schemaVersion)
  where
    sql = [here|
     UPDATE schema_version
     SET version = ?
     |]

countObjects :: DB m => m Int
countObjects = head <$> queryAtoms_ sql
  where sql = [here| SELECT COUNT(*) FROM object |]

countCausals :: DB m => m Int
countCausals = head <$> queryAtoms_ sql
  where sql = [here| SELECT COUNT(*) FROM causal |]

countWatches :: DB m => m Int
countWatches = head <$> queryAtoms_ sql
  where sql = [here| SELECT COUNT(*) FROM watch |]

saveHash :: DB m => Base32Hex -> m HashId
saveHash base32 = execute sql (Only base32) >> queryOne (loadHashId base32)
  where sql = [here|
    INSERT INTO hash (base32) VALUES (?)
    ON CONFLICT DO NOTHING
  |]

saveHashHash :: DB m => Hash -> m HashId
saveHashHash = saveHash . Hash.toBase32Hex

loadHashId :: DB m => Base32Hex -> m (Maybe HashId)
loadHashId base32 = queryAtom sql (Only base32)
  where sql = [here| SELECT id FROM hash WHERE base32 = ? |]

loadHashIdByHash :: DB m => Hash -> m (Maybe HashId)
loadHashIdByHash = loadHashId . Hash.toBase32Hex

saveCausalHash :: DB m => CausalHash -> m CausalHashId
saveCausalHash = fmap CausalHashId . saveHashHash . unCausalHash

saveBranchHash :: DB m => BranchHash -> m BranchHashId
saveBranchHash = fmap BranchHashId . saveHashHash . unBranchHash

loadCausalHashIdByCausalHash :: DB m => CausalHash -> m (Maybe CausalHashId)
loadCausalHashIdByCausalHash ch = runMaybeT do
  hId <- MaybeT $ loadHashIdByHash (unCausalHash ch)
  Alternative.whenM (isCausalHash hId) (CausalHashId hId)

loadCausalByCausalHash :: DB m => CausalHash -> m (Maybe (CausalHashId, BranchHashId, BranchObjectId))
loadCausalByCausalHash ch = runMaybeT do
  hId <- MaybeT $ loadHashIdByHash (unCausalHash ch)
  bhId <- MaybeT $ loadMaybeCausalValueHashId hId
  bObjId <- MaybeT $ maybeObjectIdForAnyHashId hId
  pure (CausalHashId hId, bhId, BranchObjectId bObjId)

expectHashIdByHash :: EDB m => Hash -> m HashId
expectHashIdByHash h = loadHashIdByHash h >>= orError (UnknownHash h)

loadHashHashById :: EDB m => HashId -> m Hash
loadHashHashById h = Hash.fromBase32Hex <$> loadHashById h

loadHashById :: EDB m => HashId -> m Base32Hex
loadHashById h = queryAtom sql (Only h) >>= orError (UnknownHashId h)
  where sql = [here| SELECT base32 FROM hash WHERE id = ? |]

saveText :: DB m => Text -> m TextId
saveText t = execute sql (Only t) >> queryOne (loadText t)
  where sql = [here| INSERT INTO text (text) VALUES (?) ON CONFLICT DO NOTHING|]

loadText :: DB m => Text -> m (Maybe TextId)
loadText t = queryAtom sql (Only t)
  where sql = [here| SELECT id FROM text WHERE text = ? |]

loadTextById :: EDB m => TextId -> m Text
loadTextById h = queryAtom sql (Only h) >>= orError (UnknownTextId h)
  where sql = [here| SELECT text FROM text WHERE id = ? |]

saveHashObject :: DB m => HashId -> ObjectId -> HashVersion -> m ()
saveHashObject hId oId version = execute sql (hId, oId, version) where
  sql = [here|
    INSERT INTO hash_object (hash_id, object_id, hash_version)
    VALUES (?, ?, ?)
    ON CONFLICT DO NOTHING
  |]

saveObject :: DB m => HashId -> ObjectType -> ByteString -> m ObjectId
saveObject h t blob = do
  oId <- execute sql (h, t, blob) >> queryOne (maybeObjectIdForPrimaryHashId h)
  saveHashObject h oId 2 -- todo: remove this from here, and add it to other relevant places once there are v1 and v2 hashes
  pure oId
  where
  sql = [here|
    INSERT INTO object (primary_hash_id, type_id, bytes)
    VALUES (?, ?, ?)
    ON CONFLICT DO NOTHING
  |]

loadObjectById :: EDB m => ObjectId -> m ByteString
loadObjectById id | debugQuery && trace ("loadObjectById " ++ show id) False = undefined
loadObjectById oId = do
 result <- queryAtom sql (Only oId) >>= orError (UnknownObjectId oId)
 when debugQuery $ traceM $ "loadObjectById " ++ show oId ++ " = " ++ show result
 pure result
  where sql = [here|
  SELECT bytes FROM object WHERE id = ?
|]

loadObjectWithTypeById :: EDB m => ObjectId -> m (ObjectType, ByteString)
loadObjectWithTypeById oId = queryMaybe sql (Only oId) >>= orError (UnknownObjectId oId)
  where sql = [here|
    SELECT type_id, bytes FROM object WHERE id = ?
  |]

loadObjectWithHashIdAndTypeById :: EDB m => ObjectId -> m (HashId, ObjectType, ByteString)
loadObjectWithHashIdAndTypeById oId = queryMaybe sql (Only oId) >>= orError (UnknownObjectId oId)
  where sql = [here|
    SELECT primary_hash_id, type_id, bytes FROM object WHERE id = ?
  |]

-- |Not all hashes have corresponding objects; e.g., hashes of term types
expectObjectIdForPrimaryHashId :: EDB m => HashId -> m ObjectId
expectObjectIdForPrimaryHashId h =
  maybeObjectIdForPrimaryHashId h >>= orError (NoObjectForPrimaryHashId h)

maybeObjectIdForPrimaryHashId :: DB m => HashId -> m (Maybe ObjectId)
maybeObjectIdForPrimaryHashId h = queryAtom sql (Only h) where sql = [here|
  SELECT id FROM object WHERE primary_hash_id = ?
|]

expectObjectIdForAnyHashId :: EDB m => HashId -> m ObjectId
expectObjectIdForAnyHashId h =
  maybeObjectIdForAnyHashId h >>= orError (NoObjectForHashId h)

maybeObjectIdForAnyHashId :: DB m => HashId -> m (Maybe ObjectId)
maybeObjectIdForAnyHashId h = queryAtom sql (Only h) where sql = [here|
    SELECT object_id FROM hash_object WHERE hash_id = ?
  |]

-- |All objects have corresponding hashes.
loadPrimaryHashByObjectId :: EDB m => ObjectId -> m Base32Hex
loadPrimaryHashByObjectId oId = queryAtom sql (Only oId) >>= orError (UnknownObjectId oId)
 where sql = [here|
  SELECT hash.base32
  FROM hash INNER JOIN object ON object.primary_hash_id = hash.id
  WHERE object.id = ?
|]

hashIdsForObject :: DB m => ObjectId -> m (NonEmpty HashId)
hashIdsForObject oId = do
  primaryHashId <- queryOne $ queryAtom sql1 (Only oId)
  hashIds <- queryAtoms sql2 (Only oId)
  pure $ primaryHashId Nel.:| filter (/= primaryHashId) hashIds
  where
    sql1 = "SELECT primary_hash_id FROM object WHERE id = ?"
    sql2 = "SELECT hash_id FROM hash_object WHERE object_id = ?"

hashIdWithVersionForObject :: DB m => ObjectId -> m [(HashId, HashVersion)]
hashIdWithVersionForObject = query sql . Only where sql = [here|
  SELECT hash_id, hash_version FROM hash_object WHERE object_id = ?
|]

-- | @recordObjectRehash old new@ records that object @old@ was rehashed and inserted as a new object, @new@.
--
-- This function rewrites @old@'s @hash_object@ rows in place to point at the new object.
recordObjectRehash :: DB m => ObjectId -> ObjectId -> m ()
recordObjectRehash old new =
  execute sql (new, old)
  where
    sql = [here|
      UPDATE hash_object
      SET object_id = ?
      WHERE object_id = ?
    |]

updateObjectBlob :: DB m => ObjectId -> ByteString -> m ()
updateObjectBlob oId bs = execute sql (oId, bs) where sql = [here|
  UPDATE object SET bytes = ? WHERE id = ?
|]

-- |Maybe we would generalize this to something other than NamespaceHash if we
-- end up wanting to store other kinds of Causals here too.
saveCausal :: DB m => CausalHashId -> BranchHashId -> m ()
saveCausal self value = execute sql (self, value) where sql = [here|
  INSERT INTO causal (self_hash_id, value_hash_id)
  VALUES (?, ?)
  ON CONFLICT DO NOTHING
|]
-- saveCausal self value = execute sql (self, value, Committed True, Generation 0) where sql = [here|
--   INSERT INTO causal (self_hash_id, value_hash_id, commit_flag, gc_generation)
--   VALUES (?, ?, ?, ?)
--   ON CONFLICT DO NOTHING
-- |]

-- -- maybe: look at whether parent causal is "committed"; if so, then increment;
-- -- otherwise, don't.
-- getNurseryGeneration :: DB m => m Generation
-- getNurseryGeneration = query_ sql <&> \case
--   [] -> Generation 0
--   [fromOnly -> g] -> Generation $ fromMaybe 0 g
--   (fmap fromOnly -> gs) ->
--     error $ "How did I get multiple values out of a MAX()? " ++ show gs
--   where sql = [here|
--     SELECT MAX(gc_generation) FROM causal;
--   |]

loadCausalValueHashId :: EDB m => CausalHashId -> m BranchHashId
loadCausalValueHashId chId@(CausalHashId id) =
  loadMaybeCausalValueHashId (id) >>= orError (UnknownCausalHashId chId)

loadCausalHash :: EDB m => CausalHashId -> m CausalHash
loadCausalHash (CausalHashId id) = CausalHash <$> loadHashHashById id

loadMaybeCausalValueHashId :: DB m => HashId -> m (Maybe BranchHashId)
loadMaybeCausalValueHashId id =
  queryAtom sql (Only id) where sql = [here|
  SELECT value_hash_id FROM causal WHERE self_hash_id = ?
|]

isCausalHash :: DB m => HashId -> m Bool
isCausalHash = queryOne . queryAtom sql . Only where sql = [here|
    SELECT EXISTS (SELECT 1 FROM causal WHERE self_hash_id = ?)
  |]

loadBranchObjectIdByCausalHashId :: EDB m => CausalHashId -> m (Maybe BranchObjectId)
loadBranchObjectIdByCausalHashId id = queryAtom sql (Only id) where sql = [here|
  SELECT object_id FROM hash_object
  INNER JOIN causal ON hash_id = causal.value_hash_id
  WHERE causal.self_hash_id = ?
|]

saveCausalParents :: DB m => CausalHashId -> [CausalHashId] -> m ()
saveCausalParents child parents = executeMany sql $ (child,) <$> parents where
  sql = [here|
    INSERT INTO causal_parent (causal_id, parent_id) VALUES (?, ?)
    ON CONFLICT DO NOTHING
  |]

loadCausalParents :: DB m => CausalHashId -> m [CausalHashId]
loadCausalParents h = queryAtoms sql (Only h) where sql = [here|
  SELECT parent_id FROM causal_parent WHERE causal_id = ?
|]

-- | The data version will increase if there has been any external
-- modification to the database since the last observed data version.
newtype DataVersion = DataVersion Int
  deriving (Eq, Ord, Show)
  deriving FromField via Int
dataVersion :: DB m => m DataVersion
dataVersion = queryOne . fmap (fmap fromOnly) . fmap headMay $ query_ [here|
    PRAGMA data_version
  |]

loadNamespaceRoot :: EDB m => m CausalHashId
loadNamespaceRoot = loadMaybeNamespaceRoot >>= \case
  Nothing -> throwError NoNamespaceRoot
  Just id -> pure id

loadMaybeNamespaceRoot :: EDB m => m (Maybe CausalHashId)
loadMaybeNamespaceRoot = query_ sql >>= \case
  [] -> pure Nothing
  [Only id] -> pure (Just id)
  (fmap fromOnly -> ids) -> throwError (MultipleNamespaceRoots ids)
 where sql = "SELECT causal_id FROM namespace_root"

setNamespaceRoot :: forall m. DB m => CausalHashId -> m ()
setNamespaceRoot id =
  query_ @m @(Only CausalHashId) "SELECT * FROM namespace_root" >>= \case
    [] -> execute insert (Only id)
    _ -> execute update (Only id)
  where
    insert = "INSERT INTO namespace_root VALUES (?)"
    update = "UPDATE namespace_root SET causal_id = ?"

saveWatch :: DB m => WatchKind -> Reference.IdH -> ByteString -> m ()
saveWatch k r blob = execute sql (r :. Only blob) >> execute sql2 (r :. Only k)
  where
    sql = [here|
      INSERT INTO watch_result (hash_id, component_index, result)
      VALUES (?, ?, ?)
      ON CONFLICT DO NOTHING
    |]
    sql2 = [here|
      INSERT INTO watch (hash_id, component_index, watch_kind_id)
      VALUES (?, ?, ?)
      ON CONFLICT DO NOTHING
    |]

loadWatch :: DB m => WatchKind -> Reference.IdH -> m (Maybe ByteString)
loadWatch k r = queryAtom sql (Only k :. r) where sql = [here|
    SELECT result FROM watch_result
    INNER JOIN watch
      ON watch_result.hash_id = watch.hash_id
      AND watch_result.component_index = watch.component_index
    WHERE watch.watch_kind_id = ?
      AND watch.hash_id = ?
      AND watch.component_index = ?
  |]

loadWatchKindsByReference :: DB m => Reference.IdH -> m [WatchKind]
loadWatchKindsByReference r = queryAtoms sql r where sql = [here|
    SELECT watch_kind_id FROM watch_result
    INNER JOIN watch
      ON watch_result.hash_id = watch.hash_id
      AND watch_result.component_index = watch.component_index
    WHERE watch.hash_id = ?
      AND watch.component_index = ?
  |]

loadWatchesByWatchKind :: DB m => WatchKind -> m [Reference.IdH]
loadWatchesByWatchKind k = query sql (Only k) where sql = [here|
  SELECT hash_id, component_index FROM watch WHERE watch_kind_id = ?
|]

clearWatches :: DB m => m ()
clearWatches = do
  execute_ "DELETE FROM watch_result"
  execute_ "DELETE FROM watch"

-- * Index-building
addToTypeIndex :: DB m => Reference' TextId HashId -> Referent.Id -> m ()
addToTypeIndex tp tm = execute sql (tp :. tm) where sql = [here|
  INSERT INTO find_type_index (
    type_reference_builtin,
    type_reference_hash_id,
    type_reference_component_index,
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  ) VALUES (?, ?, ?, ?, ?, ?)
  ON CONFLICT DO NOTHING
|]

getReferentsByType :: DB m => Reference' TextId HashId -> m [Referent.Id]
getReferentsByType r = query sql r where sql = [here|
  SELECT
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  FROM find_type_index
  WHERE type_reference_builtin IS ?
    AND type_reference_hash_id IS ?
    AND type_reference_component_index IS ?
|]

getTypeReferenceForReferent :: EDB m => Referent.Id -> m (Reference' TextId HashId)
getTypeReferenceForReferent r =
  queryMaybe sql r >>= orError (NoTypeIndexForTerm r)
  where sql = [here|
  SELECT
    type_reference_builtin,
    type_reference_hash_id,
    type_reference_component_index
  FROM find_type_index
  WHERE term_referent_object_id = ?
    AND term_referent_component_index = ?
    AND term_referent_constructor_index IS ?
|]

-- todo: error if no results
getTypeReferencesForComponent :: EDB m => ObjectId -> m [(Reference' TextId HashId, Referent.Id)]
getTypeReferencesForComponent oId =
  query sql (Only oId) <&> map fixupTypeIndexRow where sql = [here|
    SELECT
      type_reference_builtin,
      type_reference_hash_id,
      type_reference_component_index,
      term_referent_object_id,
      term_referent_component_index,
      term_referent_constructor_index
    FROM find_type_index
    WHERE term_referent_object_id = ?
  |]

addToTypeMentionsIndex :: DB m => Reference' TextId HashId -> Referent.Id -> m ()
addToTypeMentionsIndex tp tm = execute sql (tp :. tm) where sql = [here|
  INSERT INTO find_type_mentions_index (
    type_reference_builtin,
    type_reference_hash_id,
    type_reference_component_index,
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  ) VALUES (?, ?, ?, ?, ?, ?)
  ON CONFLICT DO NOTHING
|]

getReferentsByTypeMention :: DB m => Reference' TextId HashId -> m [Referent.Id]
getReferentsByTypeMention r = query sql r where sql = [here|
  SELECT
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  FROM find_type_mentions_index
  WHERE type_reference_builtin IS ?
    AND type_reference_hash_id IS ?
    AND type_reference_component_index IS ?
|]

-- todo: error if no results
getTypeMentionsReferencesForComponent :: EDB m => ObjectId -> m [(Reference' TextId HashId, Referent.Id)]
getTypeMentionsReferencesForComponent r =
  query sql (Only r) <&> map fixupTypeIndexRow where sql = [here|
    SELECT
      type_reference_builtin,
      type_reference_hash_id,
      type_reference_component_index,
      term_referent_object_id,
      term_referent_component_index,
      term_referent_constructor_index
    FROM find_type_mentions_index
    WHERE term_referent_object_id IS ?
  |]

fixupTypeIndexRow :: Reference' TextId HashId :. Referent.Id -> (Reference' TextId HashId, Referent.Id)
fixupTypeIndexRow (rh :. ri) = (rh, ri)

-- | Delete objects without hashes. An object typically *would* have a hash, but (for example) during a migration in which an object's hash
-- may change, its corresponding hash_object row may be updated to point at a new version of that object. This procedure clears out all
-- references to objects that do not have any corresponding hash_object rows.
garbageCollectObjectsWithoutHashes :: DB m => m ()
garbageCollectObjectsWithoutHashes = do
  execute_
    [here|
      CREATE TEMPORARY TABLE object_without_hash AS
        SELECT id
        FROM object
        WHERE id NOT IN (
          SELECT object_id
          FROM hash_object
        )
    |]
  execute_
    [here|
      DELETE FROM dependents_index
      WHERE dependency_object_id IN object_without_hash
        OR dependent_object_id IN object_without_hash
    |]
  execute_
    [here|
      DELETE FROM find_type_index
      WHERE term_referent_object_id IN object_without_hash
    |]
  execute_
    [here|
      DELETE FROM find_type_mentions_index
      WHERE term_referent_object_id IN object_without_hash
    |]
  execute_
    [here|
      DELETE FROM object
      WHERE id IN object_without_hash
    |]
  execute_
    [here|
      DROP TABLE object_without_hash
    |]

-- | Delete all
garbageCollectWatchesWithoutObjects :: DB m => m ()
garbageCollectWatchesWithoutObjects = do
  execute_
    [here|
      DELETE FROM watch
      WHERE watch.hash_id NOT IN
      (SELECT hash_object.hash_id FROM hash_object)
    |]

-- | Clean the database and recover disk space.
-- This is an expensive operation. Also note that it cannot be executed within a transaction.
vacuum :: DB m => m ()
vacuum = execute_ "VACUUM"

addToDependentsIndex :: DB m => Reference.Reference -> Reference.Id -> m ()
addToDependentsIndex dependency dependent = execute sql (dependency :. dependent)
  where sql = [here|
    INSERT INTO dependents_index (
      dependency_builtin,
      dependency_object_id,
      dependency_component_index,
      dependent_object_id,
      dependent_component_index
    ) VALUES (?, ?, ?, ?, ?)
    ON CONFLICT DO NOTHING
  |]

-- | Get non-self, user-defined dependents of a dependency.
getDependentsForDependency :: DB m => Reference.Reference -> m [Reference.Id]
getDependentsForDependency dependency =
  filter isNotSelfReference <$> query sql dependency
  where
    sql =
      [here|
        SELECT dependent_object_id, dependent_component_index
        FROM dependents_index
        WHERE dependency_builtin IS ?
          AND dependency_object_id IS ?
          AND dependency_component_index IS ?
      |]

    isNotSelfReference :: Reference.Id -> Bool
    isNotSelfReference =
      case dependency of
        ReferenceBuiltin _ -> const True
        ReferenceDerived (C.Reference.Id oid0 _pos0) -> \(C.Reference.Id oid1 _pos1) -> oid0 /= oid1

getDependentsForDependencyComponent :: DB m => ObjectId -> m [Reference.Id]
getDependentsForDependencyComponent dependency =
  filter isNotSelfReference <$> query sql (Only dependency)
  where
    sql =
      [here|
        SELECT dependent_object_id, dependent_component_index
        FROM dependents_index
        WHERE dependency_builtin IS NULL
          AND dependency_object_id IS ?
      |]

    isNotSelfReference :: Reference.Id -> Bool
    isNotSelfReference = \case
      (C.Reference.Id oid1 _pos1) -> dependency /= oid1

-- | Get non-self dependencies of a user-defined dependent.
getDependenciesForDependent :: DB m => Reference.Id -> m [Reference.Reference]
getDependenciesForDependent dependent@(C.Reference.Id oid0 _) =
  filter isNotSelfReference <$> query sql dependent
  where
    sql = [here|
      SELECT dependency_builtin, dependency_object_id, dependency_component_index
      FROM dependents_index
      WHERE dependent_object_id IS ?
        AND dependent_component_index IS ?
    |]

    isNotSelfReference :: Reference.Reference -> Bool
    isNotSelfReference = \case
      ReferenceBuiltin _ -> True
      ReferenceDerived (C.Reference.Id oid1 _) -> oid0 /= oid1

-- | Get non-self, user-defined dependencies of a user-defined dependent.
getDependencyIdsForDependent :: DB m => Reference.Id -> m [Reference.Id]
getDependencyIdsForDependent dependent@(C.Reference.Id oid0 _) =
  filter isNotSelfReference <$> query sql dependent
  where
    sql =
      [here|
        SELECT dependency_object_id, dependency_component_index
        FROM dependents_index
        WHERE dependency_builtin IS NULL
          AND dependent_object_id = ?
          AND dependen_component_index = ?
      |]

    isNotSelfReference :: Reference.Id -> Bool
    isNotSelfReference (C.Reference.Id oid1 _) =
      oid0 /= oid1

objectIdByBase32Prefix :: DB m => ObjectType -> Text -> m [ObjectId]
objectIdByBase32Prefix objType prefix = queryAtoms sql (objType, prefix <> "%") where sql = [here|
  SELECT object.id FROM object
  INNER JOIN hash_object ON hash_object.object_id = object.id
  INNER JOIN hash ON hash_object.hash_id = hash.id
  WHERE object.type_id = ?
    AND hash.base32 LIKE ?
|]

causalHashIdByBase32Prefix :: DB m => Text -> m [CausalHashId]
causalHashIdByBase32Prefix prefix = queryAtoms sql (Only $ prefix <> "%") where sql = [here|
  SELECT self_hash_id FROM causal
  INNER JOIN hash ON id = self_hash_id
  WHERE base32 LIKE ?
|]

namespaceHashIdByBase32Prefix :: DB m => Text -> m [BranchHashId]
namespaceHashIdByBase32Prefix prefix = queryAtoms sql (Only $ prefix <> "%") where sql = [here|
  SELECT value_hash_id FROM causal
  INNER JOIN hash ON id = value_hash_id
  WHERE base32 LIKE ?
|]

-- | Finds all causals that refer to a branch for which we don't have an object stored.
-- Although there are plans to support this in the future, currently all such cases
-- are the result of database inconsistencies and are unexpected.
getCausalsWithoutBranchObjects :: DB m => m [CausalHashId]
getCausalsWithoutBranchObjects = queryAtoms_ sql
  where sql = [here|
    SELECT self_hash_id from causal
    WHERE value_hash_id NOT IN (SELECT hash_id FROM hash_object)
|]

{- ORMOLU_ENABLE -}

-- | Delete all hash objects of a given hash version.
-- Leaves the corresponding `hash`es in the hash table alone.
removeHashObjectsByHashingVersion :: DB m => HashVersion -> m ()
removeHashObjectsByHashingVersion hashVersion =
  execute sql (Only hashVersion)
  where
    sql =
      [here|
    DELETE FROM hash_object
      WHERE hash_version = ?
|]

addHashObjectConstraintToCausalTable :: DB m => m ()
addHashObjectConstraintToCausalTable =
  execute_ sql
  where
    sql =
      [here|
-- Not sure why, but we need this savepoint or this operation fails.
SAVEPOINT ADD_HASH_OBJECT_CONSTRAINTS;
ALTER TABLE causal RENAME TO old_causal;
CREATE TABLE causal (
  self_hash_id INTEGER PRIMARY KEY NOT NULL CONSTRAINT causal_fk1 REFERENCES hash(id),
  value_hash_id INTEGER NOT NULL CONSTRAINT causal_fk2 REFERENCES hash_object(hash_id)
);
INSERT INTO causal SELECT * FROM old_causal;
DROP TABLE old_causal;
RELEASE ADD_HASH_OBJECT_CONSTRAINTS;
|]

before :: DB m => CausalHashId -> CausalHashId -> m Bool
before chId1 chId2 = fmap fromOnly . queryOne $ queryMaybe sql (chId2, chId1)
  where sql = fromString $ "SELECT EXISTS (" ++ ancestorSql ++ " WHERE ancestor.id = ?)"

-- the `Connection` arguments come second to fit the shape of Exception.bracket + uncurry curry
lca :: CausalHashId -> CausalHashId -> Connection -> Connection -> IO (Maybe CausalHashId)
lca x y _ _ | debugQuery && trace ("Q.lca " ++ show x ++ " " ++ show y) False = undefined
lca x y (Connection.underlying -> cx) (Connection.underlying -> cy) = Exception.bracket open close \(sx, sy) -> do
  SQLite.bind sx (Only x)
  SQLite.bind sy (Only y)
  let getNext = (,) <$> SQLite.nextRow sx <*> SQLite.nextRow sy
      loop2 seenX seenY =
        getNext >>= \case
          (Just (Only px), Just (Only py)) ->
            let seenX' = Set.insert px seenX
                seenY' = Set.insert py seenY
              in if Set.member px seenY' then pure (Just px)
              else if Set.member py seenX' then pure (Just py)
              else loop2 seenX' seenY'
          (Nothing, Nothing) -> pure Nothing
          (Just (Only px), Nothing) -> loop1 (SQLite.nextRow sx) seenY px
          (Nothing, Just (Only py)) -> loop1 (SQLite.nextRow sy) seenX py
      loop1 getNext matches v =
        if Set.member v matches then pure (Just v)
        else getNext >>= \case
          Just (Only v) -> loop1 getNext matches v
          Nothing -> pure Nothing
  loop2 (Set.singleton x) (Set.singleton y)
  where
    open = (,) <$>
      SQLite.openStatement cx sql <*> SQLite.openStatement cy sql
    close (cx, cy) = SQLite.closeStatement cx *> SQLite.closeStatement cy
    sql = fromString ancestorSql

ancestorSql :: String
ancestorSql = [here|
    WITH RECURSIVE
      ancestor(id) AS (
        SELECT self_hash_id
          FROM causal
          WHERE self_hash_id = ?
        UNION ALL
        SELECT parent_id
          FROM causal_parent
          JOIN ancestor ON ancestor.id = causal_id
      )
    SELECT * FROM ancestor
  |]

-- * helper functions

-- | composite input, atomic List output
queryAtoms :: (DB f, ToRow q, FromField b, Show q, Show b) => SQLite.Query -> q -> f [b]
queryAtoms q r = map fromOnly <$> query q r

-- | no input, atomic List output
queryAtoms_ :: (DB f, FromField b, Show b) => SQLite.Query -> f [b]
queryAtoms_ q = map fromOnly <$> query_ q

-- | composite input, composite Maybe output
queryMaybe :: (DB f, ToRow q, FromRow b, Show q, Show b) => SQLite.Query -> q -> f (Maybe b)
queryMaybe q r = headMay <$> query q r

-- | composite input, atomic Maybe output
queryAtom :: (DB f, ToRow q, FromField b, Show q, Show b) => SQLite.Query -> q -> f (Maybe b)
queryAtom q r = fmap fromOnly <$> queryMaybe q r

-- | Just output
queryOne :: Functor f => f (Maybe b) -> f b
queryOne = fmap fromJust

-- | composite input, composite List output
query :: (DB m, ToRow q, FromRow r, Show q, Show r) => SQLite.Query -> q -> m [r]
query q r = do
  c <- Reader.reader Connection.underlying
  header <- debugHeader
  liftIO . queryTrace (header ++ " query") q r $ SQLite.query c q r

-- | no input, composite List output
query_ :: (DB m, FromRow r, Show r) => SQLite.Query -> m [r]
query_ q = do
  c <- Reader.reader Connection.underlying
  header <- debugHeader
  liftIO . queryTrace_ (header ++ " query") q $ SQLite.query_ c q

debugHeader :: DB m => m String
debugHeader = fmap (List.intercalate ", ") $ Writer.execWriterT do
  when debugThread $ Writer.tell . pure . show =<< myThreadId
  when debugConnection $ Writer.tell . pure . show =<< ask

queryTrace :: (MonadUnliftIO m, Show q, Show a) => String -> SQLite.Query -> q -> m a -> m a
queryTrace title query input m = do
  let showInput = title ++ " " ++ show query ++ "\n  input: " ++ show input
  if debugQuery || alwaysTraceOnCrash
    then
     do
      try @_ @SQLite.SQLError m >>= \case
        Right a -> do
          when debugQuery . traceM $ showInput ++
            if " execute" `List.isSuffixOf` title then mempty else "\n output: " ++ show a
          pure a
        Left e -> do
          traceM $ showInput ++ "\n(and crashed)\n"
          throwIO e
    else m

queryTrace_ :: (MonadUnliftIO m, Show a) => String -> SQLite.Query -> m a -> m a
queryTrace_ title query m =
  if debugQuery || alwaysTraceOnCrash
    then
      tryAny @_ m >>= \case
        Right a -> do
          when debugQuery . traceM $ title ++ " " ++ show query ++
            if " execute_" `List.isSuffixOf` title then mempty else "\n output: " ++ show a
          pure a
        Left e -> do
          traceM $ title ++ " " ++ show query ++ "\n(and crashed)\n"
          throwIO e
    else m

-- |print the active database filename
traceConnectionFile :: DB m => m ()
traceConnectionFile = do
  c <- Reader.reader Connection.underlying
  liftIO (SQLite.query_ c "PRAGMA database_list;") >>= \case
    [(_seq :: Int, _name :: String, file)] -> traceM file
    x -> error $ show x

execute :: (DB m, ToRow q, Show q) => SQLite.Query -> q -> m ()
execute q r = do
  c <- Reader.reader Connection.underlying
  header <- debugHeader
  liftIO . queryTrace (header ++ " " ++ "execute") q r $ SQLite.execute c q r

execute_ :: DB m => SQLite.Query -> m ()
execute_ q = do
  c <- Reader.reader Connection.underlying
  header <- debugHeader
  liftIO . queryTrace_ (header ++ " " ++ "execute_") q $ SQLite.execute_ c q

executeMany :: (DB m, ToRow q, Show q) => SQLite.Query -> [q] -> m ()
executeMany q r = do
  c <- Reader.reader Connection.underlying
  header <- debugHeader
  liftIO . queryTrace (header ++ " " ++ "executeMany") q r $ SQLite.executeMany c q r

-- | transaction that blocks
withImmediateTransaction :: (DB m, MonadUnliftIO m) => m a -> m a
withImmediateTransaction action = do
  c <- Reader.reader Connection.underlying
  withRunInIO \run -> SQLite.withImmediateTransaction c (run action)


-- | low-level transaction stuff

-- | Create a savepoint, which is a named transaction which may wrap many nested
-- sub-transactions.
savepoint :: DB m => String -> m ()
savepoint name = execute_ (fromString $ "SAVEPOINT " ++ name)

-- | Release a savepoint, which will commit the results once all
-- wrapping transactions/savepoints are commited.
release :: DB m => String -> m ()
release name = execute_ (fromString $ "RELEASE " ++ name)

-- | Roll the database back to its state from when the savepoint was created.
-- Note: this also re-starts the savepoint and it must still be released if that is the
-- intention. See 'rollbackRelease'.
rollbackTo :: DB m => String -> m ()
rollbackTo name = execute_ (fromString $ "ROLLBACK TO " ++ name)

-- | Roll back the savepoint and immediately release it.
-- This effectively _aborts_ the savepoint, useful if an irrecoverable error is
-- encountered.
rollbackRelease :: DB m => String -> m ()
rollbackRelease name = rollbackTo name *> release name

-- | Runs the provided action within a savepoint.
-- Releases the savepoint on completion.
-- If an exception occurs, the savepoint will be rolled-back and released,
-- abandoning all changes.
withSavepoint :: (MonadUnliftIO m, DB m) => String -> (m () -> m r) -> m r
withSavepoint name action =
  UnliftIO.bracket_
    (savepoint name)
    (release name)
    (action (rollbackTo name) `UnliftIO.onException` rollbackTo name)

withSavepoint_ :: (MonadUnliftIO m, DB m) => String -> m r -> m r
withSavepoint_ name action = withSavepoint name (\_rollback -> action)

-- * orphan instances

deriving via Text instance ToField Base32Hex

deriving via Text instance FromField Base32Hex

instance ToField WatchKind where
  toField = \case
    WatchKind.RegularWatch -> SQLite.SQLInteger 0
    WatchKind.TestWatch -> SQLite.SQLInteger 1

instance FromField WatchKind where
  fromField = fromField @Int8  <&> fmap \case
    0 -> WatchKind.RegularWatch
    1 -> WatchKind.TestWatch
    tag -> error $ "Unknown WatchKind id " ++ show tag
