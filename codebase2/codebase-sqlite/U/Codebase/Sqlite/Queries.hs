{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Queries
  ( -- * text table
    saveText,
    loadText,
    expectText,
    loadTextById,
    loadTextByIdCheck,

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
    expectDeclObjectById,
    loadDeclObjectById,
    expectNamespaceObjectById,
    loadNamespaceObjectById,
    expectPatchObjectById,
    loadPatchObjectById,
    loadTermObjectById,
    expectTermObjectById,
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
    expectBranchObjectIdByCausalHashId,

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

    -- ** migrations
    countObjects,
    countCausals,
    countWatches,
    getCausalsWithoutBranchObjects,
    removeHashObjectsByHashingVersion,

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

    -- * db misc
    createSchema,
    schemaVersion,
    setSchemaVersion,
    vacuumInto,
  )
where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Int (Int8)
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Set as Set
import Data.String (fromString)
import Data.String.Here.Uninterpolated (here, hereFile)
import Data.Text (Text)
import Data.Tuple.Only (Only (..))
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference (Reference' (..))
import qualified U.Codebase.Reference as C.Reference
import U.Codebase.Sqlite.DbId
  ( BranchHashId (..),
    BranchObjectId (..),
    CausalHashId (..),
    HashId (..),
    HashVersion,
    ObjectId (..),
    SchemaVersion,
    TextId,
  )
import U.Codebase.Sqlite.ObjectType (ObjectType (DeclComponent, Namespace, Patch, TermComponent))
import qualified U.Codebase.Sqlite.Reference as Reference
import qualified U.Codebase.Sqlite.Referent as Referent
import U.Codebase.WatchKind (WatchKind)
import qualified U.Codebase.WatchKind as WatchKind
import qualified U.Util.Alternative as Alternative
import U.Util.Base32Hex (Base32Hex (..))
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import Unison.Sqlite
import qualified Unison.Sqlite.DB as DB
import qualified Unison.Sqlite.Transaction as Transaction
import UnliftIO (MonadUnliftIO)

-- * main squeeze

createSchema :: (DB m, MonadUnliftIO m) => m ()
createSchema =
  DB.runTransaction do
    traverse_ (Transaction.execute_ . fromString) $ List.splitOn ";" [hereFile|sql/create.sql|]

-- | Copy the database into the specified location, performing a VACUUM in the process.
vacuumInto :: DB m => FilePath -> m ()
vacuumInto dest = do
  execute "VACUUM INTO ?" [dest]

schemaVersion :: DB m => m SchemaVersion
schemaVersion = queryOneCol_ sql
  where
    sql = "SELECT version from schema_version;"

setSchemaVersion :: DB m => SchemaVersion -> m ()
setSchemaVersion schemaVersion = execute sql (Only schemaVersion)
  where
    sql = "UPDATE schema_version SET version = ?"

{- ORMOLU_DISABLE -}
{- Please don't try to format the SQL blocks —AI -}
countObjects :: DB m => m Int
countObjects = queryOneCol_ [here| SELECT COUNT(*) FROM object |]

countCausals :: DB m => m Int
countCausals = queryOneCol_ [here| SELECT COUNT(*) FROM causal |]

countWatches :: DB m => m Int
countWatches = queryOneCol_ [here| SELECT COUNT(*) FROM watch |]

saveHash :: DB m => Base32Hex -> m HashId
saveHash base32 = execute sql (Only base32) >> expectHashId base32
  where sql = [here|
    INSERT INTO hash (base32) VALUES (?)
    ON CONFLICT DO NOTHING
  |]

saveHashHash :: DB m => Hash -> m HashId
saveHashHash = saveHash . Hash.toBase32Hex

loadHashId :: DB m => Base32Hex -> m (Maybe HashId)
loadHashId base32 = queryMaybeCol loadHashIdSql (Only base32)

expectHashId :: DB m => Base32Hex -> m HashId
expectHashId base32 = queryOneCol loadHashIdSql (Only base32)

loadHashIdSql :: Sql
loadHashIdSql =
  [here| SELECT id FROM hash WHERE base32 = ? |]

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

loadCausalByCausalHash :: DB m => CausalHash -> m (Maybe (CausalHashId, BranchHashId))
loadCausalByCausalHash ch = runMaybeT do
  hId <- MaybeT $ loadHashIdByHash (unCausalHash ch)
  bhId <- MaybeT $ loadMaybeCausalValueHashId hId
  pure (CausalHashId hId, bhId)

expectHashIdByHash :: DB m => Hash -> m HashId
expectHashIdByHash = expectHashId . Hash.toBase32Hex

-- FIXME rename to expectHashHashById
loadHashHashById :: DB m => HashId -> m Hash
loadHashHashById h = Hash.fromBase32Hex <$> loadHashById h

-- FIXME rename to expectHashById
loadHashById :: DB m => HashId -> m Base32Hex
loadHashById h = queryOneCol sql (Only h)
  where sql = [here| SELECT base32 FROM hash WHERE id = ? |]

saveText :: DB m => Text -> m TextId
saveText t = execute sql (Only t) >> expectText t
  where sql = [here| INSERT INTO text (text) VALUES (?) ON CONFLICT DO NOTHING|]

loadText :: DB m => Text -> m (Maybe TextId)
loadText t = queryMaybeCol loadTextSql (Only t)

expectText :: DB m => Text -> m TextId
expectText t = queryOneCol loadTextSql (Only t)

loadTextSql :: Sql
loadTextSql =
  [here| SELECT id FROM text WHERE text = ? |]

-- FIXME rename to expectTextById
loadTextById :: DB m => TextId -> m Text
loadTextById h = queryOneCol loadTextByIdSql (Only h)

loadTextByIdCheck :: (DB m, SqliteExceptionReason e) => (Text -> Either e a) -> TextId -> m a
loadTextByIdCheck check h = queryOneColCheck loadTextByIdSql (Only h) check

loadTextByIdSql :: Sql
loadTextByIdSql =
  [here| SELECT text FROM text WHERE id = ? |]

saveHashObject :: DB m => HashId -> ObjectId -> HashVersion -> m ()
saveHashObject hId oId version = execute sql (hId, oId, version) where
  sql = [here|
    INSERT INTO hash_object (hash_id, object_id, hash_version)
    VALUES (?, ?, ?)
    ON CONFLICT DO NOTHING
  |]

saveObject :: DB m => HashId -> ObjectType -> ByteString -> m ObjectId
saveObject h t blob = do
  oId <- execute sql (h, t, blob) >> expectObjectIdForPrimaryHashId h
  saveHashObject h oId 2 -- todo: remove this from here, and add it to other relevant places once there are v1 and v2 hashes
  pure oId
  where
  sql = [here|
    INSERT INTO object (primary_hash_id, type_id, bytes)
    VALUES (?, ?, ?)
    ON CONFLICT DO NOTHING
  |]

-- FIXME rename to expectObjectById
loadObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m a
loadObjectById oId check = do
 result <- queryOneColCheck sql (Only oId) check
 pure result
  where sql = [here|
  SELECT bytes FROM object WHERE id = ?
|]

-- FIXME rename to expectObjectWithTypeById
loadObjectWithTypeById :: DB m => ObjectId -> m (ObjectType, ByteString)
loadObjectWithTypeById oId = queryOneRow sql (Only oId)
  where sql = [here|
    SELECT type_id, bytes FROM object WHERE id = ?
  |]

loadObjectOfTypeById ::
  (DB m, SqliteExceptionReason e) =>
  ObjectId ->
  ObjectType ->
  (ByteString -> Either e a) ->
  m (Maybe a)
loadObjectOfTypeById oid ty =
  queryMaybeColCheck loadObjectOfTypeByIdSql (oid, ty)

expectObjectOfTypeById :: (DB m, SqliteExceptionReason e) => ObjectId -> ObjectType -> (ByteString -> Either e a) -> m a
expectObjectOfTypeById oid ty =
  queryOneColCheck loadObjectOfTypeByIdSql (oid, ty)

loadObjectOfTypeByIdSql :: Sql
loadObjectOfTypeByIdSql =
  [here|
    SELECT bytes
    FROM object
    WHERE id = ?
      AND type_id = ?
  |]

-- | Load a decl component object.
loadDeclObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m (Maybe a)
loadDeclObjectById oid =
  loadObjectOfTypeById oid DeclComponent

-- | Expect a decl component object.
expectDeclObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m a
expectDeclObjectById oid =
  expectObjectOfTypeById oid DeclComponent

-- | Load a namespace object.
loadNamespaceObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m (Maybe a)
loadNamespaceObjectById oid =
  loadObjectOfTypeById oid Namespace

-- | Expect a namespace object.
expectNamespaceObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m a
expectNamespaceObjectById oid =
  expectObjectOfTypeById oid Namespace

-- | Load a patch object.
loadPatchObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m (Maybe a)
loadPatchObjectById oid =
  loadObjectOfTypeById oid Patch

-- | Expect a patch object.
expectPatchObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m a
expectPatchObjectById oid =
  expectObjectOfTypeById oid Patch

-- | Load a term component object.
loadTermObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m (Maybe a)
loadTermObjectById oid =
  loadObjectOfTypeById oid TermComponent

-- | Expect a term component object.
expectTermObjectById :: (DB m, SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> m a
expectTermObjectById oid =
  expectObjectOfTypeById oid TermComponent

-- | FIXME rename to expectObjectWithHashIdAndTypeById
loadObjectWithHashIdAndTypeById :: DB m => ObjectId -> m (HashId, ObjectType, ByteString)
loadObjectWithHashIdAndTypeById oId = queryOneRow sql (Only oId)
  where sql = [here|
    SELECT primary_hash_id, type_id, bytes FROM object WHERE id = ?
  |]

-- |Not all hashes have corresponding objects; e.g., hashes of term types
expectObjectIdForPrimaryHashId :: DB m => HashId -> m ObjectId
expectObjectIdForPrimaryHashId h =
  queryOneCol maybeObjectIdForPrimaryHashIdSql (Only h)

maybeObjectIdForPrimaryHashId :: DB m => HashId -> m (Maybe ObjectId)
maybeObjectIdForPrimaryHashId h = queryMaybeCol maybeObjectIdForPrimaryHashIdSql (Only h)

maybeObjectIdForPrimaryHashIdSql :: Sql
maybeObjectIdForPrimaryHashIdSql =
  [here| SELECT id FROM object WHERE primary_hash_id = ? |]

expectObjectIdForAnyHashId :: DB m => HashId -> m ObjectId
expectObjectIdForAnyHashId h =
  queryOneCol maybeObjectIdForAnyHashIdSql (Only h)

maybeObjectIdForAnyHashId :: DB m => HashId -> m (Maybe ObjectId)
maybeObjectIdForAnyHashId h = queryMaybeCol maybeObjectIdForAnyHashIdSql (Only h)

maybeObjectIdForAnyHashIdSql :: Sql
maybeObjectIdForAnyHashIdSql =
  [here| SELECT object_id FROM hash_object WHERE hash_id = ? |]

-- |All objects have corresponding hashes.
loadPrimaryHashByObjectId :: DB m => ObjectId -> m Base32Hex
loadPrimaryHashByObjectId oId = queryOneCol sql (Only oId)
 where sql = [here|
  SELECT hash.base32
  FROM hash INNER JOIN object ON object.primary_hash_id = hash.id
  WHERE object.id = ?
|]

hashIdsForObject :: DB m => ObjectId -> m (NonEmpty HashId)
hashIdsForObject oId = do
  primaryHashId <- queryOneCol sql1 (Only oId)
  hashIds <- queryListCol sql2 (Only oId)
  pure $ primaryHashId Nel.:| filter (/= primaryHashId) hashIds
  where
    sql1 = "SELECT primary_hash_id FROM object WHERE id = ?"
    sql2 = "SELECT hash_id FROM hash_object WHERE object_id = ?"

hashIdWithVersionForObject :: DB m => ObjectId -> m [(HashId, HashVersion)]
hashIdWithVersionForObject = queryListRow sql . Only where sql = [here|
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

-- FIXME rename to expectCausalValueHashId
loadCausalValueHashId :: DB m => CausalHashId -> m BranchHashId
loadCausalValueHashId (CausalHashId id) =
  queryOneCol loadMaybeCausalValueHashIdSql (Only id)

loadCausalHash :: DB m => CausalHashId -> m CausalHash
loadCausalHash (CausalHashId id) = CausalHash <$> loadHashHashById id

loadMaybeCausalValueHashId :: DB m => HashId -> m (Maybe BranchHashId)
loadMaybeCausalValueHashId id =
  queryMaybeCol loadMaybeCausalValueHashIdSql (Only id)

loadMaybeCausalValueHashIdSql :: Sql
loadMaybeCausalValueHashIdSql =
  [here| SELECT value_hash_id FROM causal WHERE self_hash_id = ? |]

isCausalHash :: DB m => HashId -> m Bool
isCausalHash = queryOneCol sql . Only where sql = [here|
    SELECT EXISTS (SELECT 1 FROM causal WHERE self_hash_id = ?)
  |]

loadBranchObjectIdByCausalHashId :: DB m => CausalHashId -> m (Maybe BranchObjectId)
loadBranchObjectIdByCausalHashId id = queryMaybeCol loadBranchObjectIdByCausalHashIdSql (Only id)

expectBranchObjectIdByCausalHashId :: DB m => CausalHashId -> m BranchObjectId
expectBranchObjectIdByCausalHashId id = queryOneCol loadBranchObjectIdByCausalHashIdSql (Only id)

loadBranchObjectIdByCausalHashIdSql :: Sql
loadBranchObjectIdByCausalHashIdSql =
  [here|
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
loadCausalParents h = queryListCol sql (Only h) where sql = [here|
  SELECT parent_id FROM causal_parent WHERE causal_id = ?
|]

loadNamespaceRoot :: DB m => m CausalHashId
loadNamespaceRoot =
  queryOneCol_ loadMaybeNamespaceRootSql

loadMaybeNamespaceRoot :: DB m => m (Maybe CausalHashId)
loadMaybeNamespaceRoot =
  queryMaybeCol_ loadMaybeNamespaceRootSql

loadMaybeNamespaceRootSql :: Sql
loadMaybeNamespaceRootSql =
  "SELECT causal_id FROM namespace_root"

setNamespaceRoot :: forall m. DB m => CausalHashId -> m ()
setNamespaceRoot id =
  queryOneCol_ "SELECT EXISTS (SELECT 1 FROM namespace_root)" >>= \case
    False -> execute insert (Only id)
    True -> execute update (Only id)
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

loadWatch :: (DB m, SqliteExceptionReason e) => WatchKind -> Reference.IdH -> (ByteString -> Either e a) -> m (Maybe a)
loadWatch k r check = queryMaybeColCheck sql (Only k :. r) check where sql = [here|
    SELECT result FROM watch_result
    INNER JOIN watch
      ON watch_result.hash_id = watch.hash_id
      AND watch_result.component_index = watch.component_index
    WHERE watch.watch_kind_id = ?
      AND watch.hash_id = ?
      AND watch.component_index = ?
  |]

loadWatchKindsByReference :: DB m => Reference.IdH -> m [WatchKind]
loadWatchKindsByReference r = queryListCol sql r where sql = [here|
    SELECT watch_kind_id FROM watch_result
    INNER JOIN watch
      ON watch_result.hash_id = watch.hash_id
      AND watch_result.component_index = watch.component_index
    WHERE watch.hash_id = ?
      AND watch.component_index = ?
  |]

loadWatchesByWatchKind :: DB m => WatchKind -> m [Reference.IdH]
loadWatchesByWatchKind k = queryListRow sql (Only k) where sql = [here|
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
getReferentsByType r = queryListRow sql r where sql = [here|
  SELECT
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  FROM find_type_index
  WHERE type_reference_builtin IS ?
    AND type_reference_hash_id IS ?
    AND type_reference_component_index IS ?
|]

getTypeReferenceForReferent :: DB m => Referent.Id -> m (Reference' TextId HashId)
getTypeReferenceForReferent r =
  queryOneRow sql r
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
getTypeReferencesForComponent :: DB m => ObjectId -> m [(Reference' TextId HashId, Referent.Id)]
getTypeReferencesForComponent oId =
  queryListRow sql (Only oId) <&> map fixupTypeIndexRow where sql = [here|
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
getReferentsByTypeMention r = queryListRow sql r where sql = [here|
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
getTypeMentionsReferencesForComponent :: DB m => ObjectId -> m [(Reference' TextId HashId, Referent.Id)]
getTypeMentionsReferencesForComponent r =
  queryListRow sql (Only r) <&> map fixupTypeIndexRow where sql = [here|
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
  filter isNotSelfReference <$> queryListRow sql dependency
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
  filter isNotSelfReference <$> queryListRow sql (Only dependency)
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
  filter isNotSelfReference <$> queryListRow sql dependent
  where
    sql =
      [here|
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
  filter isNotSelfReference <$> queryListRow sql dependent
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
objectIdByBase32Prefix objType prefix = queryListCol sql (objType, prefix <> "%") where sql = [here|
  SELECT object.id FROM object
  INNER JOIN hash_object ON hash_object.object_id = object.id
  INNER JOIN hash ON hash_object.hash_id = hash.id
  WHERE object.type_id = ?
    AND hash.base32 LIKE ?
|]

causalHashIdByBase32Prefix :: DB m => Text -> m [CausalHashId]
causalHashIdByBase32Prefix prefix = queryListCol sql (Only $ prefix <> "%") where sql = [here|
  SELECT self_hash_id FROM causal
  INNER JOIN hash ON id = self_hash_id
  WHERE base32 LIKE ?
|]

namespaceHashIdByBase32Prefix :: DB m => Text -> m [BranchHashId]
namespaceHashIdByBase32Prefix prefix = queryListCol sql (Only $ prefix <> "%") where sql = [here|
  SELECT value_hash_id FROM causal
  INNER JOIN hash ON id = value_hash_id
  WHERE base32 LIKE ?
|]

-- | Finds all causals that refer to a branch for which we don't have an object stored.
-- Although there are plans to support this in the future, currently all such cases
-- are the result of database inconsistencies and are unexpected.
getCausalsWithoutBranchObjects :: DB m => m [CausalHashId]
getCausalsWithoutBranchObjects = queryListCol_ sql
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

before :: DB m => CausalHashId -> CausalHashId -> m Bool
before chId1 chId2 = queryOneCol sql (chId2, chId1)
  where
    sql = fromString $ "SELECT EXISTS (" ++ ancestorSql ++ " WHERE ancestor.id = ?)"

-- the `Connection` arguments come second to fit the shape of Exception.bracket + uncurry curry
lca :: CausalHashId -> CausalHashId -> Connection -> Connection -> IO (Maybe CausalHashId)
lca x y cx cy =
  withStatement cx sql (Only x) \nextX ->
    withStatement cy sql (Only y) \nextY -> do
      let getNext = (,) <$> nextX <*> nextY
          loop2 seenX seenY =
            getNext >>= \case
              (Just (Only px), Just (Only py)) ->
                let seenX' = Set.insert px seenX
                    seenY' = Set.insert py seenY
                 in if Set.member px seenY'
                      then pure (Just px)
                      else
                        if Set.member py seenX'
                          then pure (Just py)
                          else loop2 seenX' seenY'
              (Nothing, Nothing) -> pure Nothing
              (Just (Only px), Nothing) -> loop1 nextX seenY px
              (Nothing, Just (Only py)) -> loop1 nextY seenX py
          loop1 getNext matches v =
            if Set.member v matches
              then pure (Just v)
              else
                getNext >>= \case
                  Just (Only v) -> loop1 getNext matches v
                  Nothing -> pure Nothing
      loop2 (Set.singleton x) (Set.singleton y)
  where
    sql = fromString ancestorSql

ancestorSql :: String
ancestorSql =
  [here|
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

-- * orphan instances

deriving via Text instance ToField Base32Hex

deriving via Text instance FromField Base32Hex

instance ToField WatchKind where
  toField = \case
    WatchKind.RegularWatch -> SQLInteger 0
    WatchKind.TestWatch -> SQLInteger 1

instance FromField WatchKind where
  fromField =
    fromField @Int8 <&> fmap \case
      0 -> WatchKind.RegularWatch
      1 -> WatchKind.TestWatch
      tag -> error $ "Unknown WatchKind id " ++ show tag
