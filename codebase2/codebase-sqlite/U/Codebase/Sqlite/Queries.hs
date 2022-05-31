-- | Some naming conventions used in this module:
--
-- * @32@: the base32 representation of a hash
-- * @expect@: retrieve something that's known to exist
-- * @load@: retrieve something that's not known to exist (so the return type is a Maybe, or another container that
--     could be empty)
-- * @save@: idempotent (on conflict do nothing) insert, and return the id of the thing (usually)
module U.Codebase.Sqlite.Queries
  ( -- * text table
    saveText,
    loadTextId,
    expectTextId,
    expectText,
    expectTextCheck,

    -- * hash table
    saveHash,
    saveHashHash,
    loadHashId,
    expectHash,
    expectHash32,
    expectBranchHash,
    loadHashIdByHash,
    expectHashIdByHash,
    saveCausalHash,
    expectCausalHash,
    saveBranchHash,

    -- * hash_object table
    saveHashObject,
    expectHashIdsForObject,
    hashIdWithVersionForObject,
    loadObjectIdForPrimaryHashId,
    expectObjectIdForPrimaryHashId,
    loadObjectIdForPrimaryHash,
    expectObjectIdForPrimaryHash,
    loadPatchObjectIdForPrimaryHash,
    loadObjectIdForAnyHash,
    loadObjectIdForAnyHashId,
    expectObjectIdForAnyHashId,
    recordObjectRehash,

    -- * object table
    saveObject,
    isObjectHash,
    expectObject,
    expectPrimaryHashByObjectId,
    expectPrimaryHashIdForObject,
    expectObjectWithHashIdAndType,
    expectDeclObject,
    loadDeclObject,
    expectNamespaceObject,
    loadNamespaceObject,
    expectPatchObject,
    loadPatchObject,
    loadTermObject,
    expectTermObject,

    -- * namespace_root table
    loadNamespaceRoot,
    setNamespaceRoot,
    expectNamespaceRoot,

    -- * causals

    -- ** causal table
    saveCausal,
    isCausalHash,
    expectCausal,
    loadCausalHashIdByCausalHash,
    expectCausalHashIdByCausalHash,
    expectCausalValueHashId,
    loadCausalByCausalHash,
    expectCausalByCausalHash,
    loadBranchObjectIdByCausalHashId,
    expectBranchObjectIdByCausalHashId,

    -- ** causal_parent table
    saveCausalParents,
    loadCausalParents,
    loadCausalParentsByHash,
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

    -- * Name Lookup
    resetNameLookupTables,
    insertTermNames,
    insertTypeNames,
    rootTermNames,
    rootTypeNames,

    -- * garbage collection
    garbageCollectObjectsWithoutHashes,
    garbageCollectWatchesWithoutObjects,

    -- * sync temp entities
    entityExists,
    expectEntity,
    getMissingDependentsForTempEntity,
    getMissingDependencyJwtsForTempEntity,
    tempToSyncEntity,
    syncToTempEntity,
    insertTempEntity,
    saveSyncEntity,
    deleteTempDependencies,

    -- * db misc
    createSchema,
    addTempEntityTables,
    schemaVersion,
    expectSchemaVersion,
    setSchemaVersion,
  )
where

import qualified Control.Lens as Lens
import Control.Monad.Extra ((||^))
import Data.Bitraversable (bitraverse)
import Data.Bytes.Put (runPutS)
import qualified Data.Foldable as Foldable
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.String.Here.Uninterpolated (here, hereFile)
import qualified Data.Vector as Vector
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import U.Codebase.Reference (Reference' (..))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.Branch.Format as NamespaceFormat
import qualified U.Codebase.Sqlite.Causal as Causal
import qualified U.Codebase.Sqlite.Causal as Sqlite.Causal
import U.Codebase.Sqlite.DbId
  ( BranchHashId (..),
    BranchObjectId (..),
    CausalHashId (..),
    HashId (..),
    HashVersion,
    ObjectId (..),
    PatchObjectId (..),
    SchemaVersion,
    TextId,
  )
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import U.Codebase.Sqlite.Decode
import U.Codebase.Sqlite.Entity (SyncEntity)
import qualified U.Codebase.Sqlite.Entity as Entity
import qualified U.Codebase.Sqlite.NamedRef as S
import U.Codebase.Sqlite.ObjectType (ObjectType (DeclComponent, Namespace, Patch, TermComponent))
import qualified U.Codebase.Sqlite.ObjectType as ObjectType
import U.Codebase.Sqlite.Orphans ()
import qualified U.Codebase.Sqlite.Patch.Format as PatchFormat
import qualified U.Codebase.Sqlite.Reference as Reference
import qualified U.Codebase.Sqlite.Referent as Referent
import U.Codebase.Sqlite.Serialization as Serialization
import U.Codebase.Sqlite.TempEntity (TempEntity)
import qualified U.Codebase.Sqlite.TempEntity as TempEntity
import U.Codebase.Sqlite.TempEntityType (TempEntityType)
import qualified U.Codebase.Sqlite.TempEntityType as TempEntityType
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Codebase.WatchKind (WatchKind)
import qualified U.Util.Alternative as Alternative
import U.Util.Base32Hex (Base32Hex (..))
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import Unison.Prelude
import Unison.Sqlite

-- * main squeeze

createSchema :: Transaction ()
createSchema = do
  executeFile [hereFile|sql/create.sql|]
  addTempEntityTables

addTempEntityTables :: Transaction ()
addTempEntityTables =
  executeFile [hereFile|sql/001-temp-entity-tables.sql|]

executeFile :: String -> Transaction ()
executeFile =
  traverse_ (execute_ . fromString) . filter (not . null) . List.splitOn ";"

schemaVersion :: Transaction SchemaVersion
schemaVersion = queryOneCol_ sql
  where
    sql = "SELECT version from schema_version;"

data UnexpectedSchemaVersion = UnexpectedSchemaVersion
  { actual :: SchemaVersion,
    expected :: SchemaVersion
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

-- | Expect the given schema version.
expectSchemaVersion :: SchemaVersion -> Transaction ()
expectSchemaVersion expected =
  queryOneColCheck_
    [here|
      SELECT version
      FROM schema_version
    |]
    (\actual -> if actual /= expected then Left UnexpectedSchemaVersion {actual, expected} else Right ())

setSchemaVersion :: SchemaVersion -> Transaction ()
setSchemaVersion schemaVersion = execute sql (Only schemaVersion)
  where
    sql = "UPDATE schema_version SET version = ?"

{- ORMOLU_DISABLE -}
{- Please don't try to format the SQL blocks —AI -}
countObjects :: Transaction Int
countObjects = queryOneCol_ [here| SELECT COUNT(*) FROM object |]

countCausals :: Transaction Int
countCausals = queryOneCol_ [here| SELECT COUNT(*) FROM causal |]

countWatches :: Transaction Int
countWatches = queryOneCol_ [here| SELECT COUNT(*) FROM watch |]

saveHash :: Base32Hex -> Transaction HashId
saveHash base32 = execute sql (Only base32) >> expectHashId base32
  where sql = [here|
    INSERT INTO hash (base32) VALUES (?)
    ON CONFLICT DO NOTHING
  |]

saveHashHash :: Hash -> Transaction HashId
saveHashHash = saveHash . Hash.toBase32Hex

loadHashId :: Base32Hex -> Transaction (Maybe HashId)
loadHashId base32 = queryMaybeCol loadHashIdSql (Only base32)

expectHashId :: Base32Hex -> Transaction HashId
expectHashId base32 = queryOneCol loadHashIdSql (Only base32)

loadHashIdSql :: Sql
loadHashIdSql =
  [here| SELECT id FROM hash WHERE base32 = ? |]

loadHashIdByHash :: Hash -> Transaction (Maybe HashId)
loadHashIdByHash = loadHashId . Hash.toBase32Hex

saveCausalHash :: CausalHash -> Transaction CausalHashId
saveCausalHash = fmap CausalHashId . saveHashHash . unCausalHash

saveBranchHash :: BranchHash -> Transaction BranchHashId
saveBranchHash = fmap BranchHashId . saveHashHash . unBranchHash

loadCausalHashIdByCausalHash :: CausalHash -> Transaction (Maybe CausalHashId)
loadCausalHashIdByCausalHash ch = runMaybeT do
  hId <- MaybeT $ loadHashIdByHash (unCausalHash ch)
  Alternative.whenM (lift (isCausalHash hId)) (CausalHashId hId)

expectCausalHashIdByCausalHash :: CausalHash -> Transaction CausalHashId
expectCausalHashIdByCausalHash ch = do
  hId <- expectHashIdByHash (unCausalHash ch)
  pure (CausalHashId hId)

loadCausalByCausalHash :: CausalHash -> Transaction (Maybe (CausalHashId, BranchHashId))
loadCausalByCausalHash ch = runMaybeT do
  hId <- MaybeT $ loadHashIdByHash (unCausalHash ch)
  bhId <- MaybeT $ loadCausalValueHashId hId
  pure (CausalHashId hId, bhId)

expectCausalByCausalHash :: CausalHash -> Transaction (CausalHashId, BranchHashId)
expectCausalByCausalHash ch = do
  hId <- expectCausalHashIdByCausalHash ch
  bhId <- expectCausalValueHashId hId
  pure (hId, bhId)

expectHashIdByHash :: Hash -> Transaction HashId
expectHashIdByHash = expectHashId . Hash.toBase32Hex

expectHash :: HashId -> Transaction Hash
expectHash h = Hash.fromBase32Hex <$> expectHash32 h

expectHash32 :: HashId -> Transaction Base32Hex
expectHash32 h = queryOneCol sql (Only h)
  where sql = [here| SELECT base32 FROM hash WHERE id = ? |]

expectBranchHash :: BranchHashId -> Transaction BranchHash
expectBranchHash = coerce expectHash

saveText :: Text -> Transaction TextId
saveText t = execute sql (Only t) >> expectTextId t
  where sql = [here| INSERT INTO text (text) VALUES (?) ON CONFLICT DO NOTHING|]

loadTextId :: Text -> Transaction (Maybe TextId)
loadTextId t = queryMaybeCol loadTextIdSql (Only t)

expectTextId :: Text -> Transaction TextId
expectTextId t = queryOneCol loadTextIdSql (Only t)

loadTextIdSql :: Sql
loadTextIdSql =
  [here| SELECT id FROM text WHERE text = ? |]

expectText :: TextId -> Transaction Text
expectText h = queryOneCol loadTextSql (Only h)

expectTextCheck :: SqliteExceptionReason e => TextId -> (Text -> Either e a) -> Transaction a
expectTextCheck h = queryOneColCheck loadTextSql (Only h)

loadTextSql :: Sql
loadTextSql =
  [here| SELECT text FROM text WHERE id = ? |]

saveHashObject :: HashId -> ObjectId -> HashVersion -> Transaction ()
saveHashObject hId oId version = execute sql (hId, oId, version) where
  sql = [here|
    INSERT INTO hash_object (hash_id, object_id, hash_version)
    VALUES (?, ?, ?)
    ON CONFLICT DO NOTHING
  |]

saveObject :: HashId -> ObjectType -> ByteString -> Transaction ObjectId
saveObject h t blob = do
  oId <- execute sql (h, t, blob) >> expectObjectIdForPrimaryHashId h
  saveHashObject h oId 2 -- todo: remove this from here, and add it to other relevant places once there are v1 and v2 hashes
  rowsModified >>= \case
    0 -> pure ()
    _ -> do
      hash <- expectHash32 h
      tryMoveTempEntityDependents hash
  pure oId
  where
  sql = [here|
    INSERT INTO object (primary_hash_id, type_id, bytes)
    VALUES (?, ?, ?)
    ON CONFLICT DO NOTHING
  |]

expectObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectObject oId check = do
 result <- queryOneColCheck sql (Only oId) check
 pure result
  where sql = [here|
  SELECT bytes FROM object WHERE id = ?
|]

loadObjectOfType ::
  SqliteExceptionReason e =>
  ObjectId ->
  ObjectType ->
  (ByteString -> Either e a) ->
  Transaction (Maybe a)
loadObjectOfType oid ty =
  queryMaybeColCheck loadObjectOfTypeSql (oid, ty)

expectObjectOfType :: SqliteExceptionReason e => ObjectId -> ObjectType -> (ByteString -> Either e a) -> Transaction a
expectObjectOfType oid ty =
  queryOneColCheck loadObjectOfTypeSql (oid, ty)

loadObjectOfTypeSql :: Sql
loadObjectOfTypeSql =
  [here|
    SELECT bytes
    FROM object
    WHERE id = ?
      AND type_id = ?
  |]

-- | Load a decl component object.
loadDeclObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction (Maybe a)
loadDeclObject oid =
  loadObjectOfType oid DeclComponent

-- | Expect a decl component object.
expectDeclObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectDeclObject oid =
  expectObjectOfType oid DeclComponent

-- | Load a namespace object.
loadNamespaceObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction (Maybe a)
loadNamespaceObject oid =
  loadObjectOfType oid Namespace

-- | Expect a namespace object.
expectNamespaceObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectNamespaceObject oid =
  expectObjectOfType oid Namespace

-- | Load a patch object.
loadPatchObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction (Maybe a)
loadPatchObject oid =
  loadObjectOfType oid Patch

-- | Expect a patch object.
expectPatchObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectPatchObject oid =
  expectObjectOfType oid Patch

-- | Load a term component object.
loadTermObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction (Maybe a)
loadTermObject oid =
  loadObjectOfType oid TermComponent

-- | Expect a term component object.
expectTermObject :: SqliteExceptionReason e => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectTermObject oid =
  expectObjectOfType oid TermComponent

expectPrimaryHashIdForObject :: ObjectId -> Transaction HashId
expectPrimaryHashIdForObject oId = do
  queryOneCol sql (Only oId)
  where
    sql = "SELECT primary_hash_id FROM object WHERE id = ?"

expectObjectWithType :: SqliteExceptionReason e => ObjectId -> (ObjectType -> ByteString -> Either e a) -> Transaction a
expectObjectWithType oId check = queryOneRowCheck sql (Only oId) (\(typ, bytes) -> check typ bytes)
  where sql = [here|
    SELECT type_id, bytes FROM object WHERE id = ?
  |]

expectObjectWithHashIdAndType :: ObjectId -> Transaction (HashId, ObjectType, ByteString)
expectObjectWithHashIdAndType oId = queryOneRow sql (Only oId)
  where sql = [here|
    SELECT primary_hash_id, type_id, bytes FROM object WHERE id = ?
  |]

loadObjectIdForPrimaryHashId :: HashId -> Transaction (Maybe ObjectId)
loadObjectIdForPrimaryHashId h =
  queryMaybeCol loadObjectIdForPrimaryHashIdSql (Only h)

-- | Not all hashes have corresponding objects; e.g., hashes of term types
expectObjectIdForPrimaryHashId :: HashId -> Transaction ObjectId
expectObjectIdForPrimaryHashId h =
  queryOneCol loadObjectIdForPrimaryHashIdSql (Only h)

loadObjectIdForPrimaryHashIdSql :: Sql
loadObjectIdForPrimaryHashIdSql =
  [here|
    SELECT id
    FROM object
    WHERE primary_hash_id = ?
  |]

loadObjectIdForPrimaryHash :: Hash -> Transaction (Maybe ObjectId)
loadObjectIdForPrimaryHash h =
  loadHashIdByHash h >>= \case
    Nothing -> pure Nothing
    Just hashId -> loadObjectIdForPrimaryHashId hashId

expectObjectIdForPrimaryHash :: Hash -> Transaction ObjectId
expectObjectIdForPrimaryHash =
  expectObjectIdForHash32 . Hash.toBase32Hex

expectObjectIdForHash32 :: Base32Hex -> Transaction ObjectId
expectObjectIdForHash32 hash = do
  hashId <- expectHashId hash
  expectObjectIdForPrimaryHashId hashId

expectBranchObjectIdForHash32 :: Base32Hex -> Transaction BranchObjectId
expectBranchObjectIdForHash32 =
  fmap BranchObjectId . expectObjectIdForHash32

expectPatchObjectIdForHash32 :: Base32Hex -> Transaction PatchObjectId
expectPatchObjectIdForHash32 =
  fmap PatchObjectId . expectObjectIdForHash32

expectBranchHashIdForHash32 :: Base32Hex -> Transaction BranchHashId
expectBranchHashIdForHash32 = queryOneCol sql . Only
  where
    sql =
      [here|
        SELECT hash.id FROM object
        INNER JOIN hash_object ON hash_object.object_id = object.id
        INNER JOIN hash ON hash_object.hash_id = hash.id
        WHERE object.type_id = 2
          AND hash.base32 = ?
      |]

expectCausalHashIdForHash32 :: Base32Hex -> Transaction CausalHashId
expectCausalHashIdForHash32 = queryOneCol sql . Only
  where
    sql =
      [here|
        SELECT self_hash_id
        FROM causal INNER JOIN hash ON hash.id = self_hash_id
        WHERE base32 = ?
      |]

loadPatchObjectIdForPrimaryHash :: PatchHash -> Transaction (Maybe PatchObjectId)
loadPatchObjectIdForPrimaryHash =
  (fmap . fmap) PatchObjectId . loadObjectIdForPrimaryHash . unPatchHash

loadObjectIdForAnyHash :: Hash -> Transaction (Maybe ObjectId)
loadObjectIdForAnyHash h =
  loadHashIdByHash h >>= \case
    Nothing -> pure Nothing
    Just hashId -> loadObjectIdForAnyHashId hashId

loadObjectIdForAnyHashId :: HashId -> Transaction (Maybe ObjectId)
loadObjectIdForAnyHashId h =
  queryMaybeCol loadObjectIdForAnyHashIdSql (Only h)

expectObjectIdForAnyHashId :: HashId -> Transaction ObjectId
expectObjectIdForAnyHashId h =
  queryOneCol loadObjectIdForAnyHashIdSql (Only h)

loadObjectIdForAnyHashIdSql :: Sql
loadObjectIdForAnyHashIdSql =
  [here| SELECT object_id FROM hash_object WHERE hash_id = ? |]

-- | Does a hash correspond to an object?
isObjectHash :: HashId -> Transaction Bool
isObjectHash h =
  queryOneCol sql (Only h)
  where
    sql = [here|
      SELECT EXISTS (SELECT 1 FROM object WHERE primary_hash_id = ?)
    |]

-- | All objects have corresponding hashes.
expectPrimaryHashByObjectId :: ObjectId -> Transaction Hash
expectPrimaryHashByObjectId =
  fmap Hash.fromBase32Hex . expectPrimaryHash32ByObjectId

expectPrimaryHash32ByObjectId :: ObjectId -> Transaction Base32Hex
expectPrimaryHash32ByObjectId oId = queryOneCol sql (Only oId)
 where sql = [here|
  SELECT hash.base32
  FROM hash INNER JOIN object ON object.primary_hash_id = hash.id
  WHERE object.id = ?
|]

expectHashIdsForObject :: ObjectId -> Transaction (NonEmpty HashId)
expectHashIdsForObject oId = do
  primaryHashId <- queryOneCol sql1 (Only oId)
  hashIds <- queryListCol sql2 (Only oId)
  pure $ primaryHashId Nel.:| filter (/= primaryHashId) hashIds
  where
    sql1 = "SELECT primary_hash_id FROM object WHERE id = ?"
    sql2 = "SELECT hash_id FROM hash_object WHERE object_id = ?"

hashIdWithVersionForObject :: ObjectId -> Transaction [(HashId, HashVersion)]
hashIdWithVersionForObject = queryListRow sql . Only where sql = [here|
  SELECT hash_id, hash_version FROM hash_object WHERE object_id = ?
|]

-- | @recordObjectRehash old new@ records that object @old@ was rehashed and inserted as a new object, @new@.
--
-- This function rewrites @old@'s @hash_object@ rows in place to point at the new object.
recordObjectRehash :: ObjectId -> ObjectId -> Transaction ()
recordObjectRehash old new =
  execute sql (new, old)
  where
    sql = [here|
      UPDATE hash_object
      SET object_id = ?
      WHERE object_id = ?
    |]

-- |Maybe we would generalize this to something other than NamespaceHash if we
-- end up wanting to store other kinds of Causals here too.
saveCausal :: CausalHashId -> BranchHashId -> [CausalHashId] -> Transaction ()
saveCausal self value parents = do
  execute insertCausalSql (self, value)
  rowsModified >>= \case
    0 -> pure ()
    _ -> do
      executeMany insertCausalParentsSql (fmap (self,) parents)
      flushCausalDependents self
  where
    insertCausalSql = [here|
      INSERT INTO causal (self_hash_id, value_hash_id)
      VALUES (?, ?)
      ON CONFLICT DO NOTHING
    |]
    insertCausalParentsSql = [here|
      INSERT INTO causal_parent (causal_id, parent_id) VALUES (?, ?)
    |]

flushCausalDependents :: CausalHashId -> Transaction ()
flushCausalDependents chId = do
  hash <- expectHash32 (unCausalHashId chId)
  tryMoveTempEntityDependents hash

--  Note: beef up insert_entity procedure to flush temp_entity table

-- | flushTempEntity does this:
--    1. When inserting object #foo,
--        look up all dependents of #foo in
--        temp_entity_missing_dependency table (say #bar, #baz).
--    2. Delete (#bar, #foo) and (#baz, #foo) from temp_entity_missing_dependency.
--    3. Delete #foo from temp_entity (if it's there)
--    4. For each like #bar and #baz with no more rows in temp_entity_missing_dependency,
--        insert_entity them.
--
-- Precondition: Must have inserted the entity with hash b32 already.
tryMoveTempEntityDependents :: Base32Hex -> Transaction ()
tryMoveTempEntityDependents dependencyBase32 = do
  dependents <- getMissingDependentsForTempEntity dependencyBase32
  executeMany deleteTempDependents (dependents <&> (,dependencyBase32))
  deleteTempEntity dependencyBase32
  traverse_ moveTempEntityToMain =<< tempEntitiesWithNoMissingDependencies
  where
    deleteTempDependents :: Sql
    deleteTempDependents = [here|
      DELETE FROM temp_entity_missing_dependency
      WHERE dependent = ?
        AND dependency = ?
    |]

    tempEntitiesWithNoMissingDependencies :: Transaction [Base32Hex]
    tempEntitiesWithNoMissingDependencies = queryListCol_ [here|
      SELECT hash
      FROM temp_entity
      WHERE NOT EXISTS(
        SELECT 1
        FROM temp_entity_missing_dependency dep
        WHERE dep.dependent = temp_entity.hash
      )
    |]

expectCausal :: CausalHashId -> Transaction Causal.SyncCausalFormat
expectCausal hashId = do
  valueHash <-
    queryOneCol
      [here|
        SELECT value_hash_id
        FROM causal
        WHERE self_hash_id = ?
      |]
      (Only hashId)
  parents <-
    fmap Vector.fromList do
      -- is the random ordering from the database ok? (seems so, for now)
      queryListCol
        [here|
          SELECT parent_id
          FROM causal_parent
          WHERE causal_id = ?
        |]
        (Only hashId)
  pure Causal.SyncCausalFormat {parents, valueHash}

-- | Read an entity out of main storage.
expectEntity :: Base32Hex -> Transaction SyncEntity
expectEntity hash = do
  hashId <- expectHashId hash
  -- We don't know if this is an object or a causal, so just try one, then the other.
  loadObjectIdForPrimaryHashId hashId >>= \case
    Nothing -> Entity.C <$> expectCausal (CausalHashId hashId)
    Just objectId ->
      expectObjectWithType objectId \typ bytes ->
        case typ of
          TermComponent -> Entity.TC <$> decodeSyncTermFormat bytes
          DeclComponent -> Entity.DC <$> decodeSyncDeclFormat bytes
          Namespace -> Entity.N <$> decodeSyncNamespaceFormat bytes
          Patch -> Entity.P <$> decodeSyncPatchFormat bytes

moveTempEntityToMain :: Base32Hex -> Transaction ()
moveTempEntityToMain b32 = do
  t <- expectTempEntity b32
  r <- tempToSyncEntity t
  _ <- saveSyncEntity b32 r
  pure ()

-- | Read an entity out of temp storage.
expectTempEntity :: Base32Hex -> Transaction TempEntity
expectTempEntity b32 = do
  queryOneRowCheck sql (Only b32) \(blob, typeId) ->
    case typeId of
      TempEntityType.TermComponentType -> Entity.TC <$> decodeTempTermFormat blob
      TempEntityType.DeclComponentType -> Entity.DC <$> decodeTempDeclFormat blob
      TempEntityType.NamespaceType -> Entity.N <$> decodeTempNamespaceFormat blob
      TempEntityType.PatchType -> Entity.P <$> decodeTempPatchFormat blob
      TempEntityType.CausalType -> Entity.C <$> decodeTempCausalFormat blob
  where sql = [here|
    SELECT blob, type_id
    FROM temp_entity
    WHERE hash = ?
  |]

tempToSyncEntity :: TempEntity -> Transaction SyncEntity
tempToSyncEntity = \case
  Entity.TC term -> Entity.TC <$> tempToSyncTermComponent term
  Entity.DC decl -> Entity.DC <$> tempToSyncDeclComponent decl
  Entity.N namespace -> Entity.N <$> tempToSyncNamespace namespace
  Entity.P patch -> Entity.P <$> tempToSyncPatch patch
  Entity.C causal -> Entity.C <$> tempToSyncCausal causal
  where
    tempToSyncCausal :: TempEntity.TempCausalFormat -> Transaction Causal.SyncCausalFormat
    tempToSyncCausal Causal.SyncCausalFormat {valueHash, parents} =
      Causal.SyncCausalFormat
        <$> expectBranchHashIdForHash32 valueHash
        <*> traverse expectCausalHashIdForHash32 parents

    tempToSyncDeclComponent :: TempEntity.TempDeclFormat -> Transaction DeclFormat.SyncDeclFormat
    tempToSyncDeclComponent = \case
      DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent decls) ->
        DeclFormat.SyncDecl . DeclFormat.SyncLocallyIndexedComponent
          <$> Lens.traverseOf (traverse . Lens._1) (bitraverse saveText expectObjectIdForHash32) decls

    tempToSyncNamespace :: TempEntity.TempNamespaceFormat -> Transaction NamespaceFormat.SyncBranchFormat
    tempToSyncNamespace = \case
      NamespaceFormat.SyncFull localIds bytes ->
        NamespaceFormat.SyncFull <$> tempToSyncNamespaceLocalIds localIds <*> pure bytes
      NamespaceFormat.SyncDiff parent localIds bytes ->
        NamespaceFormat.SyncDiff
          <$> expectBranchObjectIdForHash32 parent
          <*> tempToSyncNamespaceLocalIds localIds
          <*> pure bytes

    tempToSyncNamespaceLocalIds :: TempEntity.TempNamespaceLocalIds -> Transaction NamespaceFormat.BranchLocalIds
    tempToSyncNamespaceLocalIds (NamespaceFormat.LocalIds texts defns patches children) =
      NamespaceFormat.LocalIds
        <$> traverse saveText texts
        <*> traverse expectObjectIdForHash32 defns
        <*> traverse expectPatchObjectIdForHash32 patches
        <*> traverse
          ( \(branch, causal) ->
              (,)
                <$> expectBranchObjectIdForHash32 branch
                <*> expectCausalHashIdForHash32 causal
          )
          children

    tempToSyncPatch :: TempEntity.TempPatchFormat -> Transaction PatchFormat.SyncPatchFormat
    tempToSyncPatch = \case
      PatchFormat.SyncFull localIds bytes -> PatchFormat.SyncFull <$> tempToSyncPatchLocalIds localIds <*> pure bytes
      PatchFormat.SyncDiff parent localIds bytes ->
        PatchFormat.SyncDiff
          <$> expectPatchObjectIdForHash32 parent
          <*> tempToSyncPatchLocalIds localIds
          <*> pure bytes

    tempToSyncPatchLocalIds :: TempEntity.TempPatchLocalIds -> Transaction PatchFormat.PatchLocalIds
    tempToSyncPatchLocalIds (PatchFormat.LocalIds texts hashes defns) =
      PatchFormat.LocalIds
        <$> traverse saveText texts
        <*> traverse saveHash hashes
        <*> traverse expectObjectIdForHash32 defns

    tempToSyncTermComponent :: TempEntity.TempTermFormat -> Transaction TermFormat.SyncTermFormat
    tempToSyncTermComponent = \case
      TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent terms) ->
        TermFormat.SyncTerm . TermFormat.SyncLocallyIndexedComponent
          <$> Lens.traverseOf (traverse . Lens._1) (bitraverse saveText expectObjectIdForHash32) terms

syncToTempEntity :: SyncEntity -> Transaction TempEntity
syncToTempEntity = \case
  Entity.TC term -> Entity.TC <$> syncToTempTermComponent term
  Entity.DC decl -> Entity.DC <$> syncToTempDeclComponent decl
  Entity.N namespace -> Entity.N <$> syncToTempNamespace namespace
  Entity.P patch -> Entity.P <$> syncToTempPatch patch
  Entity.C causal -> Entity.C <$> syncToTempCausal causal
  where
    syncToTempCausal :: Causal.SyncCausalFormat -> Transaction TempEntity.TempCausalFormat
    syncToTempCausal Causal.SyncCausalFormat {valueHash, parents} =
      Causal.SyncCausalFormat
        <$> expectHash32 (unBranchHashId valueHash)
        <*> traverse (expectHash32 . unCausalHashId) parents

    syncToTempDeclComponent :: DeclFormat.SyncDeclFormat -> Transaction TempEntity.TempDeclFormat
    syncToTempDeclComponent = \case
      DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent decls) ->
        DeclFormat.SyncDecl . DeclFormat.SyncLocallyIndexedComponent
          <$> Lens.traverseOf (traverse . Lens._1) (bitraverse expectText expectPrimaryHash32ByObjectId) decls

    syncToTempNamespace :: NamespaceFormat.SyncBranchFormat -> Transaction TempEntity.TempNamespaceFormat
    syncToTempNamespace = \case
      NamespaceFormat.SyncFull localIds bytes ->
        NamespaceFormat.SyncFull <$> syncToTempNamespaceLocalIds localIds <*> pure bytes
      NamespaceFormat.SyncDiff parent localIds bytes ->
        NamespaceFormat.SyncDiff
          <$> expectPrimaryHash32ByObjectId (unBranchObjectId parent)
          <*> syncToTempNamespaceLocalIds localIds
          <*> pure bytes

    syncToTempNamespaceLocalIds :: NamespaceFormat.BranchLocalIds -> Transaction TempEntity.TempNamespaceLocalIds
    syncToTempNamespaceLocalIds (NamespaceFormat.LocalIds texts defns patches children) =
      NamespaceFormat.LocalIds
        <$> traverse expectText texts
        <*> traverse expectPrimaryHash32ByObjectId defns
        <*> traverse (expectPrimaryHash32ByObjectId . unPatchObjectId) patches
        <*> traverse
          ( \(branch, causal) ->
              (,)
                <$> expectPrimaryHash32ByObjectId (unBranchObjectId branch)
                <*> expectHash32 (unCausalHashId causal)
          )
          children

    syncToTempPatch :: PatchFormat.SyncPatchFormat -> Transaction TempEntity.TempPatchFormat
    syncToTempPatch = \case
      PatchFormat.SyncFull localIds bytes -> PatchFormat.SyncFull <$> syncToTempPatchLocalIds localIds <*> pure bytes
      PatchFormat.SyncDiff parent localIds bytes ->
        PatchFormat.SyncDiff
          <$> expectPrimaryHash32ByObjectId (unPatchObjectId parent)
          <*> syncToTempPatchLocalIds localIds
          <*> pure bytes

    syncToTempPatchLocalIds :: PatchFormat.PatchLocalIds -> Transaction TempEntity.TempPatchLocalIds
    syncToTempPatchLocalIds (PatchFormat.LocalIds texts hashes defns) =
      PatchFormat.LocalIds
        <$> traverse expectText texts
        <*> traverse expectHash32 hashes
        <*> traverse expectPrimaryHash32ByObjectId defns

    syncToTempTermComponent :: TermFormat.SyncTermFormat -> Transaction TempEntity.TempTermFormat
    syncToTempTermComponent = \case
      TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent terms) ->
        TermFormat.SyncTerm . TermFormat.SyncLocallyIndexedComponent
          <$> Lens.traverseOf (traverse . Lens._1) (bitraverse expectText expectPrimaryHash32ByObjectId) terms

saveSyncEntity :: Base32Hex -> SyncEntity -> Transaction (Either CausalHashId ObjectId)
saveSyncEntity b32Hex entity = do
  hashId <- saveHash b32Hex
  case entity of
    Entity.TC stf -> do
      let bytes = runPutS (Serialization.recomposeTermFormat stf)
      Right <$> saveObject hashId ObjectType.TermComponent bytes
    Entity.DC sdf -> do
      let bytes = runPutS (Serialization.recomposeDeclFormat sdf)
      Right <$> saveObject hashId ObjectType.DeclComponent bytes
    Entity.N sbf -> do
      let bytes = runPutS (Serialization.recomposeBranchFormat sbf)
      Right <$> saveObject hashId ObjectType.Namespace bytes
    Entity.P spf -> do
      let bytes = runPutS (Serialization.recomposePatchFormat spf)
      Right <$> saveObject hashId ObjectType.Patch bytes
    Entity.C scf -> case scf of
      Sqlite.Causal.SyncCausalFormat{valueHash, parents} -> do
        let causalHashId = CausalHashId hashId
        saveCausal causalHashId valueHash (Foldable.toList parents)
        pure $ Left causalHashId

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

expectCausalValueHashId :: CausalHashId -> Transaction BranchHashId
expectCausalValueHashId (CausalHashId id) =
  queryOneCol loadCausalValueHashIdSql (Only id)

expectCausalHash :: CausalHashId -> Transaction CausalHash
expectCausalHash = coerce expectHash

loadCausalValueHashId :: HashId -> Transaction (Maybe BranchHashId)
loadCausalValueHashId id =
  queryMaybeCol loadCausalValueHashIdSql (Only id)

loadCausalValueHashIdSql :: Sql
loadCausalValueHashIdSql =
  [here| SELECT value_hash_id FROM causal WHERE self_hash_id = ? |]

isCausalHash :: HashId -> Transaction Bool
isCausalHash = queryOneCol sql . Only where sql = [here|
    SELECT EXISTS (SELECT 1 FROM causal WHERE self_hash_id = ?)
  |]

loadBranchObjectIdByCausalHashId :: CausalHashId -> Transaction (Maybe BranchObjectId)
loadBranchObjectIdByCausalHashId id = queryMaybeCol loadBranchObjectIdByCausalHashIdSql (Only id)

expectBranchObjectIdByCausalHashId :: CausalHashId -> Transaction BranchObjectId
expectBranchObjectIdByCausalHashId id = queryOneCol loadBranchObjectIdByCausalHashIdSql (Only id)

loadBranchObjectIdByCausalHashIdSql :: Sql
loadBranchObjectIdByCausalHashIdSql =
  [here|
    SELECT object_id FROM hash_object
    INNER JOIN causal ON hash_id = causal.value_hash_id
    WHERE causal.self_hash_id = ?
  |]

saveCausalParents :: CausalHashId -> [CausalHashId] -> Transaction ()
saveCausalParents child parents = executeMany sql $ (child,) <$> parents where
  sql = [here|
    INSERT INTO causal_parent (causal_id, parent_id) VALUES (?, ?)
    ON CONFLICT DO NOTHING
  |]

loadCausalParents :: CausalHashId -> Transaction [CausalHashId]
loadCausalParents h = queryListCol sql (Only h) where sql = [here|
  SELECT parent_id FROM causal_parent WHERE causal_id = ?
|]

-- | Like 'loadCausalParents', but the input and outputs are hashes, not hash ids.
loadCausalParentsByHash :: Base32Hex -> Transaction [Base32Hex]
loadCausalParentsByHash hash =
  queryListCol
    [here|
      SELECT h2.base32
      FROM causal_parent cp
      JOIN hash h1 ON cp.causal_id = h1.id
      JOIN hash h2 ON cp.parent_id = h2.id
      WHERE h1.base32 = ?
    |]
    (Only hash)

expectNamespaceRoot :: Transaction CausalHashId
expectNamespaceRoot =
  queryOneCol_ loadNamespaceRootSql

loadNamespaceRoot :: Transaction (Maybe CausalHashId)
loadNamespaceRoot =
  queryMaybeCol_ loadNamespaceRootSql

loadNamespaceRootSql :: Sql
loadNamespaceRootSql =
  [here|
    SELECT causal_id
    FROM namespace_root
  |]

setNamespaceRoot :: CausalHashId -> Transaction ()
setNamespaceRoot id =
  queryOneCol_ "SELECT EXISTS (SELECT 1 FROM namespace_root)" >>= \case
    False -> execute insert (Only id)
    True -> execute update (Only id)
  where
    insert = "INSERT INTO namespace_root VALUES (?)"
    update = "UPDATE namespace_root SET causal_id = ?"

saveWatch :: WatchKind -> Reference.IdH -> ByteString -> Transaction ()
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

loadWatch ::
  SqliteExceptionReason e =>
  WatchKind ->
  Reference.IdH ->
  (ByteString -> Either e a) ->
  Transaction (Maybe a)
loadWatch k r check = queryMaybeColCheck sql (Only k :. r) check where sql = [here|
    SELECT result FROM watch_result
    INNER JOIN watch
      ON watch_result.hash_id = watch.hash_id
      AND watch_result.component_index = watch.component_index
    WHERE watch.watch_kind_id = ?
      AND watch.hash_id = ?
      AND watch.component_index = ?
  |]

loadWatchKindsByReference :: Reference.IdH -> Transaction [WatchKind]
loadWatchKindsByReference r = queryListCol sql r where sql = [here|
    SELECT watch_kind_id FROM watch_result
    INNER JOIN watch
      ON watch_result.hash_id = watch.hash_id
      AND watch_result.component_index = watch.component_index
    WHERE watch.hash_id = ?
      AND watch.component_index = ?
  |]

loadWatchesByWatchKind :: WatchKind -> Transaction [Reference.IdH]
loadWatchesByWatchKind k = queryListRow sql (Only k) where sql = [here|
  SELECT hash_id, component_index FROM watch WHERE watch_kind_id = ?
|]

clearWatches :: Transaction ()
clearWatches = do
  execute_ "DELETE FROM watch_result"
  execute_ "DELETE FROM watch"

-- * Index-building
addToTypeIndex :: Reference' TextId HashId -> Referent.Id -> Transaction ()
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

getReferentsByType :: Reference' TextId HashId -> Transaction [Referent.Id]
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

getTypeReferenceForReferent :: Referent.Id -> Transaction (Reference' TextId HashId)
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
getTypeReferencesForComponent :: ObjectId -> Transaction [(Reference' TextId HashId, Referent.Id)]
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

addToTypeMentionsIndex :: Reference' TextId HashId -> Referent.Id -> Transaction ()
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

getReferentsByTypeMention :: Reference' TextId HashId -> Transaction [Referent.Id]
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
getTypeMentionsReferencesForComponent :: ObjectId -> Transaction [(Reference' TextId HashId, Referent.Id)]
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
garbageCollectObjectsWithoutHashes :: Transaction ()
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
garbageCollectWatchesWithoutObjects :: Transaction ()
garbageCollectWatchesWithoutObjects = do
  execute_
    [here|
      DELETE FROM watch
      WHERE watch.hash_id NOT IN
      (SELECT hash_object.hash_id FROM hash_object)
    |]

addToDependentsIndex :: Reference.Reference -> Reference.Id -> Transaction ()
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
getDependentsForDependency :: Reference.Reference -> Transaction [Reference.Id]
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

getDependentsForDependencyComponent :: ObjectId -> Transaction [Reference.Id]
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
getDependenciesForDependent :: Reference.Id -> Transaction [Reference.Reference]
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
getDependencyIdsForDependent :: Reference.Id -> Transaction [Reference.Id]
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

objectIdByBase32Prefix :: ObjectType -> Text -> Transaction [ObjectId]
objectIdByBase32Prefix objType prefix = queryListCol sql (objType, prefix <> "%") where sql = [here|
  SELECT object.id FROM object
  INNER JOIN hash_object ON hash_object.object_id = object.id
  INNER JOIN hash ON hash_object.hash_id = hash.id
  WHERE object.type_id = ?
    AND hash.base32 LIKE ?
|]

causalHashIdByBase32Prefix :: Text -> Transaction [CausalHashId]
causalHashIdByBase32Prefix prefix = queryListCol sql (Only $ prefix <> "%") where sql = [here|
  SELECT self_hash_id FROM causal
  INNER JOIN hash ON id = self_hash_id
  WHERE base32 LIKE ?
|]

namespaceHashIdByBase32Prefix :: Text -> Transaction [BranchHashId]
namespaceHashIdByBase32Prefix prefix = queryListCol sql (Only $ prefix <> "%") where sql = [here|
  SELECT value_hash_id FROM causal
  INNER JOIN hash ON id = value_hash_id
  WHERE base32 LIKE ?
|]

-- | Finds all causals that refer to a branch for which we don't have an object stored.
-- Although there are plans to support this in the future, currently all such cases
-- are the result of database inconsistencies and are unexpected.
getCausalsWithoutBranchObjects :: Transaction [CausalHashId]
getCausalsWithoutBranchObjects = queryListCol_ sql
  where sql = [here|
    SELECT self_hash_id from causal
    WHERE value_hash_id NOT IN (SELECT hash_id FROM hash_object)
|]

{- ORMOLU_ENABLE -}

-- | Delete all hash objects of a given hash version.
-- Leaves the corresponding `hash`es in the hash table alone.
removeHashObjectsByHashingVersion :: HashVersion -> Transaction ()
removeHashObjectsByHashingVersion hashVersion =
  execute sql (Only hashVersion)
  where
    sql =
      [here|
    DELETE FROM hash_object
      WHERE hash_version = ?
|]

-- | Drop and recreate the name lookup tables. Use this when resetting names to a new branch.
resetNameLookupTables :: Transaction ()
resetNameLookupTables = do
  execute_ "DROP TABLE IF EXISTS term_name_lookup"
  execute_ "DROP TABLE IF EXISTS type_name_lookup"
  execute_
    [here|
      CREATE TABLE term_name_lookup (
        reversed_name TEXT NOT NULL, -- e.g. map.List.base
        referent_builtin INTEGER NULL,
        referent_object_id INTEGER NULL,
        referent_component_index INTEGER NULL,
        referent_constructor_index INTEGER NULL,
        referent_constructor_type INTEGER NULL,
        PRIMARY KEY (reversed_name, referent_builtin, referent_object_id, referent_component_index, referent_constructor_index)
      )
    |]
  -- Don't need this index at the moment, but will likely be useful later.
  -- execute_
  --   [here|
  --     CREATE INDEX IF NOT EXISTS term_name_by_referent_lookup ON term_name_lookup(referent_builtin, referent_object_id, referent_component_index, referent_constructor_index)
  --   |]
  execute_
    [here|
      CREATE TABLE type_name_lookup (
        reversed_name TEXT NOT NULL, -- e.g. map.List.base
        reference_builtin INTEGER NULL,
        reference_object_id INTEGER NULL,
        reference_component_index INTEGER NULL,
        PRIMARY KEY (reversed_name, reference_builtin, reference_object_id, reference_component_index)
      );
    |]

-- Don't need this index at the moment, but will likely be useful later.
-- execute_
--   [here|
--     CREATE INDEX IF NOT EXISTS type_name_by_reference_lookup ON type_name_lookup(reference_builtin, reference_object_id, reference_component_index);
--   |]

-- | Insert the given set of term names into the name lookup table
insertTermNames :: [S.NamedRef (Referent.TextReferent, Maybe S.ConstructorType)] -> Transaction ()
insertTermNames names = do
  executeMany sql (fmap asRow <$> names)
  where
    asRow (a, b) = a :. Only b
    sql =
      [here|
      INSERT INTO term_name_lookup (reversed_name, referent_builtin, referent_object_id, referent_component_index, referent_constructor_index, referent_constructor_type)
        VALUES (?, ?, ?, ?, ?, ?)
        ON CONFLICT DO NOTHING
        |]

-- | Insert the given set of type names into the name lookup table
insertTypeNames :: [S.NamedRef (Reference.TextReference)] -> Transaction ()
insertTypeNames names =
  executeMany sql names
  where
    sql =
      [here|
      INSERT INTO type_name_lookup (reversed_name, reference_builtin, reference_object_id, reference_component_index)
        VALUES (?, ?, ?, ?)
        ON CONFLICT DO NOTHING
        |]

-- | Get the list of a term names in the root namespace according to the name lookup index
rootTermNames :: Transaction [S.NamedRef (Referent.TextReferent, Maybe S.ConstructorType)]
rootTermNames = do
  (fmap . fmap) unRow <$> queryListRow_ sql
  where
    unRow (a :. Only b) = (a, b)
    sql =
      [here|
        SELECT reversed_name, referent_builtin, referent_object_id, referent_component_index, referent_constructor_index, referent_constructor_type FROM term_name_lookup
          ORDER BY reversed_name ASC
        |]

-- | Get the list of a type names in the root namespace according to the name lookup index
rootTypeNames :: Transaction [S.NamedRef Reference.TextReference]
rootTypeNames = do
  queryListRow_ sql
  where
    sql =
      [here|
        SELECT reversed_name, reference_builtin, reference_object_id, reference_component_index FROM type_name_lookup
        ORDER BY reversed_name ASC
        |]

before :: CausalHashId -> CausalHashId -> Transaction Bool
before chId1 chId2 = queryOneCol sql (chId2, chId1)
  where
    sql = fromString $ "SELECT EXISTS (" ++ ancestorSql ++ " WHERE ancestor.id = ?)"

lca :: CausalHashId -> CausalHashId -> Transaction (Maybe CausalHashId)
lca x y =
  queryStreamCol sql (Only x) \nextX ->
    queryStreamCol sql (Only y) \nextY -> do
      let getNext = (,) <$> nextX <*> nextY
          loop2 seenX seenY =
            getNext >>= \case
              (Just px, Just py) ->
                let seenX' = Set.insert px seenX
                    seenY' = Set.insert py seenY
                 in if Set.member px seenY'
                      then pure (Just px)
                      else
                        if Set.member py seenX'
                          then pure (Just py)
                          else loop2 seenX' seenY'
              (Nothing, Nothing) -> pure Nothing
              (Just px, Nothing) -> loop1 nextX seenY px
              (Nothing, Just py) -> loop1 nextY seenX py
          loop1 getNext matches v =
            if Set.member v matches
              then pure (Just v)
              else
                getNext >>= \case
                  Just v -> loop1 getNext matches v
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

-- * share sync / temp entities

-- | Does this entity already exist in the database, i.e. in the `object` or `causal` table?
entityExists :: Base32Hex -> Transaction Bool
entityExists hash = do
  -- first get hashId if exists
  loadHashId hash >>= \case
    Nothing -> pure False
    -- then check if is causal hash or if object exists for hash id
    Just hashId -> isCausalHash hashId ||^ isObjectHash hashId

getMissingDependencyJwtsForTempEntity :: Base32Hex -> Transaction (Maybe (NESet Text))
getMissingDependencyJwtsForTempEntity h = do
  jwts <-
    queryListCol
      [here|
        SELECT dependencyJwt FROM temp_entity_missing_dependency
        WHERE dependent = ?
      |]
      (Only h)
  pure (NESet.nonEmptySet (Set.fromList jwts))

getMissingDependentsForTempEntity :: Base32Hex -> Transaction [Base32Hex]
getMissingDependentsForTempEntity h =
  queryListCol
    [here|
      SELECT dependent
      FROM temp_entity_missing_dependency
      WHERE dependency = ?
    |]
    (Only h)

-- | Insert a new `temp_entity` row, and its associated 1+ `temp_entity_missing_dependency` rows.
--
-- Preconditions:
--   1. The entity does not already exist in "main" storage (`object` / `causal`)
--   2. The entity does not already exist in `temp_entity`.
insertTempEntity :: Base32Hex -> TempEntity -> NESet (Base32Hex, Text) -> Transaction ()
insertTempEntity entityHash entity missingDependencies = do
  execute
    [here|
      INSERT INTO temp_entity (hash, blob, type_id)
      VALUES (?, ?, ?)
    |]
    (entityHash, entityBlob, entityType)

  executeMany
    [here|
      INSERT INTO temp_entity_missing_dependency (dependent, dependency, dependencyJwt)
      VALUES (?, ?, ?)
    |]
    (map (\(depHash, depHashJwt) -> (entityHash, depHash, depHashJwt)) (Foldable.toList missingDependencies))
  where
    entityBlob :: ByteString
    entityBlob =
      runPutS (Serialization.putTempEntity entity)

    entityType :: TempEntityType
    entityType =
      Entity.entityType entity

-- | Delete a row from the `temp_entity` table, if it exists.
deleteTempEntity :: Base32Hex -> Transaction ()
deleteTempEntity hash =
  execute
    [here|
      DELETE
      FROM temp_entity
      WHERE hash = ?
    |]
    (Only hash)

-- | takes a dependent's hash and multiple dependency hashes
deleteTempDependencies :: Foldable f => Base32Hex -> f Base32Hex -> Transaction ()
deleteTempDependencies dependent (Foldable.toList -> dependencies) =
  executeMany sql (map (dependent,) dependencies)
  where
    sql =
      [here|
        DELETE FROM temp_entity_missing_dependencies
        WHERE dependent = ?
          AND dependency = ?
      |]
