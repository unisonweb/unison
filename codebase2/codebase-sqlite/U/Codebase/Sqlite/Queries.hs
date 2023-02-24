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
    saveTexts,
    loadTextId,
    expectTextId,
    expectText,
    expectTextCheck,

    -- * hash table
    saveHash,
    saveHashes,
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
    expectNamespaceRootBranchHashId,

    -- * namespace_statistics table
    saveNamespaceStats,
    loadNamespaceStatsByHashId,

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
    loadBranchObjectIdByBranchHashId,
    expectBranchObjectIdByCausalHashId,
    expectBranchObjectIdByBranchHashId,

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

    -- * projects
    Project (..),
    projectExists,
    projectExistsByName,
    expectProject,
    loadProjectByName,
    insertProject,
    Branch (..),
    projectBranchExistsByName,
    expectProjectBranch,
    loadProjectBranchByName,
    loadProjectAndBranchNames,
    insertProjectBranch,
    markProjectBranchChild,
    loadProject,
    loadProjectBranch,

    -- ** remote projects
    RemoteProject (..),
    RemoteProjectBranch (..),
    loadRemoteProject,
    ensureRemoteProject,
    setRemoteProjectName,
    loadRemoteProjectBranchByLocalProjectBranch,
    loadDefaultMergeTargetForLocalProjectBranch,

    -- ** remote project branches
    loadRemoteBranch,
    ensureRemoteProjectBranch,
    setRemoteProjectBranchName,
    insertBranchRemoteMapping,
    ensureBranchRemoteMapping,

    -- * indexes

    -- ** dependents index
    addToDependentsIndex,
    DependentsSelector (..),
    getDependentsForDependency,
    getDependentsForDependencyComponent,
    getDependenciesForDependent,
    getDependencyIdsForDependent,
    getDependenciesBetweenTerms,

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
    copyScopedNameLookup,
    dropNameLookupTables,
    insertScopedTermNames,
    insertScopedTypeNames,
    removeScopedTermNames,
    removeScopedTypeNames,
    termNamesWithinNamespace,
    typeNamesWithinNamespace,
    checkBranchHashNameLookupExists,
    trackNewBranchHashNameLookup,

    -- * Reflog
    appendReflog,
    getReflog,

    -- * garbage collection
    garbageCollectObjectsWithoutHashes,
    garbageCollectWatchesWithoutObjects,

    -- * sync temp entities
    EntityLocation (..),
    entityExists,
    entityLocation,
    expectEntity,
    syncToTempEntity,
    insertTempEntity,
    saveTempEntityInMain,
    expectTempEntity,
    deleteTempEntity,

    -- * elaborate hashes
    elaborateHashes,

    -- * migrations
    createSchema,
    addTempEntityTables,
    addReflogTable,
    addNamespaceStatsTables,
    addProjectTables,

    -- ** schema version
    currentSchemaVersion,
    expectSchemaVersion,
    setSchemaVersion,

    -- ** helpers for various migrations
    countObjects,
    countCausals,
    countWatches,
    getCausalsWithoutBranchObjects,
    removeHashObjectsByHashingVersion,

    -- * db misc
    addTypeMentionsToIndexForTerm,
    addTypeToIndexForTerm,
    c2xTerm,
    localIdsToLookups,
    s2cDecl,
    s2cTermWithType,
    saveDeclComponent,
    saveReferenceH,
    saveSyncEntity,
    saveTermComponent,
    schemaVersion,
    x2cTType,
    x2cTerm,
    checkBranchExistsForCausalHash,
  )
where

import Control.Lens (Lens')
import qualified Control.Lens as Lens
import Control.Monad.Extra ((||^))
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Writer (MonadWriter, runWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (bitraverse)
import Data.Bytes.Put (runPutS)
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.String.Here.Uninterpolated (here, hereFile)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import NeatInterpolation (trimming)
import U.Codebase.Branch.Type (NamespaceStats (..))
import qualified U.Codebase.Decl as C
import qualified U.Codebase.Decl as C.Decl
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import U.Codebase.Reference (Reference' (..))
import qualified U.Codebase.Reference as C
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import qualified U.Codebase.Reflog as Reflog
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
    ProjectBranchId (..),
    ProjectId (..),
    RemoteProjectBranchId,
    RemoteProjectId (..),
    SchemaVersion,
    TextId,
  )
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import qualified U.Codebase.Sqlite.Decl.Format as S.Decl
import U.Codebase.Sqlite.Decode
import U.Codebase.Sqlite.Entity (SyncEntity)
import qualified U.Codebase.Sqlite.Entity as Entity
import U.Codebase.Sqlite.HashHandle (HashHandle (..))
import U.Codebase.Sqlite.LocalIds
  ( LocalDefnId (..),
    LocalIds,
    LocalIds' (..),
    LocalTextId (..),
  )
import qualified U.Codebase.Sqlite.LocalIds as LocalIds
import U.Codebase.Sqlite.NamedRef (NamedRef)
import qualified U.Codebase.Sqlite.NamedRef as NamedRef
import U.Codebase.Sqlite.ObjectType (ObjectType (DeclComponent, Namespace, Patch, TermComponent))
import qualified U.Codebase.Sqlite.ObjectType as ObjectType
import U.Codebase.Sqlite.Orphans ()
import qualified U.Codebase.Sqlite.Patch.Format as PatchFormat
import qualified U.Codebase.Sqlite.Reference as Reference
import qualified U.Codebase.Sqlite.Reference as S
import qualified U.Codebase.Sqlite.Reference as S.Reference
import qualified U.Codebase.Sqlite.Referent as Referent
import qualified U.Codebase.Sqlite.Referent as S.Referent
import U.Codebase.Sqlite.Serialization as Serialization
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Sqlite.TempEntity (TempEntity)
import qualified U.Codebase.Sqlite.TempEntity as TempEntity
import U.Codebase.Sqlite.TempEntityType (TempEntityType)
import qualified U.Codebase.Sqlite.TempEntityType as TempEntityType
import qualified U.Codebase.Sqlite.Term.Format as S.Term
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import qualified U.Codebase.Term as C
import qualified U.Codebase.Term as C.Term
import qualified U.Codebase.Type as C.Type
import U.Codebase.WatchKind (WatchKind)
import qualified U.Core.ABT as ABT
import qualified U.Util.Serialization as S
import qualified U.Util.Term as TermUtil
import Unison.Hash (Hash)
import qualified Unison.Hash as Hash
import Unison.Hash32 (Hash32)
import qualified Unison.Hash32 as Hash32
import Unison.Hash32.Orphans.Sqlite ()
import Unison.Prelude
import Unison.Sqlite
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Util.Alternative as Alternative
import qualified Unison.Util.Lens as Lens

-- * main squeeze

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = 9

createSchema :: Transaction ()
createSchema = do
  executeFile [hereFile|unison/sql/create.sql|]
  addTempEntityTables
  addNamespaceStatsTables
  addReflogTable
  addProjectTables
  execute insertSchemaVersionSql (Only currentSchemaVersion)
  where
    insertSchemaVersionSql =
      [here|
        INSERT INTO schema_version (version) VALUES (?)
      |]

addTempEntityTables :: Transaction ()
addTempEntityTables =
  executeFile [hereFile|unison/sql/001-temp-entity-tables.sql|]

addNamespaceStatsTables :: Transaction ()
addNamespaceStatsTables =
  executeFile [hereFile|unison/sql/003-namespace-statistics.sql|]

addReflogTable :: Transaction ()
addReflogTable =
  executeFile [hereFile|unison/sql/002-reflog-table.sql|]

addProjectTables :: Transaction ()
addProjectTables =
  executeFile [hereFile|unison/sql/004-project-tables.sql|]

executeFile :: String -> Transaction ()
executeFile =
  traverse_ (execute_ . Sqlite.Sql)
    . filter (not . Text.null)
    . map Text.strip
    . Text.split (== ';')
    . Text.pack

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

saveHash :: Hash32 -> Transaction HashId
saveHash hash = execute sql (Only hash) >> expectHashId hash
  where sql = [here|
    INSERT INTO hash (base32) VALUES (?)
    ON CONFLICT DO NOTHING
  |]

saveHashes :: Traversable f => f Hash32 -> Transaction (f HashId)
saveHashes hashes = do
  executeMany sql (coerce @[Hash32] @[Only Hash32] (Foldable.toList hashes))
  traverse expectHashId hashes
  where sql = [here|
    INSERT INTO hash (base32) VALUES (?)
    ON CONFLICT DO NOTHING
  |]

saveHashHash :: Hash -> Transaction HashId
saveHashHash = saveHash . Hash32.fromHash

loadHashId :: Hash32 -> Transaction (Maybe HashId)
loadHashId hash = queryMaybeCol loadHashIdSql (Only hash)

expectHashId :: Hash32 -> Transaction HashId
expectHashId hash = queryOneCol loadHashIdSql (Only hash)

loadHashIdSql :: Sql
loadHashIdSql =
  [here| SELECT id FROM hash WHERE base32 = ? COLLATE NOCASE |]

loadHashIdByHash :: Hash -> Transaction (Maybe HashId)
loadHashIdByHash = loadHashId . Hash32.fromHash

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
expectHashIdByHash = expectHashId . Hash32.fromHash

expectHash :: HashId -> Transaction Hash
expectHash h = Hash32.toHash <$> expectHash32 h

expectHash32 :: HashId -> Transaction Hash32
expectHash32 h = queryOneCol sql (Only h)
  where sql = [here| SELECT base32 FROM hash WHERE id = ? |]

expectBranchHash :: BranchHashId -> Transaction BranchHash
expectBranchHash = coerce expectHash

saveText :: Text -> Transaction TextId
saveText t = execute saveTextSql (Only t) >> expectTextId t

saveTexts :: Traversable f => f Text -> Transaction (f TextId)
saveTexts texts = do
  executeMany saveTextSql (coerce @[Text] @[Only Text] (Foldable.toList texts))
  traverse expectTextId texts

saveTextSql :: Sql
saveTextSql =
  [here|
    INSERT INTO text (text)
    VALUES (?)
    ON CONFLICT DO NOTHING
  |]

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

saveObject ::
  HashHandle ->
  HashId ->
  ObjectType ->
  ByteString ->
  Transaction ObjectId
saveObject hh h t blob = do
  oId <- execute sql (h, t, blob) >> expectObjectIdForPrimaryHashId h
  saveHashObject h oId 2 -- todo: remove this from here, and add it to other relevant places once there are v1 and v2 hashes
  rowsModified >>= \case
    0 -> pure ()
    _ -> do
      hash <- expectHash32 h
      tryMoveTempEntityDependents hh hash
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
  expectObjectIdForHash32 . Hash32.fromHash

expectObjectIdForHash32 :: Hash32 -> Transaction ObjectId
expectObjectIdForHash32 hash = do
  queryOneCol
    [here|
      SELECT object.id
      FROM object
      JOIN hash ON object.primary_hash_id = hash.id
      WHERE hash.base32 = ? COLLATE NOCASE
    |]
    (Only hash)

expectBranchObjectIdForHash32 :: Hash32 -> Transaction BranchObjectId
expectBranchObjectIdForHash32 =
  fmap BranchObjectId . expectObjectIdForHash32

expectPatchObjectIdForHash32 :: Hash32 -> Transaction PatchObjectId
expectPatchObjectIdForHash32 =
  fmap PatchObjectId . expectObjectIdForHash32

expectBranchHashIdForHash32 :: Hash32 -> Transaction BranchHashId
expectBranchHashIdForHash32 = queryOneCol sql . Only
  where
    sql =
      [here|
        SELECT hash.id FROM object
        INNER JOIN hash_object ON hash_object.object_id = object.id
        INNER JOIN hash ON hash_object.hash_id = hash.id
        WHERE object.type_id = 2
          AND hash.base32 = ? COLLATE NOCASE
      |]

expectCausalHashIdForHash32 :: Hash32 -> Transaction CausalHashId
expectCausalHashIdForHash32 = queryOneCol sql . Only
  where
    sql =
      [here|
        SELECT self_hash_id
        FROM causal INNER JOIN hash ON hash.id = self_hash_id
        WHERE base32 = ? COLLATE NOCASE
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
  fmap Hash32.toHash . expectPrimaryHash32ByObjectId

expectPrimaryHash32ByObjectId :: ObjectId -> Transaction Hash32
expectPrimaryHash32ByObjectId oId = queryOneCol sql (Only oId)
 where sql = [here|
  SELECT hash.base32
  FROM hash INNER JOIN object ON object.primary_hash_id = hash.id
  WHERE object.id = ?
|]

expectHashIdsForObject :: ObjectId -> Transaction (List.NonEmpty HashId)
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
saveCausal ::
  HashHandle ->
  CausalHashId ->
  BranchHashId ->
  [CausalHashId] ->
  Transaction ()
saveCausal hh self value parents = do
  execute insertCausalSql (self, value)
  rowsModified >>= \case
    0 -> pure ()
    _ -> do
      executeMany insertCausalParentsSql (fmap (self,) parents)
      flushCausalDependents hh self
  where
    insertCausalSql = [here|
      INSERT INTO causal (self_hash_id, value_hash_id)
      VALUES (?, ?)
      ON CONFLICT DO NOTHING
    |]
    insertCausalParentsSql = [here|
      INSERT INTO causal_parent (causal_id, parent_id) VALUES (?, ?)
    |]

flushCausalDependents ::
  HashHandle ->
  CausalHashId ->
  Transaction ()
flushCausalDependents hh chId = do
  hash <- expectHash32 (unCausalHashId chId)
  tryMoveTempEntityDependents hh hash

-- | `tryMoveTempEntityDependents #foo` does this:
--    0. Precondition: We just inserted object #foo.
--    1. Look up the dependents of #foo
--    2. Delete #foo as dependency from temp_entity_missing_dependency. e.g. (#bar, #foo), (#baz, #foo)
--    3. For each like #bar and #baz with no more rows in temp_entity_missing_dependency,
--        insert_entity them.
tryMoveTempEntityDependents ::
  HashHandle ->
  Hash32 ->
  Transaction ()
tryMoveTempEntityDependents hh dependency = do
  dependents <-
    queryListCol
      [here|
        DELETE FROM temp_entity_missing_dependency
        WHERE dependency = ?
        RETURNING dependent
      |]
      (Only dependency)
  traverse_ flushIfReadyToFlush dependents
  where
    flushIfReadyToFlush :: Hash32 -> Transaction ()
    flushIfReadyToFlush dependent = do
      readyToFlush dependent >>= \case
        True -> moveTempEntityToMain hh dependent
        False -> pure ()

    readyToFlush :: Hash32 -> Transaction Bool
    readyToFlush hash = queryOneCol [here|
      SELECT EXISTS (
        SELECT 1
        FROM temp_entity
        WHERE hash = ?
      ) AND NOT EXISTS (
        SELECT 1
        FROM temp_entity_missing_dependency
        WHERE dependent = ?
      )
    |] (hash, hash)

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
expectEntity :: Hash32 -> Transaction SyncEntity
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

-- | Read an entity out of temp storage.
expectTempEntity :: Hash32 -> Transaction TempEntity
expectTempEntity hash = do
  queryOneRowCheck sql (Only hash) \(blob, typeId) ->
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

{- ORMOLU_ENABLE -}

-- | look up all of the input entity's dependencies in the main table, to convert it to a sync entity
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
          <$> Lens.traverseOf
            (traverse . Lens._1)
            ( \LocalIds.LocalIds {textLookup, defnLookup} ->
                LocalIds.LocalIds
                  <$> saveTexts textLookup
                  <*> traverse expectObjectIdForHash32 defnLookup
            )
            decls

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
        <$> saveTexts texts
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
        <$> saveTexts texts
        <*> saveHashes hashes
        <*> traverse expectObjectIdForHash32 defns

    tempToSyncTermComponent :: TempEntity.TempTermFormat -> Transaction TermFormat.SyncTermFormat
    tempToSyncTermComponent = \case
      TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent terms) ->
        TermFormat.SyncTerm . TermFormat.SyncLocallyIndexedComponent
          <$> Lens.traverseOf
            (traverse . Lens._1)
            ( \LocalIds.LocalIds {textLookup, defnLookup} ->
                LocalIds.LocalIds
                  <$> saveTexts textLookup
                  <*> traverse expectObjectIdForHash32 defnLookup
            )
            terms

{- ORMOLU_DISABLE -}

-- | looking up all of the text and hashes is the first step of converting a SyncEntity to a Share.Entity
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

expectBranchObjectIdByBranchHashId :: BranchHashId -> Transaction BranchObjectId
expectBranchObjectIdByBranchHashId id = queryOneCol loadBranchObjectIdByBranchHashIdSql (Only id)

loadBranchObjectIdByBranchHashId :: BranchHashId -> Transaction (Maybe BranchObjectId)
loadBranchObjectIdByBranchHashId id = queryMaybeCol loadBranchObjectIdByBranchHashIdSql (Only id)

loadBranchObjectIdByBranchHashIdSql :: Sql
loadBranchObjectIdByBranchHashIdSql =
  [here|
    SELECT object_id FROM hash_object
    WHERE hash_id = ?
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
loadCausalParentsByHash :: Hash32 -> Transaction [Hash32]
loadCausalParentsByHash hash =
  queryListCol
    [here|
      SELECT h2.base32
      FROM causal_parent cp
      JOIN hash h1 ON cp.causal_id = h1.id
      JOIN hash h2 ON cp.parent_id = h2.id
      WHERE h1.base32 = ? COLLATE NOCASE
    |]
    (Only hash)

expectNamespaceRootBranchHashId :: Transaction BranchHashId
expectNamespaceRootBranchHashId = do
  chId <- expectNamespaceRoot
  expectCausalValueHashId chId

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

-- | Delete all watches that were put by 'putWatch'.
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

addToDependentsIndex :: [Reference.Reference] -> Reference.Id -> Transaction ()
addToDependentsIndex dependencies dependent = executeMany sql (map (:. dependent) dependencies)
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

-- | Which dependents should be returned?
--
-- * /IncludeAllDependents/. Include all dependents, including references from one's own component-mates, and references
-- from oneself (e.g. those in recursive functions)
-- * /ExcludeSelf/. Include all dependents, including references from one's own component-mates, but excluding
-- actual self references (e.g. those in recursive functions).
-- * /ExcludeOwnComponent/. Include all dependents outside of one's own component.
data DependentsSelector
  = IncludeAllDependents
  | ExcludeSelf
  | ExcludeOwnComponent

-- | Get dependents of a dependency.
getDependentsForDependency :: DependentsSelector -> Reference.Reference -> Transaction (Set Reference.Id)
getDependentsForDependency selector dependency = do
  dependents <- queryListRow sql dependency
  pure . Set.fromList $
    case selector of
      IncludeAllDependents -> dependents
      ExcludeSelf -> filter isNotSelfReference dependents
      ExcludeOwnComponent -> filter isNotReferenceFromOwnComponent dependents
  where
    sql =
      [here|
        SELECT dependent_object_id, dependent_component_index
        FROM dependents_index
        WHERE dependency_builtin IS ?
          AND dependency_object_id IS ?
          AND dependency_component_index IS ?
      |]

    isNotReferenceFromOwnComponent :: Reference.Id -> Bool
    isNotReferenceFromOwnComponent =
      case dependency of
        ReferenceBuiltin _ -> const True
        ReferenceDerived (C.Reference.Id oid0 _pos0) -> \(C.Reference.Id oid1 _pos1) -> oid0 /= oid1

    isNotSelfReference :: Reference.Id -> Bool
    isNotSelfReference =
      case dependency of
        ReferenceBuiltin _ -> const True
        ReferenceDerived ref -> (ref /=)

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
          AND dependent_component_index = ?
      |]

    isNotSelfReference :: Reference.Id -> Bool
    isNotSelfReference (C.Reference.Id oid1 _) =
      oid0 /= oid1

-- | Given two term (components) A and B, return the set of all terms that are along any "dependency path" from A to B,
-- not including A nor B; i.e., the transitive dependencies of A that are transitive dependents of B.
--
-- For example, if A depends on X and Y, X depends on Q, Y depends on Z, and X and Z depend on B...
--
--     --X-----Q
--    /     \
--   A       B
--    \     /
--     Y---Z
--
-- ...then `getDependenciesBetweenTerms A B` would return the set {X Y Z}
getDependenciesBetweenTerms :: ObjectId -> ObjectId -> Transaction (Set ObjectId)
getDependenciesBetweenTerms oid1 oid2 =
  queryListCol sql (oid1, oid2, oid2) <&> Set.fromList
  where
    -- Given the example above, we'd have tables that look like this.
    --
    -- First, the `paths` table finds all paths from source `A`, exploring depth-first. As a minor optimization, we seed
    -- the search not with `A`, but rather the direct dependencies of `A` (namely `X` and `Y`).
    --
    -- Naming note: "path_init" / "path_last" refer to the "init" / "last" elements of a list segments of a list (though
    -- our "last" is in reverse order):
    --
    --        [foo, bar, baz, qux]
    --   init  ^^^^^^^^^^^^^
    --   last                 ^^^
    --
    -- +-paths-------------------------+
    -- +-level-+-path_last-+-path_init-+
    -- |     0 |         X |        '' | -- path: [X]
    -- |     0 |         Y |        '' | -- path: [Y]
    -- |     1 |         B |      'X,' | -- path: [X,B]   -- ends in B, yay!
    -- |     1 |         Q |      'X,' | -- path: [X,Q]
    -- |     1 |         Z |      'Y,' | -- path: [Y,Z]
    -- |     2 |         B |    'Z,Y,' | -- path: [Y,Z,B] -- ends in B, yay!
    -- +-------+-----------+-----------+
    --
    -- Next, we seed another recursive CTE with those paths that end in the sink `B`. This is just the (very verbose)
    -- way to unnest an array in SQLite. All we're doing is turning the set of strings {'X,' 'Z,Y,'}, each of which
    -- represents the inner nodes of a full path between `A` and `B`, into the set {X Z Y}, which is just the full set
    -- of such inner nodes, along any path.
    --
    -- +-elems-----------------+
    -- +-path_elem-+-path_init-+
    -- |           |      'X,' |
    -- |           |    'Z,Y,' |
    -- |       'X' |        '' |
    -- |       'Z' |      'Y,' |
    -- |       'Y' |        '' |
    -- +-----------+-----------+
    --
    -- And finally, we just select out the non-null `path_elem` rows from here, casting the strings back to integers for
    -- clarity (this isn't very matter - SQLite would cast on-the-fly).
    --
    -- +-path_elem-+
    -- |         X |
    -- |         Z |
    -- |         Y |
    -- +-----------+
    --
    -- Notes
    --
    -- (1) We only care about term dependencies, not type dependencies. This is because a type can only depend on types,
    --     not terms, so there is no point in searching through a type's transitive dependencies looking for our sink.
    -- (2) No need to search beyond the sink itself, since component dependencies form a DAG.
    -- (3) An explicit cast from e.g. string '1' to int 1 isn't strictly necessary.
    sql :: Sql
    sql = [here|
      WITH RECURSIVE paths(level, path_last, path_init) AS (
        SELECT
          0,
          dependents_index.dependency_object_id,
          ''
        FROM dependents_index
          JOIN object ON dependents_index.dependency_object_id = object.id
        WHERE dependents_index.dependent_object_id = ?
          AND object.type_id = 0 -- Note (1)
          AND dependents_index.dependent_object_id != dependents_index.dependency_object_id
        UNION ALL
        SELECT
          paths.level + 1 AS level,
          dependents_index.dependency_object_id,
          dependents_index.dependent_object_id || ',' || paths.path_init
        FROM paths
          JOIN dependents_index
            ON paths.path_last = dependents_index.dependent_object_id
          JOIN object ON dependents_index.dependency_object_id = object.id
        WHERE object.type_id = 0 -- Note (1)
          AND dependents_index.dependent_object_id != dependents_index.dependency_object_id
          AND paths.path_last != ? -- Note (2)
        ORDER BY level DESC
      ),
      elems(path_elem, path_init) AS (
        SELECT null, path_init
        FROM paths
        WHERE paths.path_last = ?
        UNION ALL
        SELECT
          substr(path_init, 0, instr(path_init, ',')),
          substr(path_init, instr(path_init, ',') + 1)
        FROM elems
        WHERE path_init != ''
      )
      SELECT DISTINCT CAST(path_elem AS integer) AS path_elem -- Note (3)
      FROM elems
      WHERE path_elem IS NOT null
    |]

objectIdByBase32Prefix :: ObjectType -> Text -> Transaction [ObjectId]
objectIdByBase32Prefix objType prefix = queryListCol sql (objType, likeEscape '\\' prefix <> "%") where sql = [here|
  SELECT object.id FROM object
  INNER JOIN hash_object ON hash_object.object_id = object.id
  INNER JOIN hash ON hash_object.hash_id = hash.id
  WHERE object.type_id = ?
    AND hash.base32 LIKE ? ESCAPE '\'
|]

causalHashIdByBase32Prefix :: Text -> Transaction [CausalHashId]
causalHashIdByBase32Prefix prefix = queryListCol sql (Only $ prefix <> "%") where sql = [here|
  SELECT self_hash_id FROM causal
  INNER JOIN hash ON id = self_hash_id
  WHERE base32 LIKE ? ESCAPE '\'
|]

namespaceHashIdByBase32Prefix :: Text -> Transaction [BranchHashId]
namespaceHashIdByBase32Prefix prefix = queryListCol sql (Only $ prefix <> "%") where sql = [here|
  SELECT value_hash_id FROM causal
  INNER JOIN hash ON id = value_hash_id
  WHERE base32 LIKE ? ESCAPE '\'
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

-- | Not used in typical operations, but if we ever end up in a situation where a bug
-- has caused the name lookup index to go out of sync this can be used to get back to a clean
-- slate.
dropNameLookupTables :: Transaction ()
dropNameLookupTables = do
  execute_
    [here|
    DROP TABLE IF EXISTS term_name_lookup
  |]
  execute_
    [here|
    DROP TABLE IF EXISTS type_name_lookup
  |]

-- | Copies existing name lookup rows but replaces their branch hash id;
-- This is a low-level operation used as part of deriving a new name lookup index
-- from an existing one as performantly as possible.
copyScopedNameLookup :: BranchHashId -> BranchHashId -> Transaction ()
copyScopedNameLookup fromBHId toBHId = do
  execute termsCopySql (toBHId, fromBHId)
  execute typesCopySql (toBHId, fromBHId)
  where
    termsCopySql =
      [here|
        INSERT INTO scoped_term_name_lookup(root_branch_hash_id, reversed_name, last_name_segment, namespace, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type)
        SELECT ?, reversed_name, last_name_segment, namespace, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM scoped_term_name_lookup
        WHERE root_branch_hash_id = ?
      |]
    typesCopySql =
      [here|
        INSERT INTO scoped_type_name_lookup(root_branch_hash_id, reversed_name, last_name_segment, namespace, reference_builtin, reference_component_hash, reference_component_index)
        SELECT ?, reversed_name, last_name_segment, namespace, reference_builtin, reference_component_hash, reference_component_index
        FROM scoped_type_name_lookup
        WHERE root_branch_hash_id = ?
      |]

-- | Inserts a new record into the name_lookups table
trackNewBranchHashNameLookup :: BranchHashId -> Transaction ()
trackNewBranchHashNameLookup bhId = do
  execute sql (Only bhId)
  where
    sql =
      [here|
        INSERT INTO name_lookups (root_branch_hash_id)
        VALUES (?)
      |]

-- | Check if we've already got an index for the desired root branch hash.
checkBranchHashNameLookupExists :: BranchHashId -> Transaction Bool
checkBranchHashNameLookupExists hashId = do
  queryOneCol sql (Only hashId)
  where
    sql =
      [here|
        SELECT EXISTS (
          SELECT 1
          FROM name_lookups
          WHERE root_branch_hash_id = ?
          LIMIT 1
        )
       |]

-- | Insert the given set of term names into the name lookup table
insertScopedTermNames :: BranchHashId -> [NamedRef (Referent.TextReferent, Maybe NamedRef.ConstructorType)] -> Transaction ()
insertScopedTermNames bhId names = do
  executeMany sql (namedRefToRow <$> names)
  where
    namedRefToRow :: NamedRef (S.Referent.TextReferent, Maybe NamedRef.ConstructorType) -> (Only BranchHashId :. [SQLData])
    namedRefToRow namedRef =
      namedRef
        & fmap refToRow
        & NamedRef.namedRefToScopedRow
        & \nr -> (Only bhId :. nr)
    refToRow :: (Referent.TextReferent, Maybe NamedRef.ConstructorType) -> (Referent.TextReferent :. Only (Maybe NamedRef.ConstructorType))
    refToRow (ref, ct) = ref :. Only ct
    sql =
      [here|
      INSERT INTO scoped_term_name_lookup (root_branch_hash_id, reversed_name, namespace, last_name_segment, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT DO NOTHING
        |]

-- | Insert the given set of type names into the name lookup table
insertScopedTypeNames :: BranchHashId -> [NamedRef (Reference.TextReference)] -> Transaction ()
insertScopedTypeNames bhId names =
  executeMany sql ((Only bhId :.) . NamedRef.namedRefToScopedRow <$> names)
  where
    sql =
      [here|
      INSERT INTO scoped_type_name_lookup (root_branch_hash_id, reversed_name, namespace, last_name_segment, reference_builtin, reference_component_hash, reference_component_index)
        VALUES (?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT DO NOTHING
        |]

-- | Remove the given set of term names into the name lookup table
removeScopedTermNames :: BranchHashId -> [NamedRef Referent.TextReferent] -> Transaction ()
removeScopedTermNames bhId names = do
  executeMany sql ((Only bhId :.) <$> names)
  where
    sql =
      [here|
      DELETE FROM scoped_term_name_lookup
        WHERE
        root_branch_hash_id IS ?
        AND reversed_name IS ?
        AND referent_builtin IS ?
        AND referent_component_hash IS ?
        AND referent_component_index IS ?
        AND referent_constructor_index IS ?
        |]

-- | Remove the given set of term names into the name lookup table
removeScopedTypeNames :: BranchHashId -> [NamedRef (Reference.TextReference)] -> Transaction ()
removeScopedTypeNames bhId names = do
  executeMany sql ((Only bhId :.) <$> names)
  where
    sql =
      [here|
      DELETE FROM scoped_type_name_lookup
        WHERE
        root_branch_hash_id IS ?
        AND reversed_name IS ?
        AND reference_builtin IS ?
        AND reference_component_hash IS ?
        AND reference_component_index IS ?
        |]

-- | We need to escape any special characters for globbing.
--
-- >>> globEscape "Nat.*.doc"
-- "Nat.[*].doc"
globEscape :: Text -> Text
globEscape =
  -- We can't use Text.replace, since we'd end up replacing either "[" or "]" multiple
  -- times.
  Text.concatMap \case
    '*' -> "*"
    '?' -> "[?]"
    '[' -> "[[]"
    ']' -> "[]]"
    c -> Text.singleton c

-- | Escape special characters for "LIKE" matches.
--
-- Prepared statements prevent sql injection, but it's still possible some user
-- may be able to craft a query using a fake "hash" that would let them see more than they
-- ought to.
--
-- You still need to provide the escape char in the sql query, E.g.
--
-- @@
--   SELECT * FROM table
--     WHERE txt LIKE ? ESCAPE '\'
-- @@
--
-- >>> likeEscape '\\' "Nat.%"
-- "Nat.\%"
likeEscape :: Char -> Text -> Text
likeEscape '%' _ = error "Can't use % or _ as escape characters"
likeEscape '_' _ = error "Can't use % or _ as escape characters"
likeEscape escapeChar pat =
  flip Text.concatMap pat \case
    '%' -> Text.pack [escapeChar, '%']
    '_' -> Text.pack [escapeChar, '_']
    c
      | c == escapeChar -> Text.pack [escapeChar, escapeChar]
      | otherwise -> Text.singleton c

-- | Get the list of a term names in the root namespace according to the name lookup index
termNamesWithinNamespace :: BranchHashId -> Maybe Text -> Transaction [NamedRef (Referent.TextReferent, Maybe NamedRef.ConstructorType)]
termNamesWithinNamespace bhId mayNamespace = do
  let namespaceGlob = case mayNamespace of
        Nothing -> "*"
        Just namespace -> globEscape namespace <> ".*"
  results :: [NamedRef (Referent.TextReferent :. Only (Maybe NamedRef.ConstructorType))] <- queryListRow sql (bhId, namespaceGlob)
  pure (fmap unRow <$> results)
  where
    unRow (a :. Only b) = (a, b)
    sql =
      [here|
        SELECT reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, referent_constructor_type FROM scoped_term_name_lookup
        WHERE
          root_branch_hash_id = ?
          AND namespace GLOB ?
        |]

-- | Get the list of a type names in the root namespace according to the name lookup index
typeNamesWithinNamespace :: BranchHashId -> Maybe Text -> Transaction [NamedRef Reference.TextReference]
typeNamesWithinNamespace bhId mayNamespace = do
  let namespaceGlob = case mayNamespace of
        Nothing -> "*"
        Just namespace -> globEscape namespace <> ".*"
  results :: [NamedRef Reference.TextReference] <- queryListRow sql (bhId, namespaceGlob)
  pure results
  where
    sql =
      [here|
        SELECT reversed_name, reference_builtin, reference_component_hash, reference_component_index FROM scoped_type_name_lookup
        WHERE
          root_branch_hash_id = ?
          AND namespace GLOB ?
      |]

-- | @before x y@ returns whether or not @x@ occurred before @y@, i.e. @x@ is an ancestor of @y@.
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

-- | Where an entity is stored.
data EntityLocation
  = -- | `object` / `causal`
    EntityInMainStorage
  | -- | `temp_entity`
    EntityInTempStorage

-- | Where is an entity stored?
entityLocation :: Hash32 -> Transaction (Maybe EntityLocation)
entityLocation hash =
  entityExists hash >>= \case
    True -> pure (Just EntityInMainStorage)
    False -> do
      let sql = [here|SELECT EXISTS (SELECT 1 FROM temp_entity WHERE hash = ?)|]
      queryOneCol sql (Only hash) <&> \case
        True -> Just EntityInTempStorage
        False -> Nothing

-- | Does this entity already exist in the database, i.e. in the `object` or `causal` table?
entityExists :: Hash32 -> Transaction Bool
entityExists hash = do
  -- first get hashId if exists
  loadHashId hash >>= \case
    Nothing -> pure False
    -- then check if is causal hash or if object exists for hash id
    Just hashId -> isCausalHash hashId ||^ isObjectHash hashId

-- | Checks whether the codebase contains the actual branch value for a given causal hash.
checkBranchExistsForCausalHash :: CausalHash -> Transaction Bool
checkBranchExistsForCausalHash ch = do
  loadCausalHashIdByCausalHash ch >>= \case
    Nothing -> pure False
    Just chId -> queryOneCol sql (Only chId)
  where
    sql =
      [here|
      SELECT EXISTS
      ( SELECT 1 FROM causal c JOIN object o ON c.value_hash_id = o.primary_hash_id
        WHERE c.self_hash_id = ?
      )
      |]

-- | Insert a new `temp_entity` row, and its associated 1+ `temp_entity_missing_dependency` rows.
--
-- Preconditions:
--   1. The entity does not already exist in "main" storage (`object` / `causal`)
--   2. The entity does not already exist in `temp_entity`.
insertTempEntity :: Hash32 -> TempEntity -> NEMap Hash32 Text -> Transaction ()
insertTempEntity entityHash entity missingDependencies = do
  execute
    [here|
      INSERT INTO temp_entity (hash, blob, type_id)
      VALUES (?, ?, ?)
      ON CONFLICT DO NOTHING
    |]
    (entityHash, entityBlob, entityType)

  executeMany
    [here|
      INSERT INTO temp_entity_missing_dependency (dependent, dependency, dependencyJwt)
      VALUES (?, ?, ?)
    |]
    (map (\(depHash, depHashJwt) -> (entityHash, depHash, depHashJwt)) ((Foldable.toList . NEMap.toList) missingDependencies))
  where
    entityBlob :: ByteString
    entityBlob =
      runPutS (Serialization.putTempEntity entity)

    entityType :: TempEntityType
    entityType =
      Entity.entityType entity

-- | Delete a row from the `temp_entity` table, if it exists.
deleteTempEntity :: Hash32 -> Transaction ()
deleteTempEntity hash =
  execute
    [here|
      DELETE
      FROM temp_entity
      WHERE hash = ?
    |]
    (Only hash)

-- | "Elaborate" a set of `temp_entity` hashes.
--
-- Given a set of `temp_entity` hashes, returns the (known) set of transitive dependencies that haven't already been
-- downloaded (i.e. aren't in the `temp_entity` table)
--
-- For example, if we have temp entities A and B, where A depends on B and B depends on C...
--
--   | temp_entity |   | temp_entity_missing_dependency |
--   |=============|   |================================|
--   | hash        |   | dependent    | dependency      |
--   |-------------|   |--------------|-----------------|
--   | A           |   | A            | B               |
--   | B           |   | B            | C               |
--
-- ... then `elaborateHashes {A}` would return the singleton set {C} (because we take the set of transitive
-- dependencies {A,B,C} and subtract the set we already have, {A,B}).
elaborateHashes :: Nel.NonEmpty Hash32 -> Transaction [Text]
elaborateHashes hashes =
  queryListCol query hashesValues
  where
    query :: Sql
    query =
      fold
        [ [sql|
            WITH RECURSIVE
            new_temp_entity_dependents (hash) AS (
          |],
          valuesSql hashesValues,
          [sql|
            ),
            elaborated_dependency (hash, hashJwt) AS (
              SELECT temd.dependency, temd.dependencyJwt
              FROM new_temp_entity_dependents AS new
                JOIN temp_entity_missing_dependency AS temd
                  ON temd.dependent = new.hash

              UNION
              SELECT temd.dependency, temd.dependencyJwt
              FROM temp_entity_missing_dependency AS temd
                JOIN elaborated_dependency AS ed
                  ON temd.dependent = ed.hash
            )
            SELECT hashJwt FROM elaborated_dependency
            WHERE NOT EXISTS (
              SELECT 1 FROM temp_entity
              WHERE temp_entity.hash = elaborated_dependency.hash
            )
          |]
        ]

    hashesValues :: Values (Only Hash32)
    hashesValues =
      Values (coerce @(List.NonEmpty Hash32) @(List.NonEmpty (Only Hash32)) hashes)

moveTempEntityToMain ::
  HashHandle ->
  Hash32 ->
  Transaction ()
moveTempEntityToMain hh hash = do
  entity <- expectTempEntity hash
  deleteTempEntity hash
  _ <- saveTempEntityInMain hh hash entity
  pure ()

-- | Save a temp entity in main storage.
--
-- Precondition: all of its dependencies are already in main storage.
saveTempEntityInMain :: HashHandle -> Hash32 -> TempEntity -> Transaction (Either CausalHashId ObjectId)
saveTempEntityInMain hh hash entity = do
  entity' <- tempToSyncEntity entity
  saveSyncEntity hh hash entity'

saveSyncEntity ::
  HashHandle ->
  Hash32 ->
  SyncEntity ->
  Transaction (Either CausalHashId ObjectId)
saveSyncEntity hh hash entity = do
  case entity of
    Entity.TC stf -> do
      lic :: TermFormat.LocallyIndexedComponent <- do
        let TermFormat.SyncTerm x = stf
        unsafeIO (unsyncTermComponent x)

      tc :: [(C.Term Symbol, C.Term.Type Symbol)] <-
        traverse
          (\(a, b, c) -> s2cTermWithType a b c)
          (toList $ TermFormat.unLocallyIndexedComponent lic)
      let bytes = runPutS (Serialization.recomposeTermFormat stf)
      objId <- saveTermComponent hh (Just bytes) (Hash32.toHash hash) tc
      pure (Right objId)
    Entity.DC sdf -> do
      lic :: S.Decl.LocallyIndexedComponent <- do
        let S.Decl.SyncDecl xs = sdf
        unsafeIO (unsyncDeclComponent xs)

      dc :: [C.Decl.Decl Symbol] <-
        traverse
          (\(localIds, decl) -> s2cDecl localIds decl)
          (toList $ S.Decl.unLocallyIndexedComponent lic)

      let bytes = runPutS (Serialization.recomposeDeclFormat sdf)
      objId <- saveDeclComponent hh (Just bytes) (Hash32.toHash hash) dc

      pure (Right objId)
    Entity.N sbf -> do
      hashId <- saveHash hash
      let bytes = runPutS (Serialization.recomposeBranchFormat sbf)
      Right <$> saveObject hh hashId ObjectType.Namespace bytes
    Entity.P spf -> do
      hashId <- saveHash hash
      let bytes = runPutS (Serialization.recomposePatchFormat spf)
      Right <$> saveObject hh hashId ObjectType.Patch bytes
    Entity.C scf -> case scf of
      Sqlite.Causal.SyncCausalFormat {valueHash, parents} -> do
        hashId <- saveHash hash
        let causalHashId = CausalHashId hashId
        saveCausal hh causalHashId valueHash (toList parents)
        pure $ Left causalHashId

s2cTermWithType :: LocalIds.LocalIds -> S.Term.Term -> S.Term.Type -> Transaction (C.Term Symbol, C.Term.Type Symbol)
s2cTermWithType ids tm tp = do
  (substText, substHash) <- localIdsToLookups expectText expectPrimaryHashByObjectId ids
  pure (x2cTerm substText substHash tm, x2cTType substText substHash tp)

saveTermComponent ::
  HashHandle ->
  -- | The serialized term component if we already have it e.g. via sync
  Maybe ByteString ->
  -- | term component hash
  Hash ->
  -- | term component
  [(C.Term Symbol, C.Term.Type Symbol)] ->
  Transaction ObjectId
saveTermComponent hh@HashHandle {toReference, toReferenceMentions} maybeEncodedTerms h terms = do
  when debug . traceM $ "Operations.saveTermComponent " ++ show h
  sTermElements <- traverse (uncurry c2sTerm) terms
  hashId <- saveHashHash h
  let bytes = fromMaybe mkByteString maybeEncodedTerms
      mkByteString =
        let li = S.Term.LocallyIndexedComponent $ Vector.fromList sTermElements
         in S.putBytes Serialization.putTermFormat $ S.Term.Term li
  oId <- saveObject hh hashId ObjectType.TermComponent bytes
  -- populate dependents index
  let unlocalizeRefs :: ((LocalIds, S.Term.Term, S.Term.Type), C.Reference.Pos) -> (Set S.Reference.Reference, S.Reference.Id)
      unlocalizeRefs ((LocalIds tIds oIds, tm, tp), i) =
        let self = C.Reference.Id oId i
            dependencies :: Set S.Reference =
              let (tmRefs, tpRefs, tmLinks, tpLinks) = TermUtil.dependencies tm
                  tpRefs' = Foldable.toList $ C.Type.dependencies tp
                  getTermSRef :: S.Term.TermRef -> S.Reference
                  getTermSRef = \case
                    C.ReferenceBuiltin t -> C.ReferenceBuiltin (tIds Vector.! fromIntegral t)
                    C.Reference.Derived Nothing i -> C.Reference.Derived oId i -- index self-references
                    C.Reference.Derived (Just h) i -> C.Reference.Derived (oIds Vector.! fromIntegral h) i
                  getTypeSRef :: S.Term.TypeRef -> S.Reference
                  getTypeSRef = \case
                    C.ReferenceBuiltin t -> C.ReferenceBuiltin (tIds Vector.! fromIntegral t)
                    C.Reference.Derived h i -> C.Reference.Derived (oIds Vector.! fromIntegral h) i
                  getSTypeLink = getTypeSRef
                  getSTermLink :: S.Term.TermLink -> S.Reference
                  getSTermLink = \case
                    C.Referent.Con ref _conId -> getTypeSRef ref
                    C.Referent.Ref ref -> getTermSRef ref
               in Set.fromList $
                    map getTermSRef tmRefs
                      ++ map getSTermLink tmLinks
                      ++ map getTypeSRef (tpRefs ++ tpRefs')
                      ++ map getSTypeLink tpLinks
         in (dependencies, self)
  for_ (map unlocalizeRefs (sTermElements `zip` [0 ..])) \(dependencies, dependent) ->
    addToDependentsIndex (Set.toList dependencies) dependent
  for_ ((snd <$> terms) `zip` [0 ..]) \(tp, i) -> do
    let self = C.Referent.RefId (C.Reference.Id oId i)
        typeForIndexing = toReference tp
        typeMentionsForIndexing = toReferenceMentions tp
    addTypeToIndexForTerm self typeForIndexing
    addTypeMentionsToIndexForTerm self typeMentionsForIndexing

  pure oId

-- | Unlocalize a decl.
s2cDecl :: LocalIds -> S.Decl.Decl Symbol -> Transaction (C.Decl Symbol)
s2cDecl ids (C.Decl.DataDeclaration dt m b ct) = do
  substTypeRef <- localIdsToTypeRefLookup ids
  pure (C.Decl.DataDeclaration dt m b (C.Type.rmap substTypeRef <$> ct))

saveDeclComponent ::
  HashHandle ->
  Maybe ByteString ->
  Hash ->
  [C.Decl Symbol] ->
  Transaction ObjectId
saveDeclComponent hh@HashHandle {toReferenceDecl, toReferenceDeclMentions} maybeEncodedDecls h decls = do
  when debug . traceM $ "Operations.saveDeclComponent " ++ show h
  sDeclElements <- traverse (c2sDecl saveText expectObjectIdForPrimaryHash) decls
  hashId <- saveHashHash h
  let bytes = fromMaybe mkByteString maybeEncodedDecls
      mkByteString =
        let li = S.Decl.LocallyIndexedComponent $ Vector.fromList sDeclElements
         in S.putBytes Serialization.putDeclFormat $ S.Decl.Decl li
  oId <- saveObject hh hashId ObjectType.DeclComponent bytes
  -- populate dependents index
  let unlocalizeRefs :: ((LocalIds, S.Decl.Decl Symbol), C.Reference.Pos) -> (Set S.Reference.Reference, S.Reference.Id)
      unlocalizeRefs ((LocalIds tIds oIds, decl), i) =
        let self = C.Reference.Id oId i
            dependencies :: Set S.Decl.TypeRef = C.Decl.dependencies decl
            getSRef :: C.Reference.Reference' LocalTextId (Maybe LocalDefnId) -> S.Reference.Reference
            getSRef = \case
              C.ReferenceBuiltin t -> C.ReferenceBuiltin (tIds Vector.! fromIntegral t)
              C.Reference.Derived Nothing i -> C.Reference.Derived oId i -- index self-references
              C.Reference.Derived (Just h) i -> C.Reference.Derived (oIds Vector.! fromIntegral h) i
         in (Set.map getSRef dependencies, self)
  for_ (map unlocalizeRefs (sDeclElements `zip` [0 ..])) \(dependencies, dependent) ->
    addToDependentsIndex (Set.toList dependencies) dependent
  for_ ((fmap C.Decl.constructorTypes decls) `zip` [0 ..]) \(ctors, i) ->
    for_ (ctors `zip` [0 ..]) \(tp, j) -> do
      let self = C.Referent.ConId (C.Reference.Id oId i) j
          typeForIndexing = toReferenceDecl h tp
          typeMentionsForIndexing = toReferenceDeclMentions h tp
      addTypeToIndexForTerm self typeForIndexing
      addTypeMentionsToIndexForTerm self typeMentionsForIndexing

  pure oId

-- | implementation detail of {s,w}2c*Term* & s2cDecl
localIdsToLookups :: (Monad m) => (t -> m Text) -> (d -> m Hash) -> LocalIds' t d -> m (LocalTextId -> Text, LocalDefnId -> Hash)
localIdsToLookups loadText loadHash localIds = do
  texts <- traverse loadText $ LocalIds.textLookup localIds
  hashes <- traverse loadHash $ LocalIds.defnLookup localIds
  let substText (LocalTextId w) = texts Vector.! fromIntegral w
      substHash (LocalDefnId w) = hashes Vector.! fromIntegral w
  pure (substText, substHash)

-- | implementation detail of {s,w}2c*Term*
x2cTerm :: (LocalTextId -> Text) -> (LocalDefnId -> Hash) -> S.Term.Term -> C.Term Symbol
x2cTerm substText substHash =
  -- substitute the text and hashes back into the term
  C.Term.extraMap substText substTermRef substTypeRef substTermLink substTypeLink id
  where
    substTermRef = bimap substText (fmap substHash)
    substTypeRef = bimap substText substHash
    substTermLink = bimap substTermRef substTypeRef
    substTypeLink = substTypeRef

-- | implementation detail of {s,w}2c*Term*
x2cTType :: (LocalTextId -> Text) -> (LocalDefnId -> Hash) -> S.Term.Type -> C.Term.Type Symbol
x2cTType substText substHash = C.Type.rmap (bimap substText substHash)

c2sTerm :: C.Term Symbol -> C.Term.Type Symbol -> Transaction (LocalIds, S.Term.Term, S.Term.Type)
c2sTerm tm tp =
  c2xTerm saveText expectObjectIdForPrimaryHash tm (Just tp)
    <&> \(w, tm, mayTp) -> (w, tm, Maybe.fromJust mayTp)

addTypeToIndexForTerm :: S.Referent.Id -> C.Reference -> Transaction ()
addTypeToIndexForTerm sTermId cTypeRef = do
  sTypeRef <- saveReferenceH cTypeRef
  addToTypeIndex sTypeRef sTermId

addTypeMentionsToIndexForTerm :: S.Referent.Id -> Set C.Reference -> Transaction ()
addTypeMentionsToIndexForTerm sTermId cTypeMentionRefs = do
  traverse_ (flip addToTypeMentionsIndex sTermId <=< saveReferenceH) cTypeMentionRefs

localIdsToTypeRefLookup :: LocalIds -> Transaction (S.Decl.TypeRef -> C.Decl.TypeRef)
localIdsToTypeRefLookup localIds = do
  (substText, substHash) <- localIdsToLookups expectText expectPrimaryHashByObjectId localIds
  pure $ bimap substText (fmap substHash)

c2sDecl ::
  forall m t d.
  (Monad m) =>
  (Text -> m t) ->
  (Hash -> m d) ->
  C.Decl Symbol ->
  m (LocalIds' t d, S.Decl.Decl Symbol)
c2sDecl saveText saveDefn (C.Decl.DataDeclaration dt m b cts) = do
  done =<< (runWriterT . flip evalStateT mempty) do
    cts' <- traverse (ABT.transformM goType) cts
    pure (C.Decl.DataDeclaration dt m b cts')
  where
    goType ::
      forall m a.
      (MonadWriter (Seq Text, Seq Hash) m, MonadState (Map Text LocalTextId, Map Hash LocalDefnId) m) =>
      C.Type.FD a ->
      m (S.Decl.F a)
    goType = \case
      C.Type.Ref r -> C.Type.Ref <$> bitraverse lookupText (traverse lookupDefn) r
      C.Type.Arrow i o -> pure $ C.Type.Arrow i o
      C.Type.Ann a k -> pure $ C.Type.Ann a k
      C.Type.App f a -> pure $ C.Type.App f a
      C.Type.Effect e a -> pure $ C.Type.Effect e a
      C.Type.Effects es -> pure $ C.Type.Effects es
      C.Type.Forall a -> pure $ C.Type.Forall a
      C.Type.IntroOuter a -> pure $ C.Type.IntroOuter a
    done :: (S.Decl.Decl Symbol, (Seq Text, Seq Hash)) -> m (LocalIds' t d, S.Decl.Decl Symbol)
    done (decl, (localTextValues, localDefnValues)) = do
      textIds <- traverse saveText localTextValues
      defnIds <- traverse saveDefn localDefnValues
      let ids =
            LocalIds
              (Vector.fromList (Foldable.toList textIds))
              (Vector.fromList (Foldable.toList defnIds))
      pure (ids, decl)

-- | implementation detail of c2{s,w}Term
--  The Type is optional, because we don't store them for watch expression results.
c2xTerm :: forall m t d. (Monad m) => (Text -> m t) -> (Hash -> m d) -> C.Term Symbol -> Maybe (C.Term.Type Symbol) -> m (LocalIds' t d, S.Term.Term, Maybe (S.Term.Type))
c2xTerm saveText saveDefn tm tp =
  done =<< (runWriterT . flip evalStateT mempty) do
    sterm <- ABT.transformM go tm
    stype <- traverse (ABT.transformM goType) tp
    pure (sterm, stype)
  where
    go :: forall m a. (MonadWriter (Seq Text, Seq Hash) m, MonadState (Map Text LocalTextId, Map Hash LocalDefnId) m) => C.Term.F Symbol a -> m (S.Term.F a)
    go = \case
      C.Term.Int n -> pure $ C.Term.Int n
      C.Term.Nat n -> pure $ C.Term.Nat n
      C.Term.Float n -> pure $ C.Term.Float n
      C.Term.Boolean b -> pure $ C.Term.Boolean b
      C.Term.Text t -> C.Term.Text <$> lookupText t
      C.Term.Char ch -> pure $ C.Term.Char ch
      C.Term.Ref r ->
        C.Term.Ref <$> bitraverse lookupText (traverse lookupDefn) r
      C.Term.Constructor typeRef cid ->
        C.Term.Constructor
          <$> bitraverse lookupText lookupDefn typeRef
          <*> pure cid
      C.Term.Request typeRef cid ->
        C.Term.Request <$> bitraverse lookupText lookupDefn typeRef <*> pure cid
      C.Term.Handle a a2 -> pure $ C.Term.Handle a a2
      C.Term.App a a2 -> pure $ C.Term.App a a2
      C.Term.Ann a typ -> C.Term.Ann a <$> ABT.transformM goType typ
      C.Term.List as -> pure $ C.Term.List as
      C.Term.If c t f -> pure $ C.Term.If c t f
      C.Term.And a a2 -> pure $ C.Term.And a a2
      C.Term.Or a a2 -> pure $ C.Term.Or a a2
      C.Term.Lam a -> pure $ C.Term.Lam a
      C.Term.LetRec bs a -> pure $ C.Term.LetRec bs a
      C.Term.Let a a2 -> pure $ C.Term.Let a a2
      C.Term.Match a cs -> C.Term.Match a <$> traverse goCase cs
      C.Term.TermLink r ->
        C.Term.TermLink
          <$> bitraverse
            (bitraverse lookupText (traverse lookupDefn))
            (bitraverse lookupText lookupDefn)
            r
      C.Term.TypeLink r ->
        C.Term.TypeLink <$> bitraverse lookupText lookupDefn r
    goType ::
      forall m a.
      (MonadWriter (Seq Text, Seq Hash) m, MonadState (Map Text LocalTextId, Map Hash LocalDefnId) m) =>
      C.Type.FT a ->
      m (S.Term.FT a)
    goType = \case
      C.Type.Ref r -> C.Type.Ref <$> bitraverse lookupText lookupDefn r
      C.Type.Arrow i o -> pure $ C.Type.Arrow i o
      C.Type.Ann a k -> pure $ C.Type.Ann a k
      C.Type.App f a -> pure $ C.Type.App f a
      C.Type.Effect e a -> pure $ C.Type.Effect e a
      C.Type.Effects es -> pure $ C.Type.Effects es
      C.Type.Forall a -> pure $ C.Type.Forall a
      C.Type.IntroOuter a -> pure $ C.Type.IntroOuter a
    goCase ::
      forall m w s a.
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field1' s (Map Text LocalTextId),
        Lens.Field1' w (Seq Text),
        Lens.Field2' s (Map Hash LocalDefnId),
        Lens.Field2' w (Seq Hash)
      ) =>
      C.Term.MatchCase Text C.Term.TypeRef a ->
      m (C.Term.MatchCase LocalTextId S.Term.TypeRef a)
    goCase = \case
      C.Term.MatchCase pat guard body ->
        C.Term.MatchCase <$> goPat pat <*> pure guard <*> pure body
    goPat ::
      forall m s w.
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field1' s (Map Text LocalTextId),
        Lens.Field1' w (Seq Text),
        Lens.Field2' s (Map Hash LocalDefnId),
        Lens.Field2' w (Seq Hash)
      ) =>
      C.Term.Pattern Text C.Term.TypeRef ->
      m (C.Term.Pattern LocalTextId S.Term.TypeRef)
    goPat = \case
      C.Term.PUnbound -> pure $ C.Term.PUnbound
      C.Term.PVar -> pure $ C.Term.PVar
      C.Term.PBoolean b -> pure $ C.Term.PBoolean b
      C.Term.PInt i -> pure $ C.Term.PInt i
      C.Term.PNat n -> pure $ C.Term.PNat n
      C.Term.PFloat d -> pure $ C.Term.PFloat d
      C.Term.PText t -> C.Term.PText <$> lookupText t
      C.Term.PChar c -> pure $ C.Term.PChar c
      C.Term.PConstructor r i ps -> C.Term.PConstructor <$> bitraverse lookupText lookupDefn r <*> pure i <*> traverse goPat ps
      C.Term.PAs p -> C.Term.PAs <$> goPat p
      C.Term.PEffectPure p -> C.Term.PEffectPure <$> goPat p
      C.Term.PEffectBind r i bindings k -> C.Term.PEffectBind <$> bitraverse lookupText lookupDefn r <*> pure i <*> traverse goPat bindings <*> goPat k
      C.Term.PSequenceLiteral ps -> C.Term.PSequenceLiteral <$> traverse goPat ps
      C.Term.PSequenceOp l op r -> C.Term.PSequenceOp <$> goPat l <*> pure op <*> goPat r

    done :: ((S.Term.Term, Maybe S.Term.Type), (Seq Text, Seq Hash)) -> m (LocalIds' t d, S.Term.Term, Maybe S.Term.Type)
    done ((tm, tp), (localTextValues, localDefnValues)) = do
      textIds <- traverse saveText localTextValues
      defnIds <- traverse saveDefn localDefnValues
      let ids =
            LocalIds
              (Vector.fromList (Foldable.toList textIds))
              (Vector.fromList (Foldable.toList defnIds))
      pure (ids, void tm, void <$> tp)

-- | Save the text and hash parts of a Reference to the database and substitute their ids.
saveReferenceH :: C.Reference -> Transaction S.ReferenceH
saveReferenceH = bitraverse saveText saveHashHash

lookupText ::
  forall m s w t.
  ( MonadState s m,
    MonadWriter w m,
    Lens.Field1' s (Map t LocalTextId),
    Lens.Field1' w (Seq t),
    Ord t
  ) =>
  t ->
  m LocalTextId
lookupText = lookup_ Lens._1 Lens._1 LocalTextId

lookupDefn ::
  forall m s w d.
  ( MonadState s m,
    MonadWriter w m,
    Lens.Field2' s (Map d LocalDefnId),
    Lens.Field2' w (Seq d),
    Ord d
  ) =>
  d ->
  m LocalDefnId
lookupDefn = lookup_ Lens._2 Lens._2 LocalDefnId

-- | shared implementation of lookupTextHelper and lookupDefnHelper
--  Look up a value in the LUT, or append it.
lookup_ ::
  (MonadState s m, MonadWriter w m, Ord t) =>
  Lens' s (Map t t') ->
  Lens' w (Seq t) ->
  (Word64 -> t') ->
  t ->
  m t'
lookup_ stateLens writerLens mk t = do
  map <- Lens.use stateLens
  case Map.lookup t map of
    Nothing -> do
      let id = mk . fromIntegral $ Map.size map
      stateLens Lens.%= Map.insert t id
      Writer.tell $ Lens.set writerLens (Seq.singleton t) mempty
      pure id
    Just t' -> pure t'

-- | Save statistics about a given branch.
saveNamespaceStats :: BranchHashId -> NamespaceStats -> Transaction ()
saveNamespaceStats bhId stats = do
  execute sql (Only bhId :. stats)
  where
    sql =
      [here|
        INSERT INTO namespace_statistics (namespace_hash_id, num_contained_terms, num_contained_types, num_contained_patches)
          VALUES (?, ?, ?, ?)
      |]

-- | Looks up statistics for a given branch, there's no guarantee that we have
-- computed and saved stats for any given branch.
loadNamespaceStatsByHashId :: BranchHashId -> Transaction (Maybe NamespaceStats)
loadNamespaceStatsByHashId bhId = do
  queryMaybeRow sql (Only bhId)
  where
    sql =
      [here|
        SELECT num_contained_terms, num_contained_types, num_contained_patches
        FROM namespace_statistics
        WHERE namespace_hash_id = ?
      |]

appendReflog :: Reflog.Entry CausalHashId Text -> Transaction ()
appendReflog entry = execute sql entry
  where
    sql =
      [here|
        INSERT INTO reflog (time, from_root_causal_id, to_root_causal_id, reason) VALUES (?, ?, ?, ?)
      |]

getReflog :: Int -> Transaction [Reflog.Entry CausalHashId Text]
getReflog numEntries = queryListRow sql (Only numEntries)
  where
    sql =
      [here|
        SELECT time, from_root_causal_id, to_root_causal_id, reason
          FROM reflog
          ORDER BY time DESC
          LIMIT ?
      |]

data Project = Project
  { projectId :: ProjectId,
    name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToRow, FromRow)

data RemoteProject = RemoteProject
  { projectId :: RemoteProjectId,
    host :: Text,
    name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToRow, FromRow)

data Branch = Branch
  { projectId :: ProjectId,
    branchId :: ProjectBranchId,
    name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToRow, FromRow)

data RemoteProjectBranch = RemoteProjectBranch
  { projectId :: RemoteProjectId,
    branchId :: RemoteProjectBranchId,
    host :: Text,
    name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToRow, FromRow)

-- | Does a project exist with this id?
projectExists :: ProjectId -> Transaction Bool
projectExists projectId =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM project
        WHERE id = ?
      )
    |]
    (Only projectId)

-- | Does a project exist by this name?
projectExistsByName :: Text -> Transaction Bool
projectExistsByName name =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM project
        WHERE name = ?
      )
    |]
    (Only name)

loadProject :: ProjectId -> Transaction (Maybe Project)
loadProject pid = queryMaybeRow loadProjectSql (Only pid)

expectProject :: ProjectId -> Transaction Project
expectProject pid = queryOneRow loadProjectSql (Only pid)

loadProjectSql :: Sql
loadProjectSql =
  [sql|
    SELECT
      id,
      name
    FROM
      project
    WHERE
      id = ?
  |]

loadProjectByName :: Text -> Transaction (Maybe Project)
loadProjectByName name =
  queryMaybeRow
    [sql|
      SELECT
        id,
        name
      FROM
        project
      WHERE
        name = ?
    |]
    (Only name)

insertProject :: ProjectId -> Text -> Transaction ()
insertProject uuid name = execute bonk (uuid, name)
  where
    bonk =
      [sql|
        INSERT INTO project (id, name)
          VALUES (?, ?)
           |]

-- | Does a project branch exist by this name?
projectBranchExistsByName :: ProjectId -> Text -> Transaction Bool
projectBranchExistsByName projectId name =
  queryOneCol
    [sql|
      SELECT
        EXISTS (
          SELECT
            1
          FROM
            project_branch
          WHERE
            project_id = ?
            AND name = ?)
    |]
    (projectId, name)

loadProjectBranch :: ProjectId -> ProjectBranchId -> Transaction (Maybe Branch)
loadProjectBranch pid bid =
  queryMaybeRow loadProjectBranchSql (pid, bid)

expectProjectBranch :: ProjectId -> ProjectBranchId -> Transaction Branch
expectProjectBranch projectId branchId =
  queryOneRow loadProjectBranchSql (projectId, branchId)

loadProjectBranchSql :: Sql
loadProjectBranchSql =
  [sql|
    SELECT
      project_id,
      branch_id,
      name
    FROM
      project_branch
    WHERE
      project_id = ?
      AND branch_id = ?
  |]

loadProjectBranchByName :: ProjectId -> Text -> Transaction (Maybe Branch)
loadProjectBranchByName projectId name =
  queryMaybeRow
    [sql|
      SELECT
        project_id,
        branch_id,
        name
      FROM
        project_branch
      WHERE
        project_id = ?
        AND name = ?
    |]
    (projectId, name)

loadProjectAndBranchNames :: ProjectId -> ProjectBranchId -> Transaction (Maybe (Text, Text))
loadProjectAndBranchNames projectId branchId =
  queryMaybeRow
    [sql|
      SELECT
        project.name,
        project_branch.name
      FROM
        project
        JOIN project_branch ON project.id = project_branch.project_id
      WHERE
        project_branch.project_id = ?
        AND project_branch.branch_id = ?
    |]
    (projectId, branchId)

insertProjectBranch :: ProjectId -> ProjectBranchId -> Text -> Transaction ()
insertProjectBranch pid bid bname = execute bonk (pid, bid, bname)
  where
    bonk =
      [sql|
        INSERT INTO project_branch (project_id, branch_id, name)
          VALUES (?, ?, ?)
      |]

markProjectBranchChild :: ProjectId -> ProjectBranchId -> ProjectBranchId -> Transaction ()
markProjectBranchChild pid parent child = execute bonk (pid, parent, child)
  where
    bonk =
      [sql|
        INSERT INTO project_branch_parent (project_id, parent_branch_id, branch_id)
          VALUES (?, ?, ?)
          |]

-- TODO: Get this from some canonical place
unisonShareUri :: Text
unisonShareUri = "https://api.unison-lang.org"

data LoadRemoteBranchFlag
  = IncludeSelfRemote
  | ExcludeSelfRemote
  deriving stock (Show, Eq)

-- | Determine the remote mapping for a local project/branch by
-- looking at the mapping for the given pair, then falling back to the
-- project of the nearest ancestor.
loadRemoteProjectBranchByLocalProjectBranch ::
  ProjectId ->
  ProjectBranchId ->
  Transaction (Maybe (RemoteProjectId, Maybe RemoteProjectBranchId))
loadRemoteProjectBranchByLocalProjectBranch p b = do
  loadRemoteProjectBranchByLocalProjectBranchGen IncludeSelfRemote p b <&> fmap fixup
  where
    -- If the depth is 0 then the local project/branch we provided has
    -- a remote mapping. Otherwise we found some ancestor's remote
    -- mapping and we only wish to retain the project portion.
    fixup = \case
      (project, branch, depth) -> case depth of
        0 -> (project, Just branch)
        _ -> (project, Nothing)

-- | Load the default merge target for a local branch (i.e. The nearest
-- ancestor's remote mapping)
loadDefaultMergeTargetForLocalProjectBranch ::
  ProjectId ->
  ProjectBranchId ->
  Transaction (Maybe (RemoteProjectId, RemoteProjectBranchId))
loadDefaultMergeTargetForLocalProjectBranch p b = do
  loadRemoteProjectBranchByLocalProjectBranchGen ExcludeSelfRemote p b <&> fmap fixup
  where
    fixup = \case
      (project, branch, _) -> (project, branch)

-- Parameterized query for finding the remote mapping for a branch and
-- the default merge target for a branch.
loadRemoteProjectBranchByLocalProjectBranchGen ::
  LoadRemoteBranchFlag ->
  ProjectId ->
  ProjectBranchId ->
  Transaction (Maybe (RemoteProjectId, RemoteProjectBranchId, Int64))
loadRemoteProjectBranchByLocalProjectBranchGen loadRemoteBranchFlag pid bid =
  queryMaybeRow theSql (unisonShareUri, pid, bid, unisonShareUri)
  where
    theSql =
      [sql|
        WITH RECURSIVE t AS (
          SELECT
            pb.project_id,
            pb.branch_id,
            pbp.parent_branch_id,
            pbrm.remote_project_id,
            pbrm.remote_branch_id,
            0 AS depth
          FROM
            project_branch AS pb
            LEFT JOIN project_branch_parent AS pbp USING (project_id, branch_id)
            LEFT JOIN project_branch_remote_mapping AS pbrm ON pbrm.local_project_id = pb.project_id
              AND pbrm.local_branch_id = pb.branch_id
              AND pbrm.remote_host = ?
          WHERE
            pb.project_id = ?
            AND pb.branch_id = ?
          UNION ALL
          SELECT
            t.project_id,
            t.parent_branch_id,
            pbp.parent_branch_id,
            pbrm.remote_project_id,
            pbrm.remote_branch_id,
            t.depth + 1
          FROM
            t
          JOIN project_branch_parent AS pbp ON pbp.project_id = t.project_id
            AND pbp.branch_id = t.parent_branch_id
          LEFT JOIN project_branch_remote_mapping AS pbrm ON pbrm.local_project_id = t.project_id
          AND pbrm.local_branch_id = t.parent_branch_id
          AND pbrm.remote_host = ?
        )
        SELECT
          remote_project_id,
          remote_branch_id,
          depth
        FROM
          t
        $whereClause
        ORDER BY
          depth
        LIMIT 1
        |]

    whereClause :: Text
    whereClause =
      let clauses =
            foldr
              (\a b -> [trimming| $a AND $b |])
              [trimming| TRUE |]
              [ [trimming| remote_project_id IS NOT NULL |],
                selfRemoteFilter
              ]
       in [trimming| WHERE $clauses |]

    selfRemoteFilter = case loadRemoteBranchFlag of
      IncludeSelfRemote -> [trimming| TRUE |]
      ExcludeSelfRemote -> [trimming| depth > 0 |]

loadRemoteProject :: RemoteProjectId -> Text -> Transaction (Maybe RemoteProject)
loadRemoteProject rpid host =
  queryMaybeRow
    [sql|
      SELECT
        id,
        host,
        name
      FROM
        remote_project
      WHERE
        id = ?
        and host = ?
    |]
    (rpid, host)

ensureRemoteProject :: RemoteProjectId -> Text -> Text -> Transaction ()
ensureRemoteProject rpid host name =
  execute
    [sql|
      INSERT INTO remote_project (
        id,
        host,
        name)
      VALUES (
        ?,
        ?,
        ?)
      ON CONFLICT (
        id,
        host)
        -- should this update the name instead?
        DO NOTHING
        |]
    (rpid, host, name)

setRemoteProjectName :: RemoteProjectId -> Text -> Transaction ()
setRemoteProjectName rpid name =
  execute
    [sql|
      UPDATE
        remote_project
      SET
        name = ?
      WHERE
        id = ?
        |]
    (name, rpid)

loadRemoteBranch :: RemoteProjectId -> Text -> RemoteProjectBranchId -> Transaction (Maybe RemoteProjectBranch)
loadRemoteBranch rpid host rbid =
  queryMaybeRow
    [sql|
      SELECT
        project_id,
        branch_id,
        host,
        name
      FROM
        remote_project_branch
      WHERE
        project_id = ?
        AND branch_id = ?
        AND host = ?
    |]
    (rpid, host, rbid)

ensureRemoteProjectBranch :: RemoteProjectId -> Text -> RemoteProjectBranchId -> Text -> Transaction ()
ensureRemoteProjectBranch rpid host rbid name =
  execute
    [sql|
      INSERT INTO remote_project_branch (
        project_id,
        host,
        branch_id,
        name)
      VALUES (
        ?,
        ?,
        ?,
        ?)
      ON CONFLICT (
        project_id,
        branch_id,
        host)
        -- should this update the name instead?
        DO NOTHING
        |]
    (rpid, host, rbid, name)

setRemoteProjectBranchName :: RemoteProjectId -> Text -> RemoteProjectBranchId -> Text -> Transaction ()
setRemoteProjectBranchName rpid host rbid name =
  execute
    [sql|
      UPDATE
        remote_project_branch
      SET
        name = ?
      WHERE
        project_id = ?
        AND host = ?
        AND branch_id = ?
        |]
    (name, rpid, host, rbid)

insertBranchRemoteMapping ::
  ProjectId ->
  ProjectBranchId ->
  RemoteProjectId ->
  Text ->
  RemoteProjectBranchId ->
  Transaction ()
insertBranchRemoteMapping pid bid rpid host rbid =
  execute
    [sql|
      INSERT INTO project_branch_remote_mapping (
        local_project_id,
        local_branch_id,
        remote_project_id,
        remote_branch_id,
        remote_host)
      VALUES (
        ?,
        ?,
        ?,
        ?,
        ?)
        |]
    (pid, bid, rpid, rbid, host)

ensureBranchRemoteMapping ::
  ProjectId ->
  ProjectBranchId ->
  RemoteProjectId ->
  Text ->
  RemoteProjectBranchId ->
  Transaction ()
ensureBranchRemoteMapping pid bid rpid host rbid =
  execute
    [sql|
      INSERT INTO project_branch_remote_mapping (
        local_project_id,
        local_branch_id,
        remote_project_id,
        remote_branch_id,
        remote_host)
      VALUES (
        ?,
        ?,
        ?,
        ?,
        ?)
      ON CONFLICT (
        local_project_id,
        local_branch_id,
        remote_host)
        DO NOTHING
        |]
    (pid, bid, rpid, rbid, host)
