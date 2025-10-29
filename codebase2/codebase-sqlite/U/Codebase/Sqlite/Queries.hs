{-# LANGUAGE TemplateHaskell #-}

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

    -- ** name segments
    saveNameSegment,
    expectNameSegment,

    -- * hash table
    saveHash,
    saveHashes,
    saveHashHash,
    loadHashId,
    expectHash,
    expectHash32,
    expectBranchHash,
    expectBranchHashId,
    loadHashIdByHash,
    expectHashIdByHash,
    saveCausalHash,
    expectCausalHash,
    expectBranchHashForCausalHash,
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

    -- * namespace_statistics table
    saveNamespaceStats,
    loadNamespaceStatsByHashId,

    -- * causals

    -- ** causal table
    saveCausal,
    isCausalHash,
    causalExistsByHash32,
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
    tryGetSquashResult,
    saveSquashResult,

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
    projectExists,
    doProjectsExist,
    projectExistsByName,
    loadProject,
    loadProjectByName,
    expectProject,
    loadAllProjects,
    loadAllProjectsByRecentlyAccessed,
    loadAllProjectsBeginningWith,
    insertProject,
    renameProject,
    deleteProject,

    -- ** project branches
    projectBranchExists,
    projectBranchExistsByName,
    loadProjectBranchByName,
    loadProjectBranchByNames,
    expectProjectBranch,
    loadAllProjectBranchesBeginningWith,
    loadAllProjectBranchInfo,
    loadProjectAndBranchNames,
    loadAllProjectBranchNamePairs,
    loadProjectBranch,
    insertProjectBranch,
    renameProjectBranch,
    deleteProjectBranch,
    setProjectBranchHead,
    loadProjectBranchHead,
    expectProjectBranchHead,
    expectProjectBranchHeadHash,
    setMostRecentBranch,
    loadMostRecentBranch,
    loadProjectBranchParent,
    loadMergeBranchParents,
    insertMergeBranchLocal,
    insertMergeBranchRemote,
    insertMergeBranchLooseCode,
    loadNamespaceUniqueTypeGuid,
    existsAnyNamespaceUniqueTypeGuidForNamespace,
    ensureUniqueTypeToGuidMappingForCausalHashId,
    insertNamespaceUniqueTypeGuid,

    -- *** update branches
    loadUpdateBranchParentCausalHashId,
    setProjectBranchIsUpdateBranch,

    -- *** upgrade branches
    loadUpgradeBranchParentCausalHashId,
    setProjectBranchIsUpgradeBranch,

    -- ** remote projects
    loadRemoteProject,
    ensureRemoteProject,
    expectRemoteProjectName,
    setRemoteProjectName,
    loadRemoteProjectBranch,
    loadDefaultMergeTargetForLocalProjectBranch,

    -- ** remote project branches
    loadRemoteBranch,
    ensureRemoteProjectBranch,
    expectRemoteProjectBranchName,
    setRemoteProjectBranchName,
    insertBranchRemoteMapping,
    ensureBranchRemoteMapping,
    deleteBranchRemoteMapping,

    -- * indexes

    -- ** dependents index
    addToDependentsIndex,
    DependentsSelector (..),
    getDependentsForDependency,
    getDependentsForDependencyComponent,
    getDependencyIdsForDependent,
    getDependenciesBetweenTerms,
    getDirectDependenciesOfScope,
    getDirectDependentsWithinScope,
    getTransitiveDependentsWithinScope,
    getTransitiveDependentsGraphWithinScope,

    -- ** type index
    addToTypeIndex,
    getReferentsByType,
    getTypeReferenceForReferent,
    getTypeReferencesForComponent,
    filterTermsByReferenceHavingType,
    filterTermsByReferentHavingType,

    -- ** type mentions index
    addToTypeMentionsIndex,
    getReferentsByTypeMention,
    getTypeMentionsReferencesForComponent,

    -- * hash prefix lookup
    objectIdByBase32Prefix,
    namespaceHashIdByBase32Prefix,
    causalHashIdByBase32Prefix,

    -- * Reflog
    getDeprecatedRootReflog,
    appendProjectBranchReflog,
    getProjectReflog,
    getProjectBranchReflog,
    getGlobalReflog,

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
    clearTempEntityTables,

    -- * elaborate hashes
    elaborateHashes,

    -- * current project path
    expectCurrentProjectPath,
    setCurrentProjectPath,

    -- * migrations
    runCreateSql,
    addTempEntityTables,
    addReflogTable,
    fixScopedNameLookupTables,
    addNamespaceStatsTables,
    addProjectTables,
    addMostRecentBranchTable,
    addNameLookupMountTables,
    addMostRecentNamespaceTable,
    addSquashResultTable,
    addSquashResultTableIfNotExists,
    cdToProjectRoot,
    addCurrentProjectPathTable,
    addProjectBranchReflogTable,
    addProjectBranchCausalHashIdColumn,
    addProjectBranchLastAccessedColumn,
    addMergeBranchTables,
    addUpdateBranchTable,
    addDerivedDependentsByDependencyIndex,
    addUpgradeBranchTable,

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
    x2cDecl,
    checkBranchExistsForCausalHash,

    -- * Types
    TextPathSegments,
    JsonParseFailure (..),
  )
where

import Control.Lens (Lens')
import Control.Lens qualified as Lens
import Control.Monad.Extra ((||^))
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Writer (MonadWriter, runWriterT)
import Control.Monad.Writer qualified as Writer
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Bitraversable (bitraverse)
import Data.ByteString.Lazy (LazyByteString)
import Data.Bytes.Put (runPutS)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.List.NonEmpty qualified as Nel
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Time qualified as Time
import Data.Vector qualified as Vector
import Network.URI (URI)
import U.Codebase.Branch.Type (NamespaceStats (..))
import U.Codebase.Decl qualified as C
import U.Codebase.Decl qualified as C.Decl
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import U.Codebase.Reference (Reference' (..))
import U.Codebase.Reference qualified as C (Reference)
import U.Codebase.Reference qualified as C.Reference
import U.Codebase.Referent qualified as C.Referent
import U.Codebase.Reflog qualified as Reflog
import U.Codebase.Sqlite.Branch.Format qualified as NamespaceFormat
import U.Codebase.Sqlite.Causal qualified as Causal
import U.Codebase.Sqlite.Causal qualified as Sqlite.Causal
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
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Decl.Format qualified as S.Decl
import U.Codebase.Sqlite.Decode
import U.Codebase.Sqlite.Entity (SyncEntity)
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.HashHandle (HashHandle (..))
import U.Codebase.Sqlite.LocalIds
  ( LocalDefnId (..),
    LocalIds,
    LocalIds' (..),
    LocalTextId (..),
  )
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.ObjectType (ObjectType (DeclComponent, Namespace, Patch, TermComponent))
import U.Codebase.Sqlite.ObjectType qualified as ObjectType
import U.Codebase.Sqlite.Orphans ()
import U.Codebase.Sqlite.Patch.Format qualified as PatchFormat
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..), ProjectBranchRow (..))
import U.Codebase.Sqlite.ProjectReflog qualified as ProjectReflog
import U.Codebase.Sqlite.Reference qualified as S
import U.Codebase.Sqlite.Reference qualified as S.Reference
import U.Codebase.Sqlite.Referent qualified as S.Referent
import U.Codebase.Sqlite.RemoteProject (RemoteProject (..))
import U.Codebase.Sqlite.RemoteProjectBranch (RemoteProjectBranch)
import U.Codebase.Sqlite.Serialization as Serialization
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.TempEntity qualified as TempEntity
import U.Codebase.Sqlite.TempEntityType (TempEntityType)
import U.Codebase.Sqlite.TempEntityType qualified as TempEntityType
import U.Codebase.Sqlite.Term.Format qualified as S.Term
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Term qualified as C
import U.Codebase.Term qualified as C.Term
import U.Codebase.Type qualified as C.Type
import U.Codebase.WatchKind (WatchKind)
import U.Core.ABT qualified as ABT
import U.Util.Serialization qualified as S
import U.Util.Term qualified as TermUtil
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..), ProjectName (..))
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Hash32.Orphans.Sqlite ()
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude
import Unison.Sqlite
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Alternative qualified as Alternative
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.FileEmbed (embedProjectStringFile)
import Unison.Util.Lens qualified as Lens
import Unison.Util.Map qualified as Map
import UnliftIO qualified

debug :: Bool
debug = False

type TextPathSegments = [Text]

-- * main squeeze

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = 22

runCreateSql :: Transaction ()
runCreateSql =
  executeStatements $(embedProjectStringFile "sql/create.sql")

addTempEntityTables :: Transaction ()
addTempEntityTables =
  executeStatements $(embedProjectStringFile "sql/001-temp-entity-tables.sql")

addNamespaceStatsTables :: Transaction ()
addNamespaceStatsTables =
  executeStatements $(embedProjectStringFile "sql/003-namespace-statistics.sql")

-- | Deprecated in favour of project-branch reflog
addReflogTable :: Transaction ()
addReflogTable =
  executeStatements $(embedProjectStringFile "sql/002-reflog-table.sql")

fixScopedNameLookupTables :: Transaction ()
fixScopedNameLookupTables =
  executeStatements $(embedProjectStringFile "sql/004-fix-scoped-name-lookup-tables.sql")

addProjectTables :: Transaction ()
addProjectTables =
  executeStatements $(embedProjectStringFile "sql/005-project-tables.sql")

addMostRecentBranchTable :: Transaction ()
addMostRecentBranchTable =
  executeStatements $(embedProjectStringFile "sql/006-most-recent-branch-table.sql")

addNameLookupMountTables :: Transaction ()
addNameLookupMountTables =
  executeStatements $(embedProjectStringFile "sql/007-add-name-lookup-mounts.sql")

addMostRecentNamespaceTable :: Transaction ()
addMostRecentNamespaceTable =
  executeStatements $(embedProjectStringFile "sql/008-add-most-recent-namespace-table.sql")

addSquashResultTable :: Transaction ()
addSquashResultTable =
  executeStatements $(embedProjectStringFile "sql/009-add-squash-cache-table.sql")

-- | Added as a fix because 'addSquashResultTable' was missed in the createSchema action
-- for a portion of time.
addSquashResultTableIfNotExists :: Transaction ()
addSquashResultTableIfNotExists =
  executeStatements $(embedProjectStringFile "sql/010-ensure-squash-cache-table.sql")

cdToProjectRoot :: Transaction ()
cdToProjectRoot =
  executeStatements $(embedProjectStringFile "sql/011-cd-to-project-root.sql")

addCurrentProjectPathTable :: Transaction ()
addCurrentProjectPathTable =
  executeStatements $(embedProjectStringFile "sql/012-add-current-project-path-table.sql")

-- | Deprecated in favour of project-branch reflog
addProjectBranchReflogTable :: Transaction ()
addProjectBranchReflogTable =
  executeStatements $(embedProjectStringFile "sql/013-add-project-branch-reflog-table.sql")

addProjectBranchCausalHashIdColumn :: Transaction ()
addProjectBranchCausalHashIdColumn =
  executeStatements $(embedProjectStringFile "sql/014-add-project-branch-causal-hash-id.sql")

addProjectBranchLastAccessedColumn :: Transaction ()
addProjectBranchLastAccessedColumn =
  executeStatements $(embedProjectStringFile "sql/015-add-project-branch-last-accessed.sql")

addMergeBranchTables :: Transaction ()
addMergeBranchTables =
  executeStatements $(embedProjectStringFile "sql/016-add-merge-branch-tables.sql")

addUpdateBranchTable :: Transaction ()
addUpdateBranchTable =
  executeStatements $(embedProjectStringFile "sql/017-add-update-branch-table.sql")

addDerivedDependentsByDependencyIndex :: Transaction ()
addDerivedDependentsByDependencyIndex =
  executeStatements $(embedProjectStringFile "sql/018-add-derived-dependents-by-dependency-index.sql")

addUpgradeBranchTable :: Transaction ()
addUpgradeBranchTable =
  executeStatements $(embedProjectStringFile "sql/019-add-upgrade-branch-table.sql")

schemaVersion :: Transaction SchemaVersion
schemaVersion =
  queryOneCol
    [sql|
      SELECT version
      FROM schema_version
    |]

data UnexpectedSchemaVersion = UnexpectedSchemaVersion
  { actual :: SchemaVersion,
    expected :: SchemaVersion
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

-- | Expect the given schema version.
expectSchemaVersion :: SchemaVersion -> Transaction ()
expectSchemaVersion expected =
  queryOneColCheck
    [sql|
      SELECT version
      FROM schema_version
    |]
    (\actual -> if actual /= expected then Left UnexpectedSchemaVersion {actual, expected} else Right ())

setSchemaVersion :: SchemaVersion -> Transaction ()
setSchemaVersion schemaVersion =
  execute
    [sql|
      UPDATE schema_version
      SET version = :schemaVersion
    |]

countObjects :: Transaction Int
countObjects = queryOneCol [sql| SELECT COUNT(*) FROM object |]

countCausals :: Transaction Int
countCausals = queryOneCol [sql| SELECT COUNT(*) FROM causal |]

countWatches :: Transaction Int
countWatches = queryOneCol [sql| SELECT COUNT(*) FROM watch |]

saveHash :: Hash32 -> Transaction HashId
saveHash hash = do
  execute
    [sql|
      INSERT INTO hash (base32) VALUES (:hash)
      ON CONFLICT DO NOTHING
    |]
  expectHashId hash

saveHashes :: (Traversable f) => f Hash32 -> Transaction (f HashId)
saveHashes hashes = do
  for_ hashes \hash ->
    execute
      [sql|
        INSERT INTO hash (base32)
        VALUES (:hash)
        ON CONFLICT DO NOTHING
      |]
  traverse expectHashId hashes

saveHashHash :: Hash -> Transaction HashId
saveHashHash = saveHash . Hash32.fromHash

loadHashId :: Hash32 -> Transaction (Maybe HashId)
loadHashId hash = queryMaybeCol (loadHashIdSql hash)

expectHashId :: Hash32 -> Transaction HashId
expectHashId hash = queryOneCol (loadHashIdSql hash)

loadHashIdSql :: Hash32 -> Sql
loadHashIdSql hash =
  [sql|
    SELECT id
    FROM hash
    WHERE base32 = :hash COLLATE NOCASE
  |]

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
expectHash32 h =
  queryOneCol
    [sql|
      SELECT base32
      FROM hash
      WHERE id = :h
    |]

expectBranchHash :: BranchHashId -> Transaction BranchHash
expectBranchHash = coerce expectHash

expectBranchHashForCausalHash :: CausalHash -> Transaction BranchHash
expectBranchHashForCausalHash ch = do
  (_, bhId) <- expectCausalByCausalHash ch
  expectBranchHash bhId

saveText :: Text -> Transaction TextId
saveText t = do
  execute
    [sql|
      INSERT INTO text (text)
      VALUES (:t)
      ON CONFLICT DO NOTHING
    |]
  expectTextId t

saveTexts :: (Traversable f) => f Text -> Transaction (f TextId)
saveTexts =
  traverse saveText

loadTextId :: Text -> Transaction (Maybe TextId)
loadTextId t = queryMaybeCol (loadTextIdSql t)

expectTextId :: Text -> Transaction TextId
expectTextId t = queryOneCol (loadTextIdSql t)

loadTextIdSql :: Text -> Sql
loadTextIdSql t =
  [sql|
    SELECT id
    FROM text
    WHERE text = :t
  |]

expectText :: TextId -> Transaction Text
expectText h = queryOneCol (loadTextSql h)

expectTextCheck :: (SqliteExceptionReason e) => TextId -> (Text -> Either e a) -> Transaction a
expectTextCheck h = queryOneColCheck (loadTextSql h)

loadTextSql :: TextId -> Sql
loadTextSql h =
  [sql|
    SELECT text
    FROM text
    WHERE id = :h
  |]

saveNameSegment :: NameSegment -> Transaction TextId
saveNameSegment =
  saveText . NameSegment.toUnescapedText

expectNameSegment :: TextId -> Transaction NameSegment
expectNameSegment =
  fmap NameSegment . expectText

saveHashObject :: HashId -> ObjectId -> HashVersion -> Transaction ()
saveHashObject hId oId version =
  execute
    [sql|
      INSERT INTO hash_object (hash_id, object_id, hash_version)
      VALUES (:hId, :oId, :version)
      ON CONFLICT DO NOTHING
    |]

saveObject ::
  HashHandle ->
  HashId ->
  ObjectType ->
  ByteString ->
  Transaction ObjectId
saveObject hh h t blob = do
  execute
    [sql|
      INSERT INTO object (primary_hash_id, type_id, bytes)
      VALUES (:h, :t, :blob)
      ON CONFLICT DO NOTHING
    |]
  oId <- expectObjectIdForPrimaryHashId h
  saveHashObject h oId 2 -- todo: remove this from here, and add it to other relevant places once there are v1 and v2 hashes
  rowsModified >>= \case
    0 -> pure ()
    _ -> do
      hash <- expectHash32 h
      tryMoveTempEntityDependents hh hash
  pure oId

expectObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectObject oId check =
  queryOneColCheck
    [sql|
      SELECT bytes
      FROM object
      WHERE id = :oId
    |]
    check

loadObjectOfType ::
  (SqliteExceptionReason e) =>
  ObjectId ->
  ObjectType ->
  (ByteString -> Either e a) ->
  Transaction (Maybe a)
loadObjectOfType oid ty =
  queryMaybeColCheck (loadObjectOfTypeSql oid ty) -- (oid, ty)

expectObjectOfType :: (SqliteExceptionReason e) => ObjectId -> ObjectType -> (ByteString -> Either e a) -> Transaction a
expectObjectOfType oid ty =
  queryOneColCheck (loadObjectOfTypeSql oid ty)

loadObjectOfTypeSql :: ObjectId -> ObjectType -> Sql
loadObjectOfTypeSql oid ty =
  [sql|
    SELECT bytes
    FROM object
    WHERE id = :oid
      AND type_id = :ty
  |]

-- | Load a decl component object.
loadDeclObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction (Maybe a)
loadDeclObject oid =
  loadObjectOfType oid DeclComponent

-- | Expect a decl component object.
expectDeclObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectDeclObject oid =
  expectObjectOfType oid DeclComponent

-- | Load a namespace object.
loadNamespaceObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction (Maybe a)
loadNamespaceObject oid =
  loadObjectOfType oid Namespace

-- | Expect a namespace object.
expectNamespaceObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectNamespaceObject oid =
  expectObjectOfType oid Namespace

-- | Load a patch object.
loadPatchObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction (Maybe a)
loadPatchObject oid =
  loadObjectOfType oid Patch

-- | Expect a patch object.
expectPatchObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectPatchObject oid =
  expectObjectOfType oid Patch

-- | Load a term component object.
loadTermObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction (Maybe a)
loadTermObject oid =
  loadObjectOfType oid TermComponent

-- | Expect a term component object.
expectTermObject :: (SqliteExceptionReason e) => ObjectId -> (ByteString -> Either e a) -> Transaction a
expectTermObject oid =
  expectObjectOfType oid TermComponent

expectPrimaryHashIdForObject :: ObjectId -> Transaction HashId
expectPrimaryHashIdForObject oId = do
  queryOneCol
    [sql|
      SELECT primary_hash_id
      FROM object
      WHERE id = :oId
    |]

expectObjectWithType :: (SqliteExceptionReason e) => ObjectId -> (ObjectType -> ByteString -> Either e a) -> Transaction a
expectObjectWithType oId check =
  queryOneRowCheck
    [sql|
      SELECT type_id, bytes
      FROM object
      WHERE id = :oId
    |]
    (\(typ, bytes) -> check typ bytes)

expectObjectWithHashIdAndType :: ObjectId -> Transaction (HashId, ObjectType, ByteString)
expectObjectWithHashIdAndType oId =
  queryOneRow
    [sql|
      SELECT primary_hash_id, type_id, bytes
      FROM object
      WHERE id = :oId
    |]

loadObjectIdForPrimaryHashId :: HashId -> Transaction (Maybe ObjectId)
loadObjectIdForPrimaryHashId h =
  queryMaybeCol (loadObjectIdForPrimaryHashIdSql h)

-- | Not all hashes have corresponding objects; e.g., hashes of term types
expectObjectIdForPrimaryHashId :: HashId -> Transaction ObjectId
expectObjectIdForPrimaryHashId h =
  queryOneCol (loadObjectIdForPrimaryHashIdSql h)

loadObjectIdForPrimaryHashIdSql :: HashId -> Sql
loadObjectIdForPrimaryHashIdSql h =
  [sql|
    SELECT id
    FROM object
    WHERE primary_hash_id = :h
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
    [sql|
      SELECT object.id
      FROM object
      JOIN hash ON object.primary_hash_id = hash.id
      WHERE hash.base32 = :hash COLLATE NOCASE
    |]

expectBranchObjectIdForHash32 :: Hash32 -> Transaction BranchObjectId
expectBranchObjectIdForHash32 =
  fmap BranchObjectId . expectObjectIdForHash32

expectPatchObjectIdForHash32 :: Hash32 -> Transaction PatchObjectId
expectPatchObjectIdForHash32 =
  fmap PatchObjectId . expectObjectIdForHash32

expectBranchHashIdForHash32 :: Hash32 -> Transaction BranchHashId
expectBranchHashIdForHash32 hash =
  queryOneCol
    [sql|
      SELECT hash.id FROM object
      INNER JOIN hash_object ON hash_object.object_id = object.id
      INNER JOIN hash ON hash_object.hash_id = hash.id
      WHERE object.type_id = 2
        AND hash.base32 = :hash COLLATE NOCASE
    |]

expectBranchHashId :: BranchHash -> Transaction BranchHashId
expectBranchHashId = expectBranchHashIdForHash32 . Hash32.fromHash . unBranchHash

expectCausalHashIdForHash32 :: Hash32 -> Transaction CausalHashId
expectCausalHashIdForHash32 hash =
  queryOneCol
    [sql|
      SELECT self_hash_id
      FROM causal INNER JOIN hash ON hash.id = self_hash_id
      WHERE base32 = :hash COLLATE NOCASE
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
  queryMaybeCol (loadObjectIdForAnyHashIdSql h)

expectObjectIdForAnyHashId :: HashId -> Transaction ObjectId
expectObjectIdForAnyHashId h =
  queryOneCol (loadObjectIdForAnyHashIdSql h)

loadObjectIdForAnyHashIdSql :: HashId -> Sql
loadObjectIdForAnyHashIdSql h =
  [sql|
    SELECT object_id
    FROM hash_object
    WHERE hash_id = :h
  |]

-- | Does a hash correspond to an object?
isObjectHash :: HashId -> Transaction Bool
isObjectHash h =
  queryOneCol
    -- sql (Only h)
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM object
        WHERE primary_hash_id = :h
      )
    |]

-- | All objects have corresponding hashes.
expectPrimaryHashByObjectId :: ObjectId -> Transaction Hash
expectPrimaryHashByObjectId =
  fmap Hash32.toHash . expectPrimaryHash32ByObjectId

expectPrimaryHash32ByObjectId :: ObjectId -> Transaction Hash32
expectPrimaryHash32ByObjectId oId =
  queryOneCol
    [sql|
      SELECT hash.base32
      FROM hash INNER JOIN object ON object.primary_hash_id = hash.id
      WHERE object.id = :oId
    |]

expectHashIdsForObject :: ObjectId -> Transaction (NonEmpty HashId)
expectHashIdsForObject oId = do
  -- sql1 (Only oId)
  primaryHashId <- queryOneCol [sql| SELECT primary_hash_id FROM object WHERE id = :oId |]
  hashIds <- queryListCol [sql| SELECT hash_id FROM hash_object WHERE object_id = :oId |]
  pure $ primaryHashId Nel.:| filter (/= primaryHashId) hashIds

hashIdWithVersionForObject :: ObjectId -> Transaction [(HashId, HashVersion)]
hashIdWithVersionForObject oId =
  queryListRow
    [sql|
      SELECT hash_id, hash_version
      FROM hash_object
      WHERE object_id = :oId
    |]

-- | @recordObjectRehash old new@ records that object @old@ was rehashed and inserted as a new object, @new@.
--
-- This function rewrites @old@'s @hash_object@ rows in place to point at the new object.
recordObjectRehash :: ObjectId -> ObjectId -> Transaction ()
recordObjectRehash old new =
  execute
    [sql|
      UPDATE hash_object
      SET object_id = :new
      WHERE object_id = :old
    |]

-- | Maybe we would generalize this to something other than NamespaceHash if we
-- end up wanting to store other kinds of Causals here too.
saveCausal ::
  HashHandle ->
  CausalHashId ->
  BranchHashId ->
  [CausalHashId] ->
  Transaction ()
saveCausal hh self value parents = do
  execute
    [sql|
      INSERT INTO causal (self_hash_id, value_hash_id)
      VALUES (:self, :value)
      ON CONFLICT DO NOTHING
    |]
  rowsModified >>= \case
    0 -> pure ()
    _ -> do
      for_ parents \parent ->
        execute
          [sql|
            INSERT INTO causal_parent (causal_id, parent_id)
            VALUES (:self, :parent)
          |]
      flushCausalDependents hh self

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
      [sql|
        DELETE FROM temp_entity_missing_dependency
        WHERE dependency = :dependency
        RETURNING dependent
      |]
  traverse_ flushIfReadyToFlush dependents
  where
    flushIfReadyToFlush :: Hash32 -> Transaction ()
    flushIfReadyToFlush dependent = do
      readyToFlush dependent >>= \case
        True -> moveTempEntityToMain hh dependent
        False -> pure ()

    readyToFlush :: Hash32 -> Transaction Bool
    readyToFlush hash =
      queryOneCol
        [sql|
          SELECT EXISTS (
            SELECT 1
            FROM temp_entity
            WHERE hash = :hash
          ) AND NOT EXISTS (
            SELECT 1
            FROM temp_entity_missing_dependency
            WHERE dependent = :hash
          )
        |]

expectCausal :: CausalHashId -> Transaction Causal.SyncCausalFormat
expectCausal hashId = do
  valueHash <-
    queryOneCol
      [sql|
        SELECT value_hash_id
        FROM causal
        WHERE self_hash_id = :hashId
      |]
  parents <-
    fmap Vector.fromList do
      -- is the random ordering from the database ok? (seems so, for now)
      queryListCol
        [sql|
          SELECT parent_id
          FROM causal_parent
          WHERE causal_id = :hashId
        |]
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
  queryOneRowCheck
    [sql|
      SELECT blob, type_id
      FROM temp_entity
      WHERE hash = :hash
    |]
    \(blob, typeId) ->
      case typeId of
        TempEntityType.TermComponentType -> Entity.TC <$> decodeTempTermFormat blob
        TempEntityType.DeclComponentType -> Entity.DC <$> decodeTempDeclFormat blob
        TempEntityType.NamespaceType -> Entity.N <$> decodeTempNamespaceFormat blob
        TempEntityType.PatchType -> Entity.P <$> decodeTempPatchFormat blob
        TempEntityType.CausalType -> Entity.C <$> decodeTempCausalFormat blob

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
  queryOneCol (loadCausalValueHashIdSql id) -- (Only id)

expectCausalHash :: CausalHashId -> Transaction CausalHash
expectCausalHash = coerce expectHash

loadCausalValueHashId :: HashId -> Transaction (Maybe BranchHashId)
loadCausalValueHashId id =
  queryMaybeCol (loadCausalValueHashIdSql id)

loadCausalValueHashIdSql :: HashId -> Sql
loadCausalValueHashIdSql id =
  [sql|
    SELECT value_hash_id
    FROM causal
    WHERE self_hash_id = :id
  |]

isCausalHash :: HashId -> Transaction Bool
isCausalHash hash =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM causal
        WHERE self_hash_id = :hash
      )
    |]

-- | Return whether or not a causal exists with the given hash32.
causalExistsByHash32 :: Hash32 -> Transaction Bool
causalExistsByHash32 hash =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM causal
        JOIN hash ON causal.self_hash_id = hash.id
        WHERE hash.base32 = :hash
      )
    |]

loadBranchObjectIdByCausalHashId :: CausalHashId -> Transaction (Maybe BranchObjectId)
loadBranchObjectIdByCausalHashId id = queryMaybeCol (loadBranchObjectIdByCausalHashIdSql id)

expectBranchObjectIdByCausalHashId :: CausalHashId -> Transaction BranchObjectId
expectBranchObjectIdByCausalHashId id = queryOneCol (loadBranchObjectIdByCausalHashIdSql id)

loadBranchObjectIdByCausalHashIdSql :: CausalHashId -> Sql
loadBranchObjectIdByCausalHashIdSql id =
  [sql|
    SELECT object_id FROM hash_object
    INNER JOIN causal ON hash_id = causal.value_hash_id
    WHERE causal.self_hash_id = :id
  |]

expectBranchObjectIdByBranchHashId :: BranchHashId -> Transaction BranchObjectId
expectBranchObjectIdByBranchHashId id = queryOneCol (loadBranchObjectIdByBranchHashIdSql id)

loadBranchObjectIdByBranchHashId :: BranchHashId -> Transaction (Maybe BranchObjectId)
loadBranchObjectIdByBranchHashId id = queryMaybeCol (loadBranchObjectIdByBranchHashIdSql id)

loadBranchObjectIdByBranchHashIdSql :: BranchHashId -> Sql
loadBranchObjectIdByBranchHashIdSql id =
  [sql|
    SELECT object_id FROM hash_object
    WHERE hash_id = :id
  |]

saveCausalParents :: CausalHashId -> [CausalHashId] -> Transaction ()
saveCausalParents child =
  traverse_ \parent ->
    execute
      [sql|
        INSERT INTO causal_parent (causal_id, parent_id)
        VALUES (:child, :parent)
        ON CONFLICT DO NOTHING
      |]

loadCausalParents :: CausalHashId -> Transaction [CausalHashId]
loadCausalParents h =
  queryListCol
    [sql|
      SELECT parent_id
      FROM causal_parent
      WHERE causal_id = :h
    |]

-- | Like 'loadCausalParents', but the input and outputs are hashes, not hash ids.
loadCausalParentsByHash :: Hash32 -> Transaction [Hash32]
loadCausalParentsByHash hash =
  queryListCol
    [sql|
      SELECT h2.base32
      FROM causal_parent cp
      JOIN hash h1 ON cp.causal_id = h1.id
      JOIN hash h2 ON cp.parent_id = h2.id
      WHERE h1.base32 = :hash COLLATE NOCASE
    |]

saveWatch :: WatchKind -> S.Reference.IdH -> ByteString -> Transaction ()
saveWatch k r blob = do
  execute
    [sql|
      INSERT INTO watch_result (hash_id, component_index, result)
      VALUES (@r, @, :blob)
      ON CONFLICT DO NOTHING
    |]
  execute
    [sql|
      INSERT INTO watch (hash_id, component_index, watch_kind_id)
      VALUES (@r, @, :k)
      ON CONFLICT DO NOTHING
    |]

loadWatch ::
  (SqliteExceptionReason e) =>
  WatchKind ->
  S.Reference.IdH ->
  (ByteString -> Either e a) ->
  Transaction (Maybe a)
loadWatch k r check =
  queryMaybeColCheck
    [sql|
      SELECT result FROM watch_result
      INNER JOIN watch
        ON watch_result.hash_id = watch.hash_id
        AND watch_result.component_index = watch.component_index
      WHERE watch.watch_kind_id = :k
        AND watch.hash_id = @r
        AND watch.component_index = @
    |]
    check

loadWatchKindsByReference :: S.Reference.IdH -> Transaction [WatchKind]
loadWatchKindsByReference r =
  queryListCol
    [sql|
      SELECT watch_kind_id FROM watch_result
      INNER JOIN watch
        ON watch_result.hash_id = watch.hash_id
        AND watch_result.component_index = watch.component_index
      WHERE watch.hash_id = @r
        AND watch.component_index = @
    |]

loadWatchesByWatchKind :: WatchKind -> Transaction [S.Reference.IdH]
loadWatchesByWatchKind k =
  queryListRow
    [sql|
      SELECT hash_id, component_index
      FROM watch
      WHERE watch_kind_id = :k
    |]

-- | Delete all watches that were put by 'putWatch'.
clearWatches :: Transaction ()
clearWatches = do
  execute [sql| DELETE FROM watch_result |]
  execute [sql| DELETE FROM watch |]

-- * Index-building

addToTypeIndex :: S.ReferenceH -> S.Referent.Id -> Transaction ()
addToTypeIndex tp tm =
  execute
    [sql|
      INSERT INTO find_type_index (
        type_reference_builtin,
        type_reference_hash_id,
        type_reference_component_index,
        term_referent_object_id,
        term_referent_component_index,
        term_referent_constructor_index
      ) VALUES (@tp, @, @, @tm, @, @)
      ON CONFLICT DO NOTHING
    |]

getReferentsByType :: S.ReferenceH -> Transaction [S.Referent.Id]
getReferentsByType r =
  queryListRow
    [sql|
      SELECT
        term_referent_object_id,
        term_referent_component_index,
        term_referent_constructor_index
      FROM find_type_index
      WHERE type_reference_builtin IS @r
        AND type_reference_hash_id IS @
        AND type_reference_component_index IS @
    |]

getTypeReferenceForReferent :: S.Referent.Id -> Transaction S.ReferenceH
getTypeReferenceForReferent r =
  queryOneRow
    [sql|
      SELECT
        type_reference_builtin,
        type_reference_hash_id,
        type_reference_component_index
      FROM find_type_index
      WHERE term_referent_object_id = @r
        AND term_referent_component_index = @
        AND term_referent_constructor_index IS @
    |]

-- todo: error if no results
getTypeReferencesForComponent :: ObjectId -> Transaction [(S.ReferenceH, S.Referent.Id)]
getTypeReferencesForComponent oId =
  fmap (map fixupTypeIndexRow) $
    queryListRow
      [sql|
        SELECT
          type_reference_builtin,
          type_reference_hash_id,
          type_reference_component_index,
          term_referent_object_id,
          term_referent_component_index,
          term_referent_constructor_index
        FROM find_type_index
        WHERE term_referent_object_id = :oId
      |]

filterTermsByReferentHavingType :: S.ReferenceH -> [S.Referent.Id] -> Transaction [S.Referent.Id]
filterTermsByReferentHavingType typ terms = create *> for_ terms insert *> select <* drop
  where
    select =
      queryListRow
        [sql|
          SELECT
            q.term_referent_object_id,
            q.term_referent_component_index,
            q.term_referent_constructor_index
          FROM filter_query q, find_type_index t
          WHERE t.type_reference_builtin IS :typeBuiltin
            AND t.type_reference_hash_id IS :typeHashId
            AND t.type_reference_component_index IS :typeComponentIndex
            AND t.term_referent_object_id = q.term_referent_object_id
            AND t.term_referent_component_index = q.term_referent_component_index
            AND t.term_referent_constructor_index IS q.term_referent_constructor_index
        |]
    insert r =
      execute
        [sql|
          INSERT INTO filter_query (
            term_referent_object_id,
            term_referent_component_index,
            term_referent_constructor_index
          ) VALUES (@r, @, @)
        |]
    typeBuiltin :: Maybe TextId = Lens.preview C.Reference.t_ typ
    typeHashId :: Maybe HashId = Lens.preview (C.Reference._ReferenceDerived . C.Reference.idH) typ
    typeComponentIndex :: Maybe C.Reference.Pos = Lens.preview (C.Reference._ReferenceDerived . C.Reference.idPos) typ
    create =
      execute
        [sql|
          CREATE TEMPORARY TABLE filter_query (
            term_referent_object_id INTEGER NOT NULL,
            term_referent_component_index INTEGER NOT NULL,
            term_referent_constructor_index INTEGER NULL
          )
        |]
    drop = execute [sql|DROP TABLE filter_query|]

filterTermsByReferenceHavingType :: S.ReferenceH -> [S.Reference.Id] -> Transaction [S.Reference.Id]
filterTermsByReferenceHavingType typ terms = create *> for_ terms insert *> select <* drop
  where
    select =
      queryListRow
        [sql|
          SELECT
            q.term_reference_object_id,
            q.term_reference_component_index
          FROM filter_query q, find_type_index t
          WHERE t.type_reference_builtin IS :typeBuiltin
            AND t.type_reference_hash_id IS :typeHashId
            AND t.type_reference_component_index IS :typeComponentIndex
            AND t.term_referent_object_id = q.term_reference_object_id
            AND t.term_referent_component_index = q.term_reference_component_index
            AND t.term_referent_constructor_index IS NULL
        |]
    insert r =
      execute
        [sql|
          INSERT INTO filter_query (
            term_reference_object_id,
            term_reference_component_index
          ) VALUES (@r, @)
        |]
    typeBuiltin :: Maybe TextId = Lens.preview C.Reference.t_ typ
    typeHashId :: Maybe HashId = Lens.preview (C.Reference._ReferenceDerived . C.Reference.idH) typ
    typeComponentIndex :: Maybe C.Reference.Pos = Lens.preview (C.Reference._ReferenceDerived . C.Reference.idPos) typ
    create =
      execute
        [sql|
          CREATE TEMPORARY TABLE filter_query (
            term_reference_object_id INTEGER NOT NULL,
            term_reference_component_index INTEGER NOT NULL
          )
        |]
    drop = execute [sql|DROP TABLE filter_query|]

addToTypeMentionsIndex :: S.ReferenceH -> S.Referent.Id -> Transaction ()
addToTypeMentionsIndex tp tm =
  execute
    [sql|
      INSERT INTO find_type_mentions_index (
        type_reference_builtin,
        type_reference_hash_id,
        type_reference_component_index,
        term_referent_object_id,
        term_referent_component_index,
        term_referent_constructor_index
      ) VALUES (@tp, @, @, @tm, @, @)
      ON CONFLICT DO NOTHING
    |]

getReferentsByTypeMention :: S.ReferenceH -> Transaction [S.Referent.Id]
getReferentsByTypeMention r =
  queryListRow
    [sql|
      SELECT
        term_referent_object_id,
        term_referent_component_index,
        term_referent_constructor_index
      FROM find_type_mentions_index
      WHERE type_reference_builtin IS @r
        AND type_reference_hash_id IS @
        AND type_reference_component_index IS @
    |]

-- todo: error if no results
getTypeMentionsReferencesForComponent :: ObjectId -> Transaction [(S.ReferenceH, S.Referent.Id)]
getTypeMentionsReferencesForComponent r =
  fmap (map fixupTypeIndexRow) $
    queryListRow
      [sql|
        SELECT
          type_reference_builtin,
          type_reference_hash_id,
          type_reference_component_index,
          term_referent_object_id,
          term_referent_component_index,
          term_referent_constructor_index
        FROM find_type_mentions_index
        WHERE term_referent_object_id IS :r
      |]

fixupTypeIndexRow :: S.ReferenceH :. S.Referent.Id -> (S.ReferenceH, S.Referent.Id)
fixupTypeIndexRow (rh :. ri) = (rh, ri)

-- | Delete objects without hashes. An object typically *would* have a hash, but (for example) during a migration in which an object's hash
-- may change, its corresponding hash_object row may be updated to point at a new version of that object. This procedure clears out all
-- references to objects that do not have any corresponding hash_object rows.
garbageCollectObjectsWithoutHashes :: Transaction ()
garbageCollectObjectsWithoutHashes = do
  execute
    [sql|
      CREATE TEMPORARY TABLE object_without_hash AS
        SELECT id
        FROM object
        WHERE id NOT IN (
          SELECT object_id
          FROM hash_object
        )
    |]
  execute
    [sql|
      DELETE FROM dependents_index
      WHERE dependency_object_id IN object_without_hash
        OR dependent_object_id IN object_without_hash
    |]
  execute
    [sql|
      DELETE FROM find_type_index
      WHERE term_referent_object_id IN object_without_hash
    |]
  execute
    [sql|
      DELETE FROM find_type_mentions_index
      WHERE term_referent_object_id IN object_without_hash
    |]
  execute
    [sql|
      DELETE FROM object
      WHERE id IN object_without_hash
    |]
  execute
    [sql|
      DROP TABLE object_without_hash
    |]

-- | Delete all
garbageCollectWatchesWithoutObjects :: Transaction ()
garbageCollectWatchesWithoutObjects = do
  execute
    [sql|
      DELETE FROM watch
      WHERE watch.hash_id NOT IN
      (SELECT hash_object.hash_id FROM hash_object)
    |]

addToDependentsIndex :: [S.Reference] -> S.Reference.Id -> Transaction ()
addToDependentsIndex dependencies dependent =
  for_ dependencies \dependency ->
    execute
      [sql|
        INSERT INTO dependents_index (
          dependency_builtin,
          dependency_object_id,
          dependency_component_index,
          dependent_object_id,
          dependent_component_index
        )
        VALUES (@dependency, @, @, @dependent, @)
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
getDependentsForDependency :: DependentsSelector -> S.Reference -> Transaction (Set S.Reference.Id)
getDependentsForDependency selector dependency = do
  dependents <-
    queryListRow
      [sql|
        SELECT dependent_object_id, dependent_component_index
        FROM dependents_index
        WHERE dependency_builtin IS @dependency
          AND dependency_object_id IS @
          AND dependency_component_index IS @
      |]
  pure . Set.fromList $
    case selector of
      IncludeAllDependents -> dependents
      ExcludeSelf -> filter isNotSelfReference dependents
      ExcludeOwnComponent -> filter isNotReferenceFromOwnComponent dependents
  where
    isNotReferenceFromOwnComponent :: S.Reference.Id -> Bool
    isNotReferenceFromOwnComponent =
      case dependency of
        ReferenceBuiltin _ -> const True
        ReferenceDerived (C.Reference.Id oid0 _pos0) -> \(C.Reference.Id oid1 _pos1) -> oid0 /= oid1

    isNotSelfReference :: S.Reference.Id -> Bool
    isNotSelfReference =
      case dependency of
        ReferenceBuiltin _ -> const True
        ReferenceDerived ref -> (ref /=)

getDependentsForDependencyComponent :: ObjectId -> Transaction [S.Reference.Id]
getDependentsForDependencyComponent dependency =
  filter isNotSelfReference
    <$> queryListRow
      [sql|
        SELECT dependent_object_id, dependent_component_index
        FROM dependents_index
        WHERE dependency_builtin IS NULL
          AND dependency_object_id IS :dependency
      |]
  where
    isNotSelfReference :: S.Reference.Id -> Bool
    isNotSelfReference = \case
      (C.Reference.Id oid1 _pos1) -> dependency /= oid1

-- | Get non-self, user-defined dependencies of a user-defined dependent.
getDependencyIdsForDependent :: S.Reference.Id -> Transaction [S.Reference.Id]
getDependencyIdsForDependent dependent@(C.Reference.Id oid0 _) =
  fmap (filter isNotSelfReference) $
    queryListRow
      [sql|
        SELECT dependency_object_id, dependency_component_index
        FROM dependents_index
        WHERE dependency_builtin IS NULL
          AND dependent_object_id = @dependent
          AND dependent_component_index = @
      |]
  where
    isNotSelfReference :: S.Reference.Id -> Bool
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
  queryListCol theSql <&> Set.fromList
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
    --  +-paths-------------------------+
    --  +-level-+-path_last-+-path_init-+
    --  |     0 |         X |        '' | -- path: [X]
    --  |     0 |         Y |        '' | -- path: [Y]
    --  |     1 |         B |      'X,' | -- path: [X,B]   -- ends in B, yay!
    --  |     1 |         Q |      'X,' | -- path: [X,Q]
    --  |     1 |         Z |      'Y,' | -- path: [Y,Z]
    --  |     2 |         B |    'Z,Y,' | -- path: [Y,Z,B] -- ends in B, yay!
    --  +-------+-----------+-----------+
    --
    -- Next, we seed another recursive CTE with those paths that end in the sink `B`. This is just the (very verbose)
    -- way to unnest an array in SQLite. All we're doing is turning the set of strings {'X,' 'Z,Y,'}, each of which
    -- represents the inner nodes of a full path between `A` and `B`, into the set {X Z Y}, which is just the full set
    -- of such inner nodes, along any path.
    --
    --  +-elems-----------------+
    --  +-path_elem-+-path_init-+
    --  |           |      'X,' |
    --  |           |    'Z,Y,' |
    --  |       'X' |        '' |
    --  |       'Z' |      'Y,' |
    --  |       'Y' |        '' |
    --  +-----------+-----------+
    --
    -- And finally, we just select out the non-null `path_elem` rows from here, casting the strings back to integers for
    -- clarity (this isn't very matter - SQLite would cast on-the-fly).
    --
    --  +-path_elem-+
    --  |         X |
    --  |         Z |
    --  |         Y |
    --  +-----------+
    --
    -- Notes
    --
    -- (1) We only care about term dependencies, not type dependencies. This is because a type can only depend on types,
    --     not terms, so there is no point in searching through a type's transitive dependencies looking for our sink.
    -- (2) No need to search beyond the sink itself, since component dependencies form a DAG.
    -- (3) An explicit cast from e.g. string '1' to int 1 isn't strictly necessary.
    theSql :: Sql
    theSql =
      [sql|
        WITH RECURSIVE paths(level, path_last, path_init) AS (
          SELECT
            0,
            dependents_index.dependency_object_id,
            ''
          FROM dependents_index
            JOIN object ON dependents_index.dependency_object_id = object.id
          WHERE dependents_index.dependent_object_id = :oid1
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
            AND paths.path_last != :oid2 -- Note (2)
          ORDER BY level DESC
        ),
        elems(path_elem, path_init) AS (
          SELECT null, path_init
          FROM paths
          WHERE paths.path_last = :oid2
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

getDirectDependenciesOfScope ::
  (S.Reference -> Transaction Bool) ->
  DefnsF Set S.TermReferenceId S.TypeReferenceId ->
  Transaction (DefnsF Set S.TermReference S.TypeReference)
getDirectDependenciesOfScope isBuiltinType scope = do
  let tempTableName = [sql| temp_dependents |]

  -- Populate a temporary table with all of the references in `scope`
  createTemporaryTableOfReferenceIds tempTableName
  for_ scope.terms \ref -> execute [sql| INSERT INTO $tempTableName VALUES (@ref, @) |]
  for_ scope.types \ref -> execute [sql| INSERT INTO $tempTableName VALUES (@ref, @) |]

  -- Get their direct dependencies (tagged with object type)
  --
  -- Builtins don't have an object type – just a textual string. We have to figure out if it's a term or a type outside
  -- of the codebase.
  dependencies0 <-
    queryListRow @(S.Reference :. Only (Maybe ObjectType))
      [sql|
        SELECT d.dependency_builtin, d.dependency_object_id, d.dependency_component_index, o.type_id
        FROM dependents_index d
          LEFT JOIN object o ON d.dependency_object_id = o.id
        WHERE (d.dependent_object_id, d.dependent_component_index) IN (
          SELECT object_id, component_index
          FROM $tempTableName
        )
          AND
            -- Filter out self-dependencies
            ((d.dependency_object_id, d.dependency_component_index) IS DISTINCT FROM (d.dependent_object_id, d.dependent_component_index))
      |]

  -- Drop the temporary table
  execute [sql| DROP TABLE $tempTableName |]

  -- Post-process the query result
  dependencies1 <-
    Foldable.foldlM
      ( \deps -> \case
          dep :. Only (Just TermComponent) -> pure $! deps & over #terms (Set.insert dep)
          dep :. Only (Just DeclComponent) -> pure $! deps & over #types (Set.insert dep)
          dep :. Only Nothing ->
            isBuiltinType dep <&> \case
              False -> deps & over #terms (Set.insert dep)
              True -> deps & over #types (Set.insert dep)
          _ -> pure deps -- impossible; could error here
      )
      (Defns Set.empty Set.empty)
      dependencies0

  pure dependencies1

-- | `getDirectDependentsWithinScope scope query` returns all direct dependents of `query` that are in `scope`.
getDirectDependentsWithinScope ::
  DefnsF Set S.TermReferenceId S.TypeReferenceId ->
  DefnsF Set S.TermReference S.TypeReference ->
  Transaction (DefnsF Set S.TermReferenceId S.TypeReferenceId)
getDirectDependentsWithinScope scope query = do
  -- Populate a temporary table with all of the references in `scope`
  let scopeTableName = [sql| dependents_search_scope |]
  createTemporaryTableOfReferenceIds scopeTableName
  for_ scope.terms \ref -> execute [sql| INSERT INTO $scopeTableName VALUES (@ref, @) |]
  for_ scope.types \ref -> execute [sql| INSERT INTO $scopeTableName VALUES (@ref, @) |]

  -- Populate a temporary table with all of the references in `query`
  let queryTableName = [sql| dependencies_query |]
  createTemporaryTableOfReferences queryTableName
  for_ query.terms \ref -> execute [sql| INSERT INTO $queryTableName VALUES (@ref, @, @) |]
  for_ query.types \ref -> execute [sql| INSERT INTO $queryTableName VALUES (@ref, @, @) |]

  -- Get their direct dependents (tagged with object type)
  dependents0 <-
    queryListRow @(S.Reference.Id :. Only ObjectType)
      [sql|
        SELECT s.object_id, s.component_index, o.type_id
        FROM $queryTableName q
          JOIN dependents_index d
            ON q.builtin IS d.dependency_builtin
            AND q.object_id IS d.dependency_object_id
            AND q.component_index IS d.dependency_component_index
          JOIN $scopeTableName s
            ON d.dependent_object_id = s.object_id
            AND d.dependent_component_index = s.component_index
          JOIN object o ON s.object_id = o.id
          WHERE
            -- Filter out self-dependents
            ((d.dependency_object_id, d.dependency_component_index) IS DISTINCT FROM (d.dependent_object_id, d.dependent_component_index))
      |]

  -- Drop the temporary tables
  execute [sql| DROP TABLE $scopeTableName |]
  execute [sql| DROP TABLE $queryTableName |]

  -- Post-process the query result
  let dependents1 =
        List.foldl'
          ( \deps -> \case
              dep :. Only TermComponent -> Defns (Set.insert dep deps.terms) deps.types
              dep :. Only DeclComponent -> Defns deps.terms (Set.insert dep deps.types)
              _ -> deps -- impossible; could error here
          )
          (Defns Set.empty Set.empty)
          dependents0

  pure dependents1

-- | `getTransitiveDependentsWithinScope scope query` returns all transitive dependents of `query` that are in `scope`.
getTransitiveDependentsWithinScope ::
  DefnsF Set S.TermReferenceId S.TypeReferenceId ->
  DefnsF Set S.TermReference S.TypeReference ->
  Transaction (DefnsF Set S.TermReferenceId S.TypeReferenceId)
getTransitiveDependentsWithinScope scope query = do
  -- Populate a temporary table with all of the references in `scope`
  let scopeTableName = [sql| dependents_search_scope |]
  createTemporaryTableOfReferenceIds scopeTableName
  for_ scope.terms \ref -> execute [sql| INSERT INTO $scopeTableName VALUES (@ref, @) |]
  for_ scope.types \ref -> execute [sql| INSERT INTO $scopeTableName VALUES (@ref, @) |]

  -- Populate a temporary table with all of the references in `query`
  let queryTableName = [sql| dependencies_query |]
  createTemporaryTableOfReferences queryTableName
  for_ query.terms \ref -> execute [sql| INSERT INTO $queryTableName VALUES (@ref, @, @) |]
  for_ query.types \ref -> execute [sql| INSERT INTO $queryTableName VALUES (@ref, @, @) |]

  -- Say the query set is { #foo, #bar }, and the scope set is { #foo, #bar, #baz, #qux, #honk }.
  --
  -- Furthermore, say the dependencies are as follows, where `x -> y` means "x depends on y".
  --
  --   #honk -> #baz -> #foo
  --            #qux -> #bar
  --
  -- The recursive query below is seeded with direct dependents of the `query` set that are in `scope`, namely:
  --
  --   #honk -> #baz -> #foo
  --            #qux -> #bar
  --            ^^^^
  --            direct deps of { #foo, #bar } are: { #baz, #qux }
  --
  -- Then, every iteration of the query expands to that set's dependents (#honk and onwards), until there are no more.
  -- We use `UNION` rather than `UNION ALL` so as to not track down the transitive dependents of any particular
  -- reference more than once.

  result0 :: [S.Reference.Id :. Only ObjectType] <-
    queryListRow
      [sql|
        WITH RECURSIVE
        dependents_index_in_scope AS (
          SELECT *
          FROM dependents_index
          WHERE (dependent_object_id, dependent_component_index) IN (
            SELECT object_id, component_index
            FROM $scopeTableName
          )
          -- Ignore self-dependents
          AND ((dependency_object_id, dependency_component_index) IS DISTINCT FROM (dependent_object_id, dependent_component_index))
        ),
        transitive_dependents (object_id, component_index, type_id) AS (
          SELECT d.dependent_object_id, d.dependent_component_index, o.type_id
          FROM $queryTableName q
            JOIN dependents_index_in_scope d
              ON d.dependency_builtin IS q.builtin
                AND d.dependency_object_id IS q.object_id
                AND d.dependency_component_index IS q.component_index
            JOIN object o ON d.dependent_object_id = o.id
          UNION
          SELECT d.dependent_object_id, d.dependent_component_index, o.type_id
          FROM transitive_dependents t
            JOIN dependents_index_in_scope d
              ON t.object_id = d.dependency_object_id
              AND t.component_index = d.dependency_component_index
            JOIN object o ON d.dependent_object_id = o.id
        )
        SELECT *
        FROM transitive_dependents
      |]

  execute [sql| DROP TABLE $scopeTableName |]
  execute [sql| DROP TABLE $queryTableName |]

  -- Post-process the query result
  let result1 =
        List.foldl'
          ( \deps -> \case
              dep :. Only TermComponent -> let !terms = Set.insert dep deps.terms in Defns terms deps.types
              dep :. Only DeclComponent -> let !types = Set.insert dep deps.types in Defns deps.terms types
              _ -> deps -- impossible; could error here
          )
          (Defns Set.empty Set.empty)
          result0

  pure result1

-- | Like 'getTransitiveDependentsWithinScope', but returns the dependents as a searchable adjacency matrix rather than
-- just a set of references.
--
-- Returns (dependent ref, dependent type, dependency, dependency type)
getTransitiveDependentsGraphWithinScope ::
  DefnsF Set S.TermReferenceId S.TypeReferenceId ->
  DefnsF Set S.TermReference S.TypeReference ->
  Transaction [S.Reference.Id :. Only ObjectType :. S.Reference :. Only (Maybe ObjectType)]
getTransitiveDependentsGraphWithinScope scope query = do
  -- Populate a temporary table with all of the references in `scope`
  let scopeTableName = [sql| dependents_search_scope |]
  createTemporaryTableOfReferenceIds scopeTableName
  for_ scope.terms \ref -> execute [sql| INSERT INTO $scopeTableName VALUES (@ref, @) |]
  for_ scope.types \ref -> execute [sql| INSERT INTO $scopeTableName VALUES (@ref, @) |]

  -- Populate a temporary table with all of the references in `query`
  let queryTableName = [sql| dependencies_query |]
  createTemporaryTableOfReferences queryTableName
  for_ query.terms \ref -> execute [sql| INSERT INTO $queryTableName VALUES (@ref, @, @) |]
  for_ query.types \ref -> execute [sql| INSERT INTO $queryTableName VALUES (@ref, @, @) |]

  result :: [S.Reference.Id :. Only ObjectType :. S.Reference :. Only (Maybe ObjectType)] <-
    queryListRow
      [sql|
        WITH RECURSIVE
        dependents_index_in_scope AS (
          SELECT *
          FROM dependents_index
          WHERE
            (dependent_object_id, dependent_component_index) IN (
              SELECT object_id, component_index
              FROM $scopeTableName
            )
            AND (dependency_object_id, dependency_component_index)
              IS DISTINCT FROM (dependent_object_id, dependent_component_index)
        ),
        transitive_dependents AS (
          SELECT *
          FROM dependents_index_in_scope
          WHERE
            (dependency_builtin IS NULL AND
              (dependency_object_id, dependency_component_index) IN (
                SELECT dependency_object_id, dependency_component_index
                FROM $queryTableName
                WHERE dependency_builtin IS NULL
              )
            )
            OR
            (dependency_builtin IS NOT NULL AND
              dependency_builtin IN (
                SELECT dependency_builtin
                FROM $queryTableName
                WHERE dependency_builtin IS NOT NULL
              )
            )
          UNION
          SELECT d.*
          FROM transitive_dependents t
            JOIN dependents_index_in_scope d
              ON t.dependent_object_id = d.dependency_object_id
              AND t.dependent_component_index = d.dependency_component_index
        )
        SELECT
          t.dependent_object_id,
          t.dependent_component_index,
          o1.type_id,
          t.dependency_builtin,
          t.dependency_object_id,
          t.dependency_component_index,
          o2.type_id
        FROM transitive_dependents t
          JOIN object o1 ON t.dependent_object_id = o1.id
          LEFT JOIN object o2 ON t.dependency_object_id = o2.id
      |]

  execute [sql| DROP TABLE $scopeTableName |]
  execute [sql| DROP TABLE $queryTableName |]

  pure result

createTemporaryTableOfReferences :: Sql -> Transaction ()
createTemporaryTableOfReferences tableName = do
  execute
    [sql|
      CREATE TEMPORARY TABLE $tableName (
        builtin INTEGER NULL,
        object_id INTEGER NULL,
        component_index INTEGER NULL
        CHECK ((builtin IS NULL) = (object_id IS NOT NULL)),
        CHECK ((object_id IS NULL) = (component_index IS NULL))
      )
    |]

createTemporaryTableOfReferenceIds :: Sql -> Transaction ()
createTemporaryTableOfReferenceIds tableName = do
  execute
    [sql|
      CREATE TEMPORARY TABLE $tableName (
        object_id INTEGER NOT NULL,
        component_index INTEGER NOT NULL,
        PRIMARY KEY (object_id, component_index)
      )
    |]

objectIdByBase32Prefix :: ObjectType -> Text -> Transaction [ObjectId]
objectIdByBase32Prefix objType prefix =
  queryListCol
    [sql|
      SELECT object.id FROM object
      INNER JOIN hash_object ON hash_object.object_id = object.id
      INNER JOIN hash ON hash_object.hash_id = hash.id
      WHERE object.type_id = :objType
        AND hash.base32 LIKE :prefix2 ESCAPE '\'
    |]
  where
    prefix2 = likeEscape '\\' prefix <> "%"

causalHashIdByBase32Prefix :: Text -> Transaction [CausalHashId]
causalHashIdByBase32Prefix prefix =
  queryListCol
    [sql|
      SELECT self_hash_id FROM causal
      INNER JOIN hash ON id = self_hash_id
      WHERE base32 LIKE :prefix2 ESCAPE '\'
    |]
  where
    prefix2 = prefix <> "%"

namespaceHashIdByBase32Prefix :: Text -> Transaction [BranchHashId]
namespaceHashIdByBase32Prefix prefix =
  queryListCol
    [sql|
      SELECT value_hash_id FROM causal
      INNER JOIN hash ON id = value_hash_id
      WHERE base32 LIKE :prefix2 ESCAPE '\'
    |]
  where
    prefix2 = prefix <> "%"

-- | Finds all causals that refer to a branch for which we don't have an object stored.
-- Although there are plans to support this in the future, currently all such cases
-- are the result of database inconsistencies and are unexpected.
getCausalsWithoutBranchObjects :: Transaction [CausalHashId]
getCausalsWithoutBranchObjects =
  queryListCol
    [sql|
      SELECT self_hash_id from causal
      WHERE value_hash_id NOT IN (
        SELECT hash_id
        FROM hash_object
      )
    |]

-- | Delete all hash objects of a given hash version.
-- Leaves the corresponding `hash`es in the hash table alone.
removeHashObjectsByHashingVersion :: HashVersion -> Transaction ()
removeHashObjectsByHashingVersion hashVersion =
  execute
    [sql|
      DELETE FROM hash_object
      WHERE hash_version = :hashVersion
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
    '*' -> "[*]"
    '?' -> "[?]"
    '[' -> "[[]"
    ']' -> "[]]"
    c -> Text.singleton c

-- | @before x y@ returns whether or not @x@ occurred before @y@, i.e. @x@ is an ancestor of @y@.
before :: CausalHashId -> CausalHashId -> Transaction Bool
before x y =
  queryOneCol
    [sql|
      SELECT EXISTS (
        $selectAncestorsOfY
        WHERE ancestor.id = :x
      )
    |]
  where
    selectAncestorsOfY = ancestorSql y

lca :: CausalHashId -> CausalHashId -> Transaction (Maybe CausalHashId)
lca alice bob =
  queryMaybeCol
    [sql|
      WITH RECURSIVE history_one (causal_id) AS (
        SELECT :alice
        UNION
        SELECT causal_parent.parent_id
        FROM history_one
          JOIN causal_parent ON history_one.causal_id = causal_parent.causal_id
      ),
      history_two (causal_id) AS (
        SELECT :bob
        UNION
        SELECT causal_parent.parent_id
        FROM history_two
          JOIN causal_parent ON history_two.causal_id = causal_parent.causal_id
      ),
      common_ancestors (causal_id) AS (
        SELECT causal_id
        FROM history_one
        INTERSECT
        SELECT causal_id
        FROM history_two
        ORDER BY causal_id DESC
      )
      SELECT causal_id
      FROM common_ancestors
      WHERE NOT EXISTS (
        SELECT 1
        FROM causal_parent
        WHERE causal_parent.parent_id = common_ancestors.causal_id
          AND EXISTS (
            SELECT 1
            FROM common_ancestors c
            WHERE c.causal_id = causal_parent.causal_id
          )
      )
      LIMIT 1
    |]

ancestorSql :: CausalHashId -> Sql
ancestorSql h =
  [sql|
    WITH RECURSIVE
      ancestor(id) AS (
        SELECT self_hash_id
          FROM causal
          WHERE self_hash_id = :h
        UNION
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
  deriving (Eq, Show, Ord)

-- | Where is an entity stored?
entityLocation :: Hash32 -> Transaction (Maybe EntityLocation)
entityLocation hash =
  entityExists hash >>= \case
    True -> pure (Just EntityInMainStorage)
    False -> do
      let theSql = [sql| SELECT EXISTS (SELECT 1 FROM temp_entity WHERE hash = :hash) |]
      queryOneCol theSql <&> \case
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
    Just chId ->
      queryOneCol
        [sql|
          SELECT EXISTS (
            SELECT 1
            FROM causal c JOIN object o ON c.value_hash_id = o.primary_hash_id
            WHERE c.self_hash_id = :chId
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
    [sql|
      INSERT INTO temp_entity (hash, blob, type_id)
      VALUES (:entityHash, :entityBlob, :entityType)
      ON CONFLICT DO NOTHING
    |]

  for_ (NEMap.toList missingDependencies) \(depHash, depHashJwt) ->
    execute
      [sql|
        INSERT INTO temp_entity_missing_dependency (dependent, dependency, dependencyJwt)
        VALUES (:entityHash, :depHash, :depHashJwt)
      |]
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
    [sql|
      DELETE
      FROM temp_entity
      WHERE hash = :hash
    |]

-- | Clears the `temp_entity` and `temp_entity_missing_dependency` tables.
-- The hashjwts stored in temp entity tables can sometimes go stale, so we clear them out.
-- This is safe because temp entities are generally considered ephemeral
-- except during an active pull.
clearTempEntityTables :: Transaction ()
clearTempEntityTables = do
  execute [sql| DELETE FROM temp_entity_missing_dependency |]
  execute [sql| DELETE FROM temp_entity |]

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
elaborateHashes :: NonEmpty Hash32 -> Transaction [Text]
elaborateHashes (coerce @_ @(NonEmpty (Only Hash32)) -> hashes) =
  queryListCol
    [sql|
      WITH RECURSIVE
        new_temp_entity_dependents (hash) AS (VALUES :hashes),
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
        either (unsafeIO . UnliftIO.throwIO) pure $ unsyncTermComponent x

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
        either (unsafeIO . UnliftIO.throwIO) pure $ unsyncDeclComponent xs

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
                    ReferenceBuiltin t -> ReferenceBuiltin (tIds Vector.! fromIntegral t)
                    C.Reference.Derived Nothing i -> C.Reference.Derived oId i -- index self-references
                    C.Reference.Derived (Just h) i -> C.Reference.Derived (oIds Vector.! fromIntegral h) i
                  getTypeSRef :: S.Term.TypeRef -> S.Reference
                  getTypeSRef = \case
                    ReferenceBuiltin t -> ReferenceBuiltin (tIds Vector.! fromIntegral t)
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
s2cDecl ids decl = do
  substTypeRef <- localIdsToTypeRefLookup ids
  pure $ x2cDecl substTypeRef decl

x2cDecl :: (r -> r1) -> (C.Decl.DeclR r Symbol -> C.Decl.DeclR r1 Symbol)
x2cDecl substTypeRef (C.Decl.DataDeclaration dt m b ct) = C.Decl.DataDeclaration dt m b (C.Type.rmap substTypeRef <$> ct)

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
              ReferenceBuiltin t -> ReferenceBuiltin (tIds Vector.! fromIntegral t)
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
  execute
    [sql|
      INSERT INTO namespace_statistics (
        namespace_hash_id,
        num_contained_terms,
        num_contained_types,
        num_contained_patches
      )
      VALUES (:bhId, @stats, @, @)
    |]

-- | Looks up statistics for a given branch, there's no guarantee that we have
-- computed and saved stats for any given branch.
loadNamespaceStatsByHashId :: BranchHashId -> Transaction (Maybe NamespaceStats)
loadNamespaceStatsByHashId bhId = do
  queryMaybeRow
    [sql|
      SELECT num_contained_terms, num_contained_types, num_contained_patches
      FROM namespace_statistics
      WHERE namespace_hash_id = :bhId
    |]

getDeprecatedRootReflog :: Int -> Transaction [Reflog.Entry CausalHashId Text]
getDeprecatedRootReflog numEntries =
  queryListRow
    [sql|
      SELECT time, from_root_causal_id, to_root_causal_id, reason
      FROM reflog
      ORDER BY time DESC
      LIMIT :numEntries
    |]

appendProjectBranchReflog :: ProjectReflog.Entry ProjectId ProjectBranchId CausalHashId -> Transaction ()
appendProjectBranchReflog entry =
  execute
    [sql|
      INSERT INTO project_branch_reflog (project_id, project_branch_id, time, from_root_causal_id, to_root_causal_id, reason)
      VALUES (@entry, @, @, @, @, @)
    |]

-- | Get x number of entries from the project reflog for the provided project
getProjectReflog :: Int -> ProjectId -> Transaction [ProjectReflog.Entry ProjectId ProjectBranchId CausalHashId]
getProjectReflog numEntries projectId =
  queryListRow
    [sql|
      SELECT project_id, project_branch_id, time, from_root_causal_id, to_root_causal_id, reason
      FROM project_branch_reflog
      WHERE project_id = :projectId
      ORDER BY
        time DESC,
        -- Strictly for breaking ties in transcripts with the same time,
        -- this will break ties in the correct order, sorting later inserted rows first.
        ROWID DESC
      LIMIT :numEntries
    |]

-- | Get x number of entries from the project reflog for the provided branch.
getProjectBranchReflog :: Int -> ProjectBranchId -> Transaction [ProjectReflog.Entry ProjectId ProjectBranchId CausalHashId]
getProjectBranchReflog numEntries projectBranchId =
  queryListRow
    [sql|
      SELECT project_id, project_branch_id, time, from_root_causal_id, to_root_causal_id, reason
      FROM project_branch_reflog
      WHERE project_branch_id = :projectBranchId
      ORDER BY
        time DESC,
        -- Strictly for breaking ties in transcripts with the same time,
        -- this will break ties in the correct order, sorting later inserted rows first.
        ROWID DESC
      LIMIT :numEntries
    |]

-- | Get x number of entries from the global reflog spanning all projects
getGlobalReflog :: Int -> Transaction [ProjectReflog.Entry ProjectId ProjectBranchId CausalHashId]
getGlobalReflog numEntries =
  queryListRow
    [sql|
      SELECT project_id, project_branch_id, time, from_root_causal_id, to_root_causal_id, reason
      FROM project_branch_reflog
      ORDER BY
        time DESC,
        -- Strictly for breaking ties in transcripts with the same time,
        -- this will break ties in the correct order, sorting later inserted rows first.
        ROWID DESC
      LIMIT :numEntries
    |]

-- | Does a project exist with this id?
projectExists :: ProjectId -> Transaction Bool
projectExists projectId =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM project
        WHERE id = :projectId
      )
    |]

-- | Check if any projects exist
doProjectsExist :: Transaction Bool
doProjectsExist =
  queryOneCol
    [sql| SELECT EXISTS (SELECT 1 FROM project) |]

-- | Does a project exist by this name?
projectExistsByName :: ProjectName -> Transaction Bool
projectExistsByName name =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM project
        WHERE name = :name
      )
    |]

loadProject :: ProjectId -> Transaction (Maybe Project)
loadProject pid = queryMaybeRow (loadProjectSql pid)

expectProject :: ProjectId -> Transaction Project
expectProject pid = queryOneRow (loadProjectSql pid)

loadProjectSql :: ProjectId -> Sql
loadProjectSql pid =
  [sql|
    SELECT
      id,
      name
    FROM
      project
    WHERE
      id = :pid
  |]

loadProjectByName :: ProjectName -> Transaction (Maybe Project)
loadProjectByName name =
  queryMaybeRow
    [sql|
      SELECT
        id,
        name
      FROM
        project
      WHERE
        name = :name
    |]

-- | Load all projects.
loadAllProjects :: Transaction [Project]
loadAllProjects =
  queryListRow
    [sql|
      SELECT id, name
      FROM project
      ORDER BY name ASC
    |]

-- | Load all projects.
loadAllProjectsByRecentlyAccessed :: Transaction [Project]
loadAllProjectsByRecentlyAccessed =
  queryListRow
    [sql|
      SELECT project.id, project.name
      FROM project
        JOIN project_branch ON project.id = project_branch.project_id
      ORDER BY project_branch.last_accessed DESC NULLS LAST, project.name ASC
    |]

-- | Load all projects whose name matches a prefix.
loadAllProjectsBeginningWith :: Maybe Text -> Transaction [Project]
loadAllProjectsBeginningWith mayPrefix = do
  let prefixGlob = maybe "*" (\prefix -> (globEscape prefix <> "*")) mayPrefix
  queryListRow
    [sql|
        SELECT id, name
        FROM project
        WHERE name GLOB :prefixGlob
        ORDER BY name ASC
      |]

-- | Insert a `project` row.
insertProject :: ProjectId -> ProjectName -> Transaction ()
insertProject uuid name =
  execute
    [sql|
      INSERT INTO project (id, name)
      VALUES (:uuid, :name)
    |]

-- | Rename a `project` row.
--
-- Precondition: the new name is available.
renameProject :: ProjectId -> ProjectName -> Transaction ()
renameProject projectId name =
  execute
    [sql|
      UPDATE project
      SET name = :name
      WHERE id = :projectId
    |]

-- | Does a project branch exist?
projectBranchExists :: ProjectId -> ProjectBranchId -> Transaction Bool
projectBranchExists projectId branchId =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM project_branch
        WHERE project_id = :projectId
          AND branch_id = :branchId
      )
    |]

-- | Does a project branch exist by this name?
projectBranchExistsByName :: ProjectId -> ProjectBranchName -> Transaction Bool
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
            project_id = :projectId
            AND name = :name)
    |]

loadProjectBranch :: ProjectId -> ProjectBranchId -> Transaction (Maybe ProjectBranch)
loadProjectBranch projectId branchId =
  fmap mungeLoadProjectBranchResult <$> queryMaybeRow (loadProjectBranchSql projectId branchId)

expectProjectBranch :: ProjectId -> ProjectBranchId -> Transaction ProjectBranch
expectProjectBranch projectId branchId =
  mungeLoadProjectBranchResult <$> queryOneRow (loadProjectBranchSql projectId branchId)

loadProjectBranchSql :: ProjectId -> ProjectBranchId -> Sql
loadProjectBranchSql projectId branchId =
  [sql|
    SELECT
      project_branch.project_id,
      project_branch.branch_id,
      project_branch.name,
      project_branch_parent.parent_branch_id,
      EXISTS (SELECT 1 FROM merge_branch WHERE project_id = :projectId AND branch_id = :branchId),
      EXISTS (SELECT 1 FROM update_branch WHERE project_id = :projectId AND branch_id = :branchId),
      EXISTS (SELECT 1 FROM upgrade_branch WHERE project_id = :projectId AND branch_id = :branchId)
    FROM
      project_branch
      LEFT JOIN project_branch_parent ON project_branch.project_id = project_branch_parent.project_id
        AND project_branch.branch_id = project_branch_parent.branch_id
    WHERE
      project_branch.project_id = :projectId
      AND project_branch.branch_id = :branchId
  |]

mungeLoadProjectBranchResult ::
  (ProjectId, ProjectBranchId, ProjectBranchName, Maybe ProjectBranchId, Bool, Bool, Bool) ->
  ProjectBranch
mungeLoadProjectBranchResult (projectId, branchId, name, parentBranchId, isMerge, isUpdate, isUpgrade) =
  ProjectBranch {projectId, branchId, name, parentBranchId, isMerge, isUpdate, isUpgrade}

loadProjectBranchByName :: ProjectId -> ProjectBranchName -> Transaction (Maybe ProjectBranch)
loadProjectBranchByName projectId name = do
  maybeProjectBranchRow <-
    queryMaybeRow
      [sql|
        SELECT
          project_branch.project_id,
          project_branch.branch_id,
          project_branch.name,
          project_branch_parent.parent_branch_id
        FROM project_branch
          LEFT JOIN project_branch_parent ON project_branch.project_id = project_branch_parent.project_id
            AND project_branch.branch_id = project_branch_parent.branch_id
        WHERE
          project_branch.project_id = :projectId
          AND project_branch.name = :name
      |]
  case maybeProjectBranchRow of
    Just projectBranchRow -> Just <$> loadProjectBranchByProjectBranchRow projectBranchRow
    Nothing -> pure Nothing

loadProjectBranchByProjectBranchRow :: ProjectBranchRow -> Transaction ProjectBranch
loadProjectBranchByProjectBranchRow branch = do
  isMerge <- projectBranchIsMergeBranch branch.projectId branch.branchId
  isUpdate <- projectBranchIsUpdateBranch branch.projectId branch.branchId
  isUpgrade <- projectBranchIsUpgradeBranch branch.projectId branch.branchId
  pure
    ProjectBranch
      { projectId = branch.projectId,
        branchId = branch.branchId,
        name = branch.name,
        parentBranchId = branch.parentBranchId,
        isMerge,
        isUpdate,
        isUpgrade
      }

loadProjectBranchByNames :: ProjectName -> ProjectBranchName -> Transaction (Maybe ProjectBranch)
loadProjectBranchByNames projectName branchName = do
  maybeProjectBranchRow <-
    queryMaybeRow
      [sql|
        SELECT
          project_branch.project_id,
          project_branch.branch_id,
          project_branch.name,
          project_branch_parent.parent_branch_id
        FROM project
          JOIN project_branch ON project.id = project_branch.project_id
          LEFT JOIN project_branch_parent ON project_branch.project_id = project_branch_parent.project_id
            AND project_branch.branch_id = project_branch_parent.branch_id
        WHERE
          project.name = :projectName
          AND project_branch.name = :branchName
      |]
  case maybeProjectBranchRow of
    Just projectBranchRow -> Just <$> loadProjectBranchByProjectBranchRow projectBranchRow
    Nothing -> pure Nothing

-- | Load all branch id/name pairs in a project whose name matches an optional prefix.
loadAllProjectBranchesBeginningWith :: ProjectId -> Maybe Text -> Transaction [(ProjectBranchId, ProjectBranchName)]
loadAllProjectBranchesBeginningWith projectId mayPrefix =
  let prefixGlob = maybe "*" (\prefix -> (globEscape prefix <> "*")) mayPrefix
   in queryListRow
        [sql|
        SELECT project_branch.branch_id, project_branch.name
        FROM project_branch
        WHERE project_branch.project_id = :projectId
          AND project_branch.name GLOB :prefixGlob
        ORDER BY project_branch.name ASC
      |]

-- | Load ALL project/branch name pairs
-- Useful for autocomplete/fuzzy-finding
loadAllProjectBranchNamePairs :: Transaction [(ProjectAndBranch ProjectName ProjectBranchName, ProjectAndBranch ProjectId ProjectBranchId)]
loadAllProjectBranchNamePairs =
  queryListRow
    [sql|
      SELECT
        project.name,
        project_branch.name,
        project.id,
        project_branch.branch_id
      FROM
        project
        JOIN project_branch ON project.id = project_branch.project_id
      ORDER BY project_branch.last_accessed DESC NULLS LAST, project.name ASC, project_branch.name ASC
    |]
    <&> fmap \(projectName, branchName, projectId, branchId) ->
      ( ProjectAndBranch projectName branchName,
        ProjectAndBranch projectId branchId
      )

-- | Load info about all branches in a project, for display by the @branches@ command.
--
-- Each branch name maps to a possibly-empty collection of associated remote branches.
loadAllProjectBranchInfo :: ProjectId -> Transaction (Map ProjectBranchName (Map URI (ProjectName, ProjectBranchName)))
loadAllProjectBranchInfo projectId =
  fmap postprocess $
    queryListRow
      [sql|
        SELECT
          pb.name AS local_branch_name,
          rpb.host AS host,
          rp.name AS remote_project_name,
          rpb.name AS remote_branch_name
        FROM project_branch AS pb
        LEFT JOIN project_branch_remote_mapping AS pbrm ON pb.project_id = pbrm.local_project_id
          AND pb.branch_id = pbrm.local_branch_id
        LEFT JOIN remote_project AS rp ON pbrm.remote_project_id = rp.id
        LEFT JOIN remote_project_branch AS rpb ON pbrm.remote_project_id = rpb.project_id
          AND pbrm.remote_branch_id = rpb.branch_id
        WHERE pb.project_id = :projectId
        ORDER BY local_branch_name ASC, host ASC, remote_project_name ASC, remote_branch_name ASC
      |]
  where
    -- Each input tuple is the local branch name, plus either:
    --
    --   1. One of 1+ (host, remote project, remote branch) triplets, indicating this local branch is associated with 1+
    --      remote branches (with distinct hosts)
    --
    --      *or*
    --
    --   2. Three Nothings, indicating this local branch is associated with 0 remote branches.
    postprocess ::
      [(ProjectBranchName, Maybe URI, Maybe ProjectName, Maybe ProjectBranchName)] ->
      Map ProjectBranchName (Map URI (ProjectName, ProjectBranchName))
    postprocess =
      foldl' f Map.empty
      where
        f ::
          Map ProjectBranchName (Map URI (ProjectName, ProjectBranchName)) ->
          (ProjectBranchName, Maybe URI, Maybe ProjectName, Maybe ProjectBranchName) ->
          Map ProjectBranchName (Map URI (ProjectName, ProjectBranchName))
        f !acc (localBranchName, maybeHost, maybeRemoteProjectName, maybeRemoteBranchName) =
          Map.upsert g localBranchName acc
          where
            g :: Maybe (Map URI (ProjectName, ProjectBranchName)) -> Map URI (ProjectName, ProjectBranchName)
            g maybeRemoteBranches =
              case (maybeHost, maybeRemoteProjectName, maybeRemoteBranchName) of
                -- One more remote (host, project name, branch name) tuple to collect, either as a singleton map
                -- (because it's the first we've seen for this local branch), or as a map insert (because it's not).
                (Just host, Just remoteProjectName, Just remoteBranchName) ->
                  case maybeRemoteBranches of
                    Nothing -> Map.singleton host (remoteProjectName, remoteBranchName)
                    Just remoteBranches -> Map.insert host (remoteProjectName, remoteBranchName) remoteBranches
                -- We know these three are all Nothing (this local branch has no associated remote branches)
                -- No need to pattern match on maybeRemoteBranches; we know it's Nothing, too
                _ -> Map.empty

loadProjectAndBranchNames :: ProjectId -> ProjectBranchId -> Transaction (Maybe (ProjectName, ProjectBranchName))
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
        project_branch.project_id = :projectId
        AND project_branch.branch_id = :branchId
    |]

-- | Insert a project branch.
insertProjectBranch :: (HasCallStack) => Text -> CausalHashId -> ProjectBranchRow -> Transaction ()
insertProjectBranch description causalHashId branch = do
  let projectId = branch.projectId
  let branchId = branch.branchId
  let branchName = branch.name
  let maybeParentBranchId = branch.parentBranchId

  -- Ensure we never point at a causal we don't have the branch for.
  _ <- expectBranchObjectIdByCausalHashId causalHashId

  execute
    [sql|
      INSERT INTO project_branch (project_id, branch_id, name, causal_hash_id)
        VALUES (:projectId, :branchId, :branchName, :causalHashId)
    |]
  updateProjectBranchLastAccessed projectId branchId
  whenJust maybeParentBranchId \parentBranchId ->
    execute
      [sql|
        INSERT INTO project_branch_parent (project_id, parent_branch_id, branch_id)
          VALUES (:projectId, :parentBranchId, :branchId)
      |]
  time <- Sqlite.unsafeIO $ Time.getCurrentTime
  appendProjectBranchReflog $
    ProjectReflog.Entry
      { project = projectId,
        branch = branchId,
        time,
        fromRootCausalHash = Nothing,
        toRootCausalHash = causalHashId,
        reason = description
      }

-- | Rename a project branch.
--
-- Precondition: the new name is available.
renameProjectBranch :: ProjectId -> ProjectBranchId -> ProjectBranchName -> Transaction ()
renameProjectBranch projectId branchId branchName = do
  execute
    [sql|
      UPDATE project_branch
      SET name = :branchName
      WHERE project_id = :projectId
        AND branch_id = :branchId
    |]

deleteProject :: ProjectId -> Transaction ()
deleteProject projectId = do
  execute
    [sql|
      DELETE FROM project_branch_remote_mapping
      WHERE local_project_id = :projectId
    |]
  execute
    [sql|
      DELETE FROM project_branch_parent
      WHERE project_id = :projectId
    |]
  execute
    [sql|
      DELETE FROM project_branch
      WHERE project_id = :projectId
    |]
  execute
    [sql|
      DELETE FROM project
      WHERE id = :projectId
    |]

-- | Delete a project branch.
--
-- Re-parenting happens in the obvious way:
--
--   Before:
--
--     main <- topic <- topic2
--
--  After deleting `topic`:
--
--    main <- topic2
deleteProjectBranch :: (HasCallStack) => ProjectId -> ProjectBranchId -> Transaction ()
deleteProjectBranch projectId branchId = do
  maybeParentBranchId :: Maybe ProjectBranchId <-
    queryMaybeCol
      [sql|
        SELECT parent_branch_id
        FROM project_branch_parent
        WHERE project_id = :projectId AND branch_id = :branchId
      |]
  -- If the branch being deleted has a parent, then reparent its children. Otherwise, the 'on delete cascade' foreign
  -- key from `project_branch_parent` will take care of deleting its children's parent entries.
  whenJust maybeParentBranchId \parentBranchId ->
    execute
      [sql|
        UPDATE project_branch_parent
        SET parent_branch_id = :parentBranchId
        WHERE project_id = :projectId AND parent_branch_id = :branchId
      |]
  execute
    [sql|
      DELETE FROM project_branch
      WHERE project_id = :projectId AND branch_id = :branchId
    |]

-- | Set project branch HEAD
setProjectBranchHead :: Text -> ProjectId -> ProjectBranchId -> CausalHashId -> Transaction ()
setProjectBranchHead description projectId branchId causalHashId = do
  -- Ensure we never point at a causal we don't have the branch for.
  _ <- expectBranchObjectIdByCausalHashId causalHashId
  oldRootCausalHashId <- expectProjectBranchHead projectId branchId
  execute
    [sql|
      UPDATE project_branch
      SET causal_hash_id = :causalHashId
      WHERE project_id = :projectId AND branch_id = :branchId
    |]
  time <- Sqlite.unsafeIO $ Time.getCurrentTime
  appendProjectBranchReflog $
    ProjectReflog.Entry
      { project = projectId,
        branch = branchId,
        time = time,
        fromRootCausalHash = Just oldRootCausalHashId,
        toRootCausalHash = causalHashId,
        reason = description
      }

loadProjectBranchHead :: ProjectId -> ProjectBranchId -> Transaction (Maybe CausalHashId)
loadProjectBranchHead projectId branchId =
  queryMaybeCol (loadProjectBranchHeadSql projectId branchId)

expectProjectBranchHead :: (HasCallStack) => ProjectId -> ProjectBranchId -> Transaction CausalHashId
expectProjectBranchHead projectId branchId =
  queryOneCol (loadProjectBranchHeadSql projectId branchId)

expectProjectBranchHeadHash :: (HasCallStack) => ProjectId -> ProjectBranchId -> Transaction CausalHash
expectProjectBranchHeadHash projectId branchId = do
  headHashId <- expectProjectBranchHead projectId branchId
  expectCausalHash headHashId

loadProjectBranchHeadSql :: ProjectId -> ProjectBranchId -> Sql
loadProjectBranchHeadSql projectId branchId =
  [sql|
    SELECT causal_hash_id
    FROM project_branch
    WHERE project_id = :projectId AND branch_id = :branchId
  |]

data LoadRemoteBranchFlag
  = IncludeSelfRemote
  | ExcludeSelfRemote
  deriving stock (Show, Eq)

-- | Determine the remote mapping for a local project/branch by
-- looking at the mapping for the given pair, then falling back to the
-- project of the nearest ancestor.
loadRemoteProjectBranch ::
  ProjectId ->
  URI ->
  ProjectBranchId ->
  Transaction (Maybe (RemoteProjectId, Maybe RemoteProjectBranchId))
loadRemoteProjectBranch p u b = do
  loadRemoteProjectBranchGen IncludeSelfRemote p u b <&> fmap fixup
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
  URI ->
  ProjectBranchId ->
  Transaction (Maybe (RemoteProjectId, RemoteProjectBranchId))
loadDefaultMergeTargetForLocalProjectBranch p u b = do
  loadRemoteProjectBranchGen ExcludeSelfRemote p u b <&> fmap fixup
  where
    fixup = \case
      (project, branch, _) -> (project, branch)

-- Parameterized query for finding the remote mapping for a branch and
-- the default merge target for a branch.
loadRemoteProjectBranchGen ::
  LoadRemoteBranchFlag ->
  ProjectId ->
  URI ->
  ProjectBranchId ->
  Transaction (Maybe (RemoteProjectId, RemoteProjectBranchId, Int64))
loadRemoteProjectBranchGen loadRemoteBranchFlag pid remoteUri bid =
  queryMaybeRow theSql
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
              AND pbrm.remote_host = :remoteUri
          WHERE
            pb.project_id = :pid
            AND pb.branch_id = :bid
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
          LEFT JOIN project_branch_parent AS pbp ON pbp.project_id = t.project_id
            AND pbp.branch_id = t.parent_branch_id
          LEFT JOIN project_branch_remote_mapping AS pbrm ON pbrm.local_project_id = t.project_id
          AND pbrm.local_branch_id = t.parent_branch_id
          AND pbrm.remote_host = :remoteUri
          WHERE t.parent_branch_id IS NOT NULL
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

    whereClause :: Sql
    whereClause =
      let clauses =
            foldr
              (\a b -> [sql| $a AND $b |])
              [sql| TRUE |]
              [ [sql| remote_project_id IS NOT NULL |],
                selfRemoteFilter
              ]
       in [sql| WHERE $clauses |]

    selfRemoteFilter = case loadRemoteBranchFlag of
      IncludeSelfRemote -> [sql| TRUE |]
      ExcludeSelfRemote -> [sql| depth > 0 |]

loadRemoteProject :: RemoteProjectId -> URI -> Transaction (Maybe RemoteProject)
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
        id = :rpid
        and host = :host
    |]

ensureRemoteProject :: RemoteProjectId -> URI -> ProjectName -> Transaction ()
ensureRemoteProject rpid host name =
  execute
    [sql|
      INSERT INTO remote_project (
        id,
        host,
        name)
      VALUES (
        :rpid,
        :host,
        :name)
      ON CONFLICT (
        id,
        host)
        -- should this update the name instead?
        DO NOTHING
    |]

expectRemoteProjectName :: RemoteProjectId -> URI -> Transaction ProjectName
expectRemoteProjectName projectId host =
  queryOneCol
    [sql|
      SELECT
        name
      FROM
        remote_project
      WHERE
        id = :projectId
        AND host = :host
    |]

setRemoteProjectName :: RemoteProjectId -> ProjectName -> Transaction ()
setRemoteProjectName rpid name =
  execute
    [sql|
      UPDATE
        remote_project
      SET
        name = :name
      WHERE
        id = :rpid
    |]

loadRemoteBranch :: RemoteProjectId -> URI -> RemoteProjectBranchId -> Transaction (Maybe RemoteProjectBranch)
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
        project_id = :rpid
        AND branch_id = :rbid
        AND host = :host
    |]

ensureRemoteProjectBranch :: RemoteProjectId -> URI -> RemoteProjectBranchId -> ProjectBranchName -> Transaction ()
ensureRemoteProjectBranch rpid host rbid name =
  execute
    [sql|
      INSERT INTO remote_project_branch (
        project_id,
        host,
        branch_id,
        name)
      VALUES (
        :rpid,
        :host,
        :rbid,
        :name)
      ON CONFLICT (
        project_id,
        branch_id,
        host)
        -- should this update the name instead?
        DO NOTHING
        |]

expectRemoteProjectBranchName :: URI -> RemoteProjectId -> RemoteProjectBranchId -> Transaction ProjectBranchName
expectRemoteProjectBranchName host projectId branchId =
  queryOneCol
    [sql|
      SELECT
        name
      FROM
        remote_project_branch
      WHERE
        host = :host
        AND project_id = :projectId
        AND branch_id = :branchId
    |]

setRemoteProjectBranchName :: RemoteProjectId -> URI -> RemoteProjectBranchId -> ProjectBranchName -> Transaction ()
setRemoteProjectBranchName rpid host rbid name =
  execute
    [sql|
      UPDATE
        remote_project_branch
      SET
        name = :name
      WHERE
        project_id = :rpid
        AND host = :host
        AND branch_id = :rbid
    |]

insertBranchRemoteMapping ::
  ProjectId ->
  ProjectBranchId ->
  RemoteProjectId ->
  URI ->
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
        :pid,
        :bid,
        :rpid,
        :rbid,
        :host)
        |]

ensureBranchRemoteMapping ::
  ProjectId ->
  ProjectBranchId ->
  RemoteProjectId ->
  URI ->
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
        :pid,
        :bid,
        :rpid,
        :rbid,
        :host)
      ON CONFLICT (
        local_project_id,
        local_branch_id,
        remote_host)
        DO NOTHING
    |]

deleteBranchRemoteMapping ::
  ProjectId ->
  ProjectBranchId ->
  URI ->
  Transaction ()
deleteBranchRemoteMapping pid bid host =
  execute
    [sql|
      DELETE FROM project_branch_remote_mapping
      WHERE local_project_id = :pid
        AND local_branch_id = :bid
        AND remote_host = :host
    |]

setMostRecentBranch :: ProjectId -> ProjectBranchId -> Transaction ()
setMostRecentBranch projectId branchId =
  execute
    [sql|
      INSERT INTO most_recent_branch (
        project_id,
        branch_id)
      VALUES (
        :projectId,
        :branchId)
      ON CONFLICT
        DO UPDATE SET
          project_id = excluded.project_id,
          branch_id = excluded.branch_id
  |]

loadMostRecentBranch :: ProjectId -> Transaction (Maybe ProjectBranchId)
loadMostRecentBranch projectId =
  queryMaybeCol
    [sql|
      SELECT
        branch_id
      FROM
        most_recent_branch
      WHERE
        project_id = :projectId
    |]

loadProjectBranchParent :: ProjectId -> ProjectBranchId -> Transaction (Maybe ProjectBranchId)
loadProjectBranchParent projectId projectBranchId =
  queryMaybeCol
    [sql|
      SELECT parent_branch_id
      FROM project_branch_parent
      WHERE project_id = :projectId
        AND branch_id = :projectBranchId
    |]

-- | Get whether or not a project branch is a "merge branch". Returns false if the branch either isn't a merge branch
-- (likely) or doesn't exist at all (weird).
projectBranchIsMergeBranch :: ProjectId -> ProjectBranchId -> Transaction Bool
projectBranchIsMergeBranch projectId branchId =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM merge_branch
        WHERE project_id = :projectId
          AND branch_id = :branchId
      )
    |]

loadMergeBranchParents ::
  ProjectId ->
  ProjectBranchId ->
  Transaction
    ( Maybe
        ( Maybe ProjectBranchId,
          CausalHashId,
          Maybe ProjectBranchId,
          CausalHashId
        )
    )
loadMergeBranchParents projectId branchId =
  queryMaybeRow
    [sql|
      SELECT local_source_branch_id, source_causal_hash_id, target_branch_id, target_causal_hash_id
      FROM merge_branch
      WHERE project_id = :projectId
        AND branch_id = :branchId
    |]

insertMergeBranchLocal ::
  ProjectId ->
  ProjectBranchId ->
  (ProjectBranchId, CausalHashId) ->
  (ProjectBranchId, CausalHashId) ->
  Transaction ()
insertMergeBranchLocal
  projectId
  mergeBranchId
  (sourceBranchId, sourceCausalHashId)
  (targetBranchId, targetCausalHashId) =
    execute
      [sql|
        INSERT INTO merge_branch (
          project_id,
          branch_id,
          local_source_project_id,
          local_source_branch_id,
          source_causal_hash_id,
          target_project_id,
          target_branch_id,
          target_causal_hash_id
        )
        VALUES (
          :projectId,
          :mergeBranchId,
          :projectId,
          :sourceBranchId,
          :sourceCausalHashId,
          :projectId,
          :targetBranchId,
          :targetCausalHashId
        )
      |]

insertMergeBranchRemote ::
  ProjectId ->
  ProjectBranchId ->
  (RemoteProjectId, RemoteProjectBranchId, URI, CausalHashId) ->
  (ProjectBranchId, CausalHashId) ->
  Transaction ()
insertMergeBranchRemote
  projectId
  mergeBranchId
  (sourceProjectId, sourceBranchId, sourceHost, sourceCausalHashId)
  (targetBranchId, targetCausalHashId) =
    execute
      [sql|
        INSERT INTO merge_branch (
          project_id,
          branch_id,
          remote_source_project_id,
          remote_source_branch_id,
          remote_source_host,
          source_causal_hash_id,
          target_project_id,
          target_branch_id,
          target_causal_hash_id
        )
        VALUES (
          :projectId,
          :mergeBranchId,
          :sourceProjectId,
          :sourceBranchId,
          :sourceHost,
          :sourceCausalHashId,
          :projectId,
          :targetBranchId,
          :targetCausalHashId
        )
      |]

insertMergeBranchLooseCode ::
  ProjectId ->
  ProjectBranchId ->
  CausalHashId ->
  (ProjectBranchId, CausalHashId) ->
  Transaction ()
insertMergeBranchLooseCode
  projectId
  mergeBranchId
  sourceCausalHashId
  (targetBranchId, targetCausalHashId) =
    execute
      [sql|
        INSERT INTO merge_branch (
          project_id,
          branch_id,
          source_causal_hash_id,
          target_project_id,
          target_branch_id,
          target_causal_hash_id
        )
        VALUES (
          :projectId,
          :mergeBranchId,
          :sourceCausalHashId,
          :projectId,
          :targetBranchId,
          :targetCausalHashId
        )
      |]

loadNamespaceUniqueTypeGuid :: BranchHashId -> Name -> Transaction (Maybe Text)
loadNamespaceUniqueTypeGuid namespaceHashId name = do
  queryMaybeCol
    [sql|
      SELECT type_guid
      FROM namespace_unique_type_guid
      WHERE namespace_hash_id = :namespaceHashId
        AND type_name = :segments
    |]
  where
    segments :: LazyByteString
    segments =
      name
        & Name.segments
        & List.NonEmpty.toList
        & map NameSegment.toUnescapedText
        & Aeson.encode @[Text]

existsAnyNamespaceUniqueTypeGuidForNamespace :: BranchHashId -> Transaction Bool
existsAnyNamespaceUniqueTypeGuidForNamespace namespaceHashId =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM namespace_unique_type_guid
        WHERE namespace_hash_id = :namespaceHashId
      )
    |]

ensureUniqueTypeToGuidMappingForCausalHashId :: CausalHashId -> Map Name Text -> Transaction ()
ensureUniqueTypeToGuidMappingForCausalHashId causalHashId uniqueTypeGuids =
  when (not (Map.null uniqueTypeGuids)) do
    namespaceHashId <- expectCausalValueHashId causalHashId
    existsAnyNamespaceUniqueTypeGuidForNamespace namespaceHashId >>= \case
      True -> pure ()
      False ->
        for_ (Map.toList uniqueTypeGuids) \(name, guid) ->
          insertNamespaceUniqueTypeGuid namespaceHashId name guid

insertNamespaceUniqueTypeGuid :: BranchHashId -> Name -> Text -> Transaction ()
insertNamespaceUniqueTypeGuid namespaceHashId typeName typeGuid =
  execute
    [sql|
      INSERT INTO namespace_unique_type_guid (namespace_hash_id, type_name, type_guid)
      VALUES (:namespaceHashId, :typeNameJson, :typeGuid)
    |]
  where
    typeNameJson :: LazyByteString
    typeNameJson =
      typeName
        & Name.segments
        & List.NonEmpty.toList
        & map NameSegment.toUnescapedText
        & Aeson.encode

-- | Get whether or not a project branch is an "update branch". Returns false if the branch either isn't an update
-- branch (likely) or doesn't exist at all (weird).
projectBranchIsUpdateBranch :: ProjectId -> ProjectBranchId -> Transaction Bool
projectBranchIsUpdateBranch projectId branchId =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM update_branch
        WHERE project_id = :projectId
          AND branch_id = :branchId
      )
    |]

-- | Load whether the given branch is an update branch, and if it is, return its parent's causal hash id.
loadUpdateBranchParentCausalHashId :: ProjectId -> ProjectBranchId -> Transaction (Maybe CausalHashId)
loadUpdateBranchParentCausalHashId projectId branchId =
  queryMaybeCol
    [sql|
      SELECT parent_causal_hash_id
      FROM update_branch
      WHERE project_id = :projectId
        AND branch_id = :branchId
    |]

-- | Record that a project branch is an "update branch".
setProjectBranchIsUpdateBranch :: ProjectId -> ProjectBranchId -> CausalHashId -> Transaction ()
setProjectBranchIsUpdateBranch projectId branchId parentCausalHashId =
  execute
    [sql|
      INSERT INTO update_branch (project_id, branch_id, parent_causal_hash_id)
      VALUES (:projectId, :branchId, :parentCausalHashId)
    |]

-- | Get whether or not a project branch is an "upgrade branch". Returns false if the branch either isn't an upgrade
-- branch (likely) or doesn't exist at all (weird).
projectBranchIsUpgradeBranch :: ProjectId -> ProjectBranchId -> Transaction Bool
projectBranchIsUpgradeBranch projectId branchId =
  queryOneCol
    [sql|
      SELECT EXISTS (
        SELECT 1
        FROM upgrade_branch
        WHERE project_id = :projectId
          AND branch_id = :branchId
      )
    |]

-- | Load whether the given branch is an upgrade branch, and if it is, return its parent's causal hash id.
loadUpgradeBranchParentCausalHashId :: ProjectId -> ProjectBranchId -> Transaction (Maybe CausalHashId)
loadUpgradeBranchParentCausalHashId projectId branchId =
  queryMaybeCol
    [sql|
      SELECT parent_causal_hash_id
      FROM upgrade_branch
      WHERE project_id = :projectId
        AND branch_id = :branchId
    |]

-- | Record that a project branch is an "upgrade branch".
setProjectBranchIsUpgradeBranch :: ProjectId -> ProjectBranchId -> CausalHashId -> Transaction ()
setProjectBranchIsUpgradeBranch projectId branchId parentCausalHashId =
  execute
    [sql|
      INSERT INTO upgrade_branch (project_id, branch_id, parent_causal_hash_id)
      VALUES (:projectId, :branchId, :parentCausalHashId)
    |]

data JsonParseFailure = JsonParseFailure
  { bytes :: !Text,
    failure :: !Text
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

-- | Get the most recent namespace the user has visited.
expectCurrentProjectPath :: (HasCallStack) => Transaction (ProjectId, ProjectBranchId, [NameSegment])
expectCurrentProjectPath =
  queryOneRowCheck
    [sql|
      SELECT project_id, branch_id, path
      FROM current_project_path
    |]
    check
  where
    check :: (ProjectId, ProjectBranchId, Text) -> Either JsonParseFailure (ProjectId, ProjectBranchId, [NameSegment])
    check (projId, branchId, pathText) =
      case Aeson.eitherDecodeStrict (Text.encodeUtf8 pathText) of
        Left failure -> Left JsonParseFailure {bytes = pathText, failure = Text.pack failure}
        Right namespace -> Right (projId, branchId, map NameSegment namespace)

updateProjectBranchLastAccessed :: ProjectId -> ProjectBranchId -> Transaction ()
updateProjectBranchLastAccessed projectId branchId = do
  sv <- schemaVersion
  -- The 'last_accessed' field doesn't exist before schema version 18.
  when (sv >= 18) $ do
    execute
      [sql|
        UPDATE project_branch
        SET last_accessed = strftime('%s', 'now', 'subsec')
        WHERE project_id = :projectId
          AND branch_id = :branchId
      |]

-- | Set the most recent namespace the user has visited.
setCurrentProjectPath ::
  ProjectId ->
  ProjectBranchId ->
  [NameSegment] ->
  Transaction ()
setCurrentProjectPath projId branchId path = do
  execute
    [sql| DELETE FROM current_project_path |]
  execute
    [sql|
      INSERT INTO current_project_path(project_id, branch_id, path)
      VALUES (:projId, :branchId, :jsonPath)
    |]
  updateProjectBranchLastAccessed projId branchId
  where
    jsonPath :: Text
    jsonPath =
      Text.Lazy.toStrict (Aeson.encodeToLazyText $ NameSegment.toUnescapedText <$> path)

-- | Get the causal hash result from squashing the provided branch hash if we've squashed it
-- at some point in the past.
tryGetSquashResult :: BranchHashId -> Transaction (Maybe CausalHashId)
tryGetSquashResult bhId = do
  queryMaybeCol
    [sql|
      SELECT
        squashed_causal_hash_id
      FROM
        squash_results
      WHERE
        branch_hash_id = :bhId
    |]

-- | Save the result of running a squash on the provided branch hash id.
saveSquashResult :: BranchHashId -> CausalHashId -> Transaction ()
saveSquashResult bhId chId =
  execute
    [sql|
      INSERT INTO squash_results (
        branch_hash_id,
        squashed_causal_hash_id)
      VALUES (
        :bhId,
        :chId
        )
      ON CONFLICT DO NOTHING
    |]
