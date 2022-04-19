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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Queries
  ( -- * Constraint kinds
    DB,
    EDB,
    Err,

    -- * Error types
    Integrity (..),

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
    isObjectHash,
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

    -- * sync temp entities
    getMissingDependentsForTempEntity,
    getMissingDependencyJwtsForTempEntity,
    tempEntityExists,
    insertTempEntity,
    deleteTempDependencies,

    -- * db misc
    createSchema,
    addTempEntityTables,
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
  )
where

import qualified Control.Exception as Exception
import qualified Control.Lens as Lens
import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader (MonadReader (ask))
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import Data.Bitraversable (bitraverse)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.String.Here.Uninterpolated (here, hereFile)
import Database.SQLite.Simple
  ( FromRow,
    Only (..),
    ToRow (..),
    (:.) (..),
  )
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference (Reference' (..))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.Branch.Format as NamespaceFormat
import qualified U.Codebase.Sqlite.Causal as Causal
import qualified U.Codebase.Sqlite.Causal as Sqlite.Causal
import U.Codebase.Sqlite.Connection (Connection)
import qualified U.Codebase.Sqlite.Connection as Connection
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
import U.Codebase.Sqlite.JournalMode (JournalMode)
import qualified U.Codebase.Sqlite.JournalMode as JournalMode
import U.Codebase.Sqlite.ObjectType (ObjectType)
import qualified U.Codebase.Sqlite.ObjectType as ObjectType
import qualified U.Codebase.Sqlite.Patch.Format as PatchFormat
import U.Codebase.Sqlite.ReadyEntity (ReadyEntity)
import qualified U.Codebase.Sqlite.ReadyEntity as ReadyEntity
import qualified U.Codebase.Sqlite.Reference as Reference
import qualified U.Codebase.Sqlite.Referent as Referent
import U.Codebase.Sqlite.Serialization as Serialization
import U.Codebase.Sqlite.TempEntity (HashJWT, TempEntity)
import qualified U.Codebase.Sqlite.TempEntity as TempEntity
import U.Codebase.Sqlite.TempEntityType (TempEntityType)
import qualified U.Codebase.Sqlite.TempEntityType as TempEntityType
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Codebase.WatchKind (WatchKind)
import qualified U.Codebase.WatchKind as WatchKind
import qualified U.Util.Alternative as Alternative
import U.Util.Base32Hex (Base32Hex (..))
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import Unison.Prelude
import UnliftIO (throwIO, tryAny)
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
  deriving anyclass (Exception)

orError :: Err m => Integrity -> Maybe b -> m b
orError e = maybe (throwError e) pure

-- * main squeeze

createSchema :: (DB m, MonadUnliftIO m) => m ()
createSchema = do
  withImmediateTransaction do
    executeFile [hereFile|sql/create.sql|]
    addTempEntityTables

addTempEntityTables :: DB m => m ()
addTempEntityTables =
  executeFile [hereFile|sql/001-temp-entity-tables.sql|]

executeFile :: DB m => String -> m ()
executeFile =
  traverse_ (execute_ . fromString) . filter (not . null) . List.splitOn ";"

setJournalMode :: DB m => JournalMode -> m ()
setJournalMode m =
  let s = Char.toLower <$> show m
   in map (fromOnly @String)
        <$> query_ (fromString $ "PRAGMA journal_mode = " ++ s) >>= \case
          [y] | y == s -> pure ()
          y ->
            liftIO . putStrLn $
              "I couldn't set the codebase journal mode to " ++ s
                ++ "; it's set to "
                ++ show y
                ++ "."

setFlags :: DB m => m ()
setFlags = do
  execute_ "PRAGMA foreign_keys = ON;"
  setJournalMode JournalMode.WAL

-- | Copy the database into the specified location, performing a VACUUM in the process.
vacuumInto :: DB m => FilePath -> m ()
vacuumInto dest = do
  execute "VACUUM INTO ?" [dest]

schemaVersion :: DB m => m SchemaVersion
schemaVersion =
  queryAtoms_ sql >>= \case
    [] -> error $ show NoSchemaVersion
    [v] -> pure v
    vs -> error $ show (MultipleSchemaVersions vs)
  where
    sql = "SELECT version from schema_version;"

setSchemaVersion :: DB m => SchemaVersion -> m ()
setSchemaVersion schemaVersion = execute sql (Only schemaVersion)
  where
    sql = "UPDATE schema_version SET version = ?"

{- ORMOLU_DISABLE -}
{- Please don't try to format the SQL blocks —AI -}
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

loadCausalByCausalHash :: DB m => CausalHash -> m (Maybe (CausalHashId, BranchHashId))
loadCausalByCausalHash ch = runMaybeT do
  hId <- MaybeT $ loadHashIdByHash (unCausalHash ch)
  bhId <- MaybeT $ loadMaybeCausalValueHashId hId
  pure (CausalHashId hId, bhId)

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
  changes >>= \case
    0 -> pure ()
    _ -> do
      _ <- Except.runExceptT do
        hash <- loadHashById h
        tryMoveTempEntityDependents hash
      pure ()
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

-- | Does a hash correspond to an object?
isObjectHash :: DB m => HashId -> m Bool
isObjectHash h = queryOne $ queryAtom sql (Only h) where sql = [here|
  SELECT EXISTS (SELECT 1 FROM object WHERE primary_hash_id = ?)
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
saveCausal :: EDB m => CausalHashId -> BranchHashId -> [CausalHashId] -> m ()
saveCausal self value parents = do
  execute insertCausalSql (self, value)
  changes >>= \case
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

flushCausalDependents :: EDB m => CausalHashId -> m ()
flushCausalDependents chId = loadHashById (unCausalHashId chId) >>= tryMoveTempEntityDependents

expectObjectIdForHashJWT :: DB m => TempEntity.HashJWT -> m ObjectId
expectObjectIdForHashJWT hashJwt = do
  hashId <- throwExceptT (expectHashIdByHash (decode hashJwt))
  throwExceptT (expectObjectIdForAnyHashId hashId)
  where
    decode :: TempEntity.HashJWT -> Hash
    decode =
      undefined
      -- FIXME need to know how to go HashJWT -> Hash at the DB layer too, not just Share API layer
      -- Hash.fromBase32Hex . Share.toBase32Hex . Share.hashJWTHash . Share.HashJWT

expectBranchObjectIdForHashJWT :: DB m => TempEntity.HashJWT -> m BranchObjectId
expectBranchObjectIdForHashJWT =
  fmap BranchObjectId . expectObjectIdForHashJWT

expectPatchObjectIdForHashJWT :: DB m => TempEntity.HashJWT -> m PatchObjectId
expectPatchObjectIdForHashJWT =
  fmap PatchObjectId . expectObjectIdForHashJWT

expectBranchHashIdForHashJWT :: DB m => TempEntity.HashJWT -> m BranchHashId
expectBranchHashIdForHashJWT = undefined

expectCausalHashIdForHashJWT :: DB m => TempEntity.HashJWT -> m CausalHashId
expectCausalHashIdForHashJWT = undefined

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
tryMoveTempEntityDependents :: EDB m => Base32Hex -> m ()
tryMoveTempEntityDependents dependencyBase32 = do
  dependents <- getMissingDependentsForTempEntity dependencyBase32
  executeMany deleteTempDependents (dependents <&> (,dependencyBase32))
  deleteTempEntity dependencyBase32
  traverse_ moveTempEntityToMain =<< tempEntitiesWithNoMissingDependencies
  where
    deleteTempDependents :: SQLite.Query
    deleteTempDependents = [here|
      DELETE FROM temp_entity_missing_dependency
      WHERE dependent = ?
        AND dependency = ?
    |]

    tempEntitiesWithNoMissingDependencies :: DB m => m [Base32Hex]
    tempEntitiesWithNoMissingDependencies = fmap (map fromOnly) $ query_ [here|
      SELECT hash
      FROM temp_entity
      WHERE NOT EXISTS(
        SELECT 1
        FROM temp_entity_missing_dependency dep
        WHERE dep.dependent = temp_entity.hash
      )
    |]

moveTempEntityToMain :: EDB m => Base32Hex -> m ()
moveTempEntityToMain b32 = do
  loadTempEntity b32 >>= \case
    Left _ -> undefined
    Right t -> do
      r <- tempToSyncEntity t
      _ <- saveReadyEntity b32 r
      pure ()

loadTempEntity :: DB m => Base32Hex -> m (Either String TempEntity)
loadTempEntity b32 = do
  (blob, typeId) <- queryOne $ queryMaybe sql (Only b32)
  pure $ case typeId of
    TempEntityType.TermComponentType ->
      TempEntity.TC <$> runGetS Serialization.getTempTermFormat blob
    TempEntityType.DeclComponentType ->
      TempEntity.DC <$> runGetS Serialization.getTempDeclFormat blob
    TempEntityType.NamespaceType ->
      TempEntity.N <$> runGetS Serialization.getTempNamespaceFormat  blob
    TempEntityType.PatchType ->
      TempEntity.P <$> runGetS Serialization.getTempPatchFormat blob
    TempEntityType.CausalType ->
      TempEntity.C <$> runGetS Serialization.getTempCausalFormat blob

  where sql = [here|
    SELECT (blob, type_id)
    FROM temp_entity
    WHERE hash = ?
  |]

tempToSyncEntity :: DB m => TempEntity -> m ReadyEntity
tempToSyncEntity = \case
  TempEntity.TC term -> ReadyEntity.TC <$> tempToSyncTermComponent term
  TempEntity.DC decl -> ReadyEntity.DC <$> tempToSyncDeclComponent decl
  TempEntity.N namespace -> ReadyEntity.N <$> tempToSyncNamespace namespace
  TempEntity.P patch -> ReadyEntity.P <$> tempToSyncPatch patch
  TempEntity.C causal -> ReadyEntity.C <$> tempToSyncCausal causal
  where
    tempToSyncCausal :: DB m => TempEntity.TempCausalFormat -> m Causal.SyncCausalFormat
    tempToSyncCausal Causal.SyncCausalFormat {valueHash, parents} =
      Causal.SyncCausalFormat
        <$> expectBranchHashIdForHashJWT valueHash
        <*> traverse expectCausalHashIdForHashJWT parents

    tempToSyncDeclComponent :: DB m => TempEntity.TempDeclFormat -> m DeclFormat.SyncDeclFormat
    tempToSyncDeclComponent = \case
      DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent decls) ->
        DeclFormat.SyncDecl . DeclFormat.SyncLocallyIndexedComponent
          <$> Lens.traverseOf (traverse . Lens._1) (bitraverse saveText expectObjectIdForHashJWT) decls

    tempToSyncNamespace :: DB m => TempEntity.TempNamespaceFormat -> m NamespaceFormat.SyncBranchFormat
    tempToSyncNamespace = \case
      NamespaceFormat.SyncFull localIds bytes ->
        NamespaceFormat.SyncFull <$> tempToSyncNamespaceLocalIds localIds <*> pure bytes
      NamespaceFormat.SyncDiff parent localIds bytes ->
        NamespaceFormat.SyncDiff
          <$> expectBranchObjectIdForHashJWT parent
          <*> tempToSyncNamespaceLocalIds localIds
          <*> pure bytes

    tempToSyncNamespaceLocalIds :: DB m => TempEntity.TempNamespaceLocalIds -> m NamespaceFormat.BranchLocalIds
    tempToSyncNamespaceLocalIds (NamespaceFormat.LocalIds texts defns patches children) =
      NamespaceFormat.LocalIds
        <$> traverse saveText texts
        <*> traverse expectObjectIdForHashJWT defns
        <*> traverse expectPatchObjectIdForHashJWT patches
        <*> traverse
          ( \(branch, causal) ->
              (,)
                <$> expectBranchObjectIdForHashJWT branch
                <*> expectCausalHashIdForHashJWT causal
          )
          children

    tempToSyncPatch :: DB m => TempEntity.TempPatchFormat -> m PatchFormat.SyncPatchFormat
    tempToSyncPatch = \case
      PatchFormat.SyncFull localIds bytes -> PatchFormat.SyncFull <$> tempToSyncPatchLocalIds localIds <*> pure bytes
      PatchFormat.SyncDiff parent localIds bytes ->
        PatchFormat.SyncDiff
          <$> expectPatchObjectIdForHashJWT parent
          <*> tempToSyncPatchLocalIds localIds
          <*> pure bytes

    tempToSyncPatchLocalIds :: DB m => TempEntity.TempPatchLocalIds -> m PatchFormat.PatchLocalIds
    tempToSyncPatchLocalIds (PatchFormat.LocalIds texts hashes defns) =
      PatchFormat.LocalIds
        <$> traverse saveText texts
        <*> traverse saveHash hashes
        <*> traverse expectObjectIdForHashJWT defns

    tempToSyncTermComponent :: DB m => TempEntity.TempTermFormat -> m TermFormat.SyncTermFormat
    tempToSyncTermComponent = \case
      TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent terms) ->
        TermFormat.SyncTerm . TermFormat.SyncLocallyIndexedComponent
          <$> Lens.traverseOf (traverse . Lens._1) (bitraverse saveText expectObjectIdForHashJWT) terms

saveReadyEntity :: EDB m => Base32Hex -> ReadyEntity -> m (Either CausalHashId ObjectId)
saveReadyEntity b32Hex entity = do
  hashId <- saveHash b32Hex
  case entity of
    ReadyEntity.TC stf -> do
      let bytes = runPutS (Serialization.recomposeTermFormat stf)
      Right <$> saveObject hashId ObjectType.TermComponent bytes
    ReadyEntity.DC sdf -> do
      let bytes = runPutS (Serialization.recomposeDeclFormat sdf)
      Right <$> saveObject hashId ObjectType.DeclComponent bytes
    ReadyEntity.N sbf -> do
      let bytes = runPutS (Serialization.recomposeBranchFormat sbf)
      Right <$> saveObject hashId ObjectType.Namespace bytes
    ReadyEntity.P spf -> do
      let bytes = runPutS (Serialization.recomposePatchFormat spf)
      Right <$> saveObject hashId ObjectType.Patch bytes
    ReadyEntity.C scf -> case scf of
      Sqlite.Causal.SyncCausalFormat{valueHash, parents} -> do
        let causalHashId = CausalHashId hashId
        saveCausal causalHashId valueHash (Foldable.toList parents)
        pure $ Left causalHashId

changes :: DB m => m Int
changes = do
  conn <- Reader.reader Connection.underlying
  liftIO (SQLite.changes conn)

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

before :: DB m => CausalHashId -> CausalHashId -> m Bool
before chId1 chId2 = fmap fromOnly . queryOne $ queryMaybe sql (chId2, chId1)
  where
    sql = fromString $ "SELECT EXISTS (" ++ ancestorSql ++ " WHERE ancestor.id = ?)"

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
             in if Set.member px seenY'
                  then pure (Just px)
                  else
                    if Set.member py seenX'
                      then pure (Just py)
                      else loop2 seenX' seenY'
          (Nothing, Nothing) -> pure Nothing
          (Just (Only px), Nothing) -> loop1 (SQLite.nextRow sx) seenY px
          (Nothing, Just (Only py)) -> loop1 (SQLite.nextRow sy) seenX py
      loop1 getNext matches v =
        if Set.member v matches
          then pure (Just v)
          else
            getNext >>= \case
              Just (Only v) -> loop1 getNext matches v
              Nothing -> pure Nothing
  loop2 (Set.singleton x) (Set.singleton y)
  where
    open =
      (,)
        <$> SQLite.openStatement cx sql <*> SQLite.openStatement cy sql
    close (cx, cy) = SQLite.closeStatement cx *> SQLite.closeStatement cy
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

--
getMissingDependencyJwtsForTempEntity :: DB m => Base32Hex -> m (Maybe (NESet Text))
getMissingDependencyJwtsForTempEntity h =
  NESet.nonEmptySet . Set.fromList
    <$> queryAtoms
      [here|
        SELECT dependencyJwt FROM temp_entity_missing_dependency
        WHERE dependent = ?
      |]
      (Only h)

getMissingDependentsForTempEntity :: DB m => Base32Hex -> m [Base32Hex]
getMissingDependentsForTempEntity h =
  queryAtoms
    [here|
      SELECT dependent
      FROM temp_entity_missing_dependency
      WHERE dependency = ?
    |]
    (Only h)

tempEntityExists :: DB m => Base32Hex -> m Bool
tempEntityExists h = queryOne $ queryAtom sql (Only h)
  where
    sql =
      [here|
        SELECT EXISTS (
          SELECT 1
          FROM temp_entity
          WHERE hash = ?
        )
      |]

-- | Insert a new `temp_entity` row, and its associated 1+ `temp_entity_missing_dependency` rows.
--
-- Preconditions:
--   1. The entity does not already exist in "main" storage (`object` / `causal`)
--   2. The entity does not already exist in `temp_entity`.
insertTempEntity :: DB m => Base32Hex -> TempEntity -> NESet (Base32Hex, HashJWT) -> m ()
insertTempEntity entityHash entity missingDependencies = do
  execute
    [here|
      INSERT INTO temp_entity (hash, blob, typeId)
      VALUES (?, ?, ?)
    |]
    (entityHash, entityBlob, entityType)

  executeMany
    [here|
      INSERT INTO temp_entity_missing_dependencies (dependent, dependency, dependencyJwt)
      VALUES (?, ?, ?)
    |]
    (map (\(depHash, depHashJwt) -> (entityHash, depHash, depHashJwt)) (Foldable.toList missingDependencies))
  where
    entityBlob :: ByteString
    entityBlob =
      runPutS (Serialization.putTempEntity entity)

    entityType :: TempEntityType
    entityType =
      TempEntity.tempEntityType entity

-- | Delete a row from the `temp_entity` table, if it exists.
deleteTempEntity :: DB m => Base32Hex -> m ()
deleteTempEntity hash =
  execute
    [here|
      DELETE
      FROM temp_entity
      WHERE hash = ?
    |]
    (Only hash)

-- | takes a dependent's hash and multiple dependency hashes
deleteTempDependencies :: (DB m, Foldable f) => Base32Hex -> f Base32Hex -> m ()
deleteTempDependencies dependent (Foldable.toList -> dependencies) =
  executeMany sql (map (dependent,) dependencies)
  where
    sql =
      [here|
        DELETE FROM temp_entity_missing_dependencies
        WHERE dependent = ?
          AND dependency = ?
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
    then do
      try @_ @SQLite.SQLError m >>= \case
        Right a -> do
          when debugQuery . traceM $
            showInput
              ++ if " execute" `List.isSuffixOf` title then mempty else "\n output: " ++ show a
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
          when debugQuery . traceM $
            title ++ " " ++ show query
              ++ if " execute_" `List.isSuffixOf` title then mempty else "\n output: " ++ show a
          pure a
        Left e -> do
          traceM $ title ++ " " ++ show query ++ "\n(and crashed)\n"
          throwIO e
    else m

-- | print the active database filename
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

executeMany :: (DB m, Foldable f, ToRow q, Show q) => SQLite.Query -> f q -> m ()
executeMany q (Foldable.toList -> r) = do
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
  fromField =
    fromField @Int8 <&> fmap \case
      0 -> WatchKind.RegularWatch
      1 -> WatchKind.TestWatch
      tag -> error $ "Unknown WatchKind id " ++ show tag
