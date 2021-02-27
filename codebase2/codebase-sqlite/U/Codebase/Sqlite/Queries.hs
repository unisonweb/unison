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

module U.Codebase.Sqlite.Queries where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import Data.String.Here.Uninterpolated (here, hereFile)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, Only (..), SQLData, ToRow (..), (:.) (..))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField (..))
import Debug.Trace (trace, traceM)
import U.Codebase.HashTags (BranchHash, CausalHash, unBranchHash, unCausalHash)
import U.Codebase.Reference (Reference')
import qualified U.Codebase.Referent as C.Referent
import U.Codebase.Sqlite.DbId (BranchHashId (..), BranchObjectId (..), CausalHashId (..), CausalOldHashId, Generation (..), HashId (..), ObjectId (..), TextId)
import U.Codebase.Sqlite.ObjectType (ObjectType)
import qualified U.Codebase.Sqlite.Reference as Reference
import qualified U.Codebase.Sqlite.Referent as Referent
import U.Codebase.WatchKind (WatchKind)
import qualified U.Codebase.WatchKind as WatchKind
import U.Util.Base32Hex (Base32Hex (..))
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import UnliftIO (MonadUnliftIO, throwIO, try, withRunInIO)

-- * types

type DB m = (MonadIO m, MonadReader Connection m)

type EDB m = (DB m, MonadError Integrity m)

data Integrity
  = UnknownHashId HashId
  | UnknownTextId TextId
  | UnknownObjectId ObjectId
  | UnknownCausalHashId CausalHashId
  | UnknownCausalOldHashId CausalOldHashId
  | UnknownHash Hash
  | UnknownText Text
  | NoObjectForHashId HashId
  | NoNamespaceRoot
  | MultipleNamespaceRoots [CausalHashId]
  | NoTypeIndexForTerm Referent.Id
  deriving (Show)

-- | discard errors that you're sure are impossible
noExcept :: (Monad m, Show e) => ExceptT e m a -> m a
noExcept a =
  runExceptT a >>= \case
    Right a -> pure a
    Left e -> error $ "unexpected error: " ++ show e

orError :: MonadError e m => e -> Maybe b -> m b
orError e = maybe (throwError e) pure

type TypeHashReference = Reference' TextId HashId

-- * main squeeze

createSchema :: (DB m, MonadUnliftIO m) => m ()
createSchema = do
  withImmediateTransaction . traverse_ (execute_ . fromString) $
    List.splitOn ";" [hereFile|sql/create.sql|]
      <> List.splitOn ";" [hereFile|sql/create-index.sql|]

setFlags :: DB m => m ()
setFlags = execute_ "PRAGMA foreign_keys = ON;"

type SchemaType = String

type SchemaName = String

checkForMissingSchema :: DB m => m [(SchemaType, SchemaName)]
checkForMissingSchema = filterM missing schema
  where
    missing (t, n) = null @[] @(Only Int) <$> query sql (t, n)
    sql = "SELECT 1 FROM sqlite_master WHERE type = ? and name = ?"
    schema =
      [ ("table", "hash"),
        ("index", "hash_base32"),
        ("table", "text"),
        ("table", "hash_object"),
        ("index", "hash_object_hash_id"),
        ("index", "hash_object_object_id"),
        ("table", "object_type_description"),
        ("table", "object"),
        ("index", "object_hash_id"),
        ("index", "object_type_id"),
        ("table", "causal"),
        ("index", "causal_value_hash_id"),
        ("index", "causal_gc_generation"),
        ("table", "namespace_root"),
        ("table", "causal_parent"),
        ("index", "causal_parent_causal_id"),
        ("index", "causal_parent_parent_id"),
        -- ,("table", "causal_old")
        ("table", "watch_result"),
        ("table", "watch"),
        ("index", "watch_kind"),
        ("table", "watch_kind_description")
      ]

{- ORMOLU_DISABLE -}
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
loadCausalHashIdByCausalHash =
  (fmap . fmap) CausalHashId . loadHashIdByHash . unCausalHash

expectHashIdByHash :: EDB m => Hash -> m HashId
expectHashIdByHash h = loadHashIdByHash h >>= orError (UnknownHash h)

loadHashById :: EDB m => HashId -> m Base32Hex
loadHashById h = queryAtom sql (Only h) >>= orError (UnknownHashId h)
  where sql = [here| SELECT base32 FROM hash WHERE id = ? |]

saveText :: DB m => Text -> m TextId
saveText t = execute sql (Only t) >> queryOne (loadText t)
  where sql = [here| INSERT INTO text (text) VALUES (?) ON CONFLICT DO NOTHING|]

loadText :: DB m => Text -> m (Maybe TextId)
loadText t = queryAtom sql (Only t)
  where sql = [here| SELECT id FROM text WHERE text = ? |]

expectText :: EDB m => Text -> m TextId
expectText t = loadText t >>= orError (UnknownText t)

loadTextById :: EDB m => TextId -> m Text
loadTextById h = queryAtom sql (Only h) >>= orError (UnknownTextId h)
  where sql = [here| SELECT text FROM text WHERE id = ? |]

saveHashObject :: DB m => HashId -> ObjectId -> Int -> m ()
saveHashObject hId oId version = execute sql (hId, oId, version) where
  sql = [here|
    INSERT INTO hash_object (hash_id, object_id, hash_version)
    VALUES (?, ?, ?)
    ON CONFLICT DO NOTHING
  |]

saveObject :: DB m => HashId -> ObjectType -> ByteString -> m ObjectId
saveObject h t blob = do
  oId <- execute sql (h, t, blob) >> queryOne (maybeObjectIdForPrimaryHashId h)
  saveHashObject h oId 1 -- todo: remove this from here, and add it to other relevant places once there are v1 and v2 hashes
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
  maybeObjectIdForPrimaryHashId h >>= orError (UnknownHashId h)

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

objectAndPrimaryHashByAnyHash :: EDB m => Base32Hex -> m (Maybe (Base32Hex, ObjectId))
objectAndPrimaryHashByAnyHash h = runMaybeT do
  hashId <- MaybeT $ loadHashId h -- hash may not exist
  oId <- MaybeT $ maybeObjectIdForAnyHashId hashId -- hash may not correspond to object
  base32 <- loadPrimaryHashByObjectId oId
  pure (base32, oId)

objectExistsWithHash :: DB m => Base32Hex -> m Bool
objectExistsWithHash h = queryExists sql (Only h) where
  sql = [here|
    SELECT 1
    FROM hash INNER JOIN hash_object ON hash.id = hash_object.hash_id
    WHERE base32 = ?
  |]

hashIdsForObject :: DB m => ObjectId -> m (NonEmpty HashId)
hashIdsForObject oId = do
  primaryHashId <- queryOne $ queryAtom sql1 (Only oId)
  hashIds <- queryAtoms sql2 (Only oId)
  pure $ primaryHashId Nel.:| filter (/= primaryHashId) hashIds
  where
    sql1 = "SELECT primary_hash_id FROM object WHERE id = ?"
    sql2 = "SELECT hash_id FROM hash_object WHERE object_id = ?"

hashIdWithVersionForObject :: DB m => ObjectId -> m [(HashId, Int)]
hashIdWithVersionForObject = query sql . Only where sql = [here|
  SELECT hash_id, hash_version FROM hash_object WHERE object_id = ?
|]

updateObjectBlob :: DB m => ObjectId -> ByteString -> m ()
updateObjectBlob oId bs = execute sql (oId, bs) where sql = [here|
  UPDATE object SET bytes = ? WHERE id = ?
|]

-- |Maybe we would generalize this to something other than NamespaceHash if we
-- end up wanting to store other kinds of Causals here too.
saveCausal :: DB m => CausalHashId -> BranchHashId -> m ()
saveCausal self value = execute sql (self, value, Generation 0) where sql = [here|
  INSERT INTO causal (self_hash_id, value_hash_id, gc_generation)
  VALUES (?, ?, ?)
  ON CONFLICT DO NOTHING
|]

-- maybe: look at whether parent causal is "committed"; if so, then increment;
-- otherwise, don't.
getNurseryGeneration :: DB m => m Generation
getNurseryGeneration = query_ sql <&> \case
  [] -> Generation 0
  [fromOnly -> g] -> Generation g
  (fmap fromOnly -> gs) ->
    error $ "How did I get multiple values out of a MAX()? " ++ show gs
  where sql = [here|
    SELECT MAX(gc_generation) FROM causal;
  |]

loadCausalValueHashId :: EDB m => CausalHashId -> m BranchHashId
loadCausalValueHashId chId@(CausalHashId id) =
  loadMaybeCausalValueHashId (id) >>= orError (UnknownCausalHashId chId)

loadMaybeCausalValueHashId :: DB m => HashId -> m (Maybe BranchHashId)
loadMaybeCausalValueHashId id =
  queryAtom sql (Only id) where sql = [here|
  SELECT value_hash_id FROM causal WHERE self_hash_id = ?
|]

isCausalHash :: DB m => HashId -> m Bool
isCausalHash = fmap isJust . loadMaybeCausalValueHashId

-- todo: do a join here
loadBranchObjectIdByCausalHashId :: EDB m => CausalHashId -> m (Maybe BranchObjectId)
loadBranchObjectIdByCausalHashId id = queryAtom sql (Only id) where sql = [here|
  SELECT object_id FROM hash_object
  INNER JOIN causal ON hash_id = causal.value_hash_id
  WHERE causal.self_hash_id = ?
|]

saveCausalOld :: DB m => HashId -> CausalHashId -> m ()
saveCausalOld v1 v2 = execute sql (v1, v2) where sql = [here|
  INSERT INTO causal_old (old_hash_id, new_hash_id) VALUES (?, ?)
  ON CONFLICT DO NOTHING
|]

loadCausalHashIdByCausalOldHash :: EDB m => CausalOldHashId -> m CausalHashId
loadCausalHashIdByCausalOldHash id =
  queryAtom sql (Only id) >>= orError (UnknownCausalOldHashId id) where sql = [here|
  SELECT new_hash_id FROM causal_old where old_hash_id = ?
|]

loadOldCausalValueHash :: EDB m => CausalOldHashId -> m BranchHashId
loadOldCausalValueHash id =
 queryAtom sql (Only id) >>= orError (UnknownCausalOldHashId id) where sql = [here|
  SELECT value_hash_id FROM causal
  INNER JOIN causal_old ON self_hash_id = new_hash_id
  WHERE old_hash_id = ?
|]

saveCausalParent :: DB m => CausalHashId -> CausalHashId -> m ()
saveCausalParent child parent = saveCausalParents child [parent]

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

loadNamespaceRoot :: EDB m => m CausalHashId
loadNamespaceRoot = queryAtoms sql () >>= \case
  [] -> throwError NoNamespaceRoot
  [id] -> pure id
  ids -> throwError (MultipleNamespaceRoots ids)
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

loadWatchesByWatchKind :: DB m => WatchKind -> m [Reference.Id]
loadWatchesByWatchKind k = query sql (Only k) where sql = [here|
  SELECT object_id, component_index FROM watch WHERE watch_kind_id = ?
|]

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

getTypeReferenceForReference :: EDB m => Reference.Id -> m (Reference' TextId HashId)
getTypeReferenceForReference (C.Referent.RefId -> r) =
  queryMaybe sql r >>= orError (NoTypeIndexForTerm r)
  where sql = [here|
  SELECT
    type_reference_builtin,
    type_reference_hash_id,
    type_reference_component_index
  FROM find_type_index
  WHERE term_referent_object_id = ?
    AND term_referent_component_index = ?
    AND term_referent_constructor_index = ?
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

getDependentsForDependency :: DB m => Reference.Reference -> m [Reference.Id]
getDependentsForDependency dependency = query sql dependency where sql = [here|
  SELECT dependent_object_id, dependent_component_index
  FROM dependents_index
  WHERE dependency_builtin IS ?
    AND dependency_object_id IS ?
    AND dependency_component_index IS ?
|]

getDependencyIdsForDependent :: DB m => Reference.Id -> m [Reference.Id]
getDependencyIdsForDependent dependent = query sql dependent where sql = [here|
  SELECT dependency_object_id, dependency_component_index
  FROM dependents_index
  WHERE dependency_builtin IS NULL
    AND dependent_object_id = ?
    AND dependen_component_index = ?
|]

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
{- ORMOLU_ENABLE -}

-- * helper functions

-- | composite input, atomic List output
queryAtoms :: (DB f, ToRow q, FromField b, Show q, Show b) => SQLite.Query -> q -> f [b]
queryAtoms q r = map fromOnly <$> query q r

-- | composite input, composite Maybe output
queryMaybe :: (DB f, ToRow q, FromRow b, Show q, Show b) => SQLite.Query -> q -> f (Maybe b)
queryMaybe q r = headMay <$> query q r

-- | composite input, atomic Maybe output
queryAtom :: (DB f, ToRow q, FromField b, Show q, Show b) => SQLite.Query -> q -> f (Maybe b)
queryAtom q r = fmap fromOnly <$> queryMaybe q r

-- | Just output
queryOne :: Functor f => f (Maybe b) -> f b
queryOne = fmap fromJust

-- | composite input, Boolean output
queryExists :: (DB m, ToRow q, Show q) => SQLite.Query -> q -> m Bool
queryExists q r = not . null . map (id @SQLData) <$> queryAtoms q r

debugQuery :: Bool
debugQuery = False

-- | composite input, composite List output
query :: (DB m, ToRow q, FromRow r, Show q, Show r) => SQLite.Query -> q -> m [r]
query q r = do
  c <- ask
  liftIO . queryTrace "query" q r $ SQLite.query c q r

-- | no input, composite List output
query_ :: (DB m, FromRow r, Show r) => SQLite.Query -> m [r]
query_ q = do
  c <- ask
  liftIO . queryTrace_ "query" q $ SQLite.query_ c q

queryTrace :: (MonadUnliftIO m, Show q, Show a) => String -> SQLite.Query -> q -> m a -> m a
queryTrace title query input m =
  if debugQuery
    then
      try @_ @SQLite.SQLError m >>= \case
        Right a -> do
          traceM $ title ++ " " ++ show query ++ "\n  input: " ++ show input ++ "\n output: " ++ show a
          pure a
        Left e -> do
          traceM $ title ++ " " ++ show query ++ "\n  input: " ++ show input ++ "\n(and crashed)\n"
          throwIO e
    else m

queryTrace_ :: (MonadUnliftIO m, Show a) => String -> SQLite.Query -> m a -> m a
queryTrace_ title query m =
  if debugQuery
    then
      try @_ @SQLite.SQLError m >>= \case
        Right a -> do
          traceM $ title ++ " " ++ show query ++ "\n output: " ++ show a
          pure a
        Left e -> do
          traceM $ title ++ " " ++ show query ++ "\n(and crashed)\n"
          throwIO e
    else m

execute :: (DB m, ToRow q, Show q) => SQLite.Query -> q -> m ()
execute q r = do c <- ask; liftIO . queryTrace "execute" q r $ SQLite.execute c q r

execute_ :: DB m => SQLite.Query -> m ()
execute_ q = do c <- ask; liftIO . queryTrace "execute_" q "" $ SQLite.execute_ c q

executeMany :: (DB m, ToRow q, Show q) => SQLite.Query -> [q] -> m ()
executeMany q r = do c <- ask; liftIO . queryTrace "executeMany" q r $ SQLite.executeMany c q r

-- | transaction that blocks
withImmediateTransaction :: (DB m, MonadUnliftIO m) => m a -> m a
withImmediateTransaction action = do
  c <- ask
  withRunInIO \run -> SQLite.withImmediateTransaction c (run action)

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a : _) = Just a

-- * orphan instances

deriving via Text instance ToField Base32Hex

deriving via Text instance FromField Base32Hex

instance ToField WatchKind where
  toField = \case
    WatchKind.RegularWatch -> SQLite.SQLInteger 0
    WatchKind.TestWatch -> SQLite.SQLInteger 1
