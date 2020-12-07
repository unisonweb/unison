{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module U.Codebase.Sqlite.Queries where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.String.Here.Uninterpolated (here)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, Only (..), SQLData, ToRow (..), (:.) (..))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField (..))
import U.Codebase.Reference (Reference')
import U.Codebase.Sqlite.DbId (BranchObjectId(..), BranchHashId(..), CausalHashId, CausalOldHashId, HashId (..), ObjectId (..), TextId)
import U.Codebase.Sqlite.ObjectType (ObjectType)
import qualified U.Codebase.Sqlite.Reference as Reference
import qualified U.Codebase.Sqlite.Referent as Referent
import U.Codebase.WatchKind (WatchKind)
import qualified U.Codebase.WatchKind as WatchKind
import U.Util.Base32Hex (Base32Hex (..))
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import UnliftIO (withRunInIO, MonadUnliftIO)

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
  deriving Show

-- |discard errors that you're sure are impossible
noExcept :: (Monad m, Show e) => ExceptT e m a -> m a
noExcept a = runExceptT a >>= \case
  Right a -> pure a
  Left e -> error $ "unexpected error: " ++ show e

-- noMaybe :: Maybe a -> a
-- noMaybe = fromMaybe (error "unexpected Nothing")

orError :: MonadError e m => e -> Maybe b -> m b
orError e = maybe (throwError e) pure

-- type DerivedReferent = Referent.Id ObjectId ObjectId
-- type DerivedReference = Reference.Id ObjectId
type TypeHashReference = Reference' TextId HashId

-- * main squeeze

saveHash :: DB m => Base32Hex -> m HashId
saveHash base32 = execute sql (Only base32) >> queryOne (loadHashId base32)
  where sql = [here| INSERT OR IGNORE INTO hash (base32) VALUES (?) |]

saveHashHash :: DB m => Hash -> m HashId
saveHashHash = saveHash . Hash.toBase32Hex

loadHashId :: DB m => Base32Hex -> m (Maybe HashId)
loadHashId base32 = queryOnly sql (Only base32)
  where sql = [here| SELECT id FROM hash WHERE base32 = ? |]

loadHashIdByHash :: DB m => Hash -> m (Maybe HashId)
loadHashIdByHash = loadHashId . Hash.toBase32Hex

expectHashIdByHash :: EDB m => Hash -> m HashId
expectHashIdByHash h = loadHashIdByHash h >>= orError (UnknownHash h)

loadHashById :: EDB m => HashId -> m Base32Hex
loadHashById h = queryOnly sql (Only h) >>= orError (UnknownHashId h)
  where sql = [here| SELECT base32 FROM hash WHERE id = ? |]

saveText :: DB m => Text -> m TextId
saveText t = execute sql (Only t) >> queryOne (loadText t)
  where sql = [here| INSERT OR IGNORE INTO text (text) VALUES (?) |]

loadText :: DB m => Text -> m (Maybe TextId)
loadText t = queryOnly sql (Only t)
  where sql = [here| SELECT id FROM text WHERE text = ? |]

expectText :: EDB m => Text -> m TextId
expectText t = loadText t >>= orError (UnknownText t)

loadTextById :: EDB m => TextId -> m Text
loadTextById h = queryOnly sql (Only h) >>= orError (UnknownTextId h)
  where sql = [here| SELECT text FROM text WHERE id = ? |]

saveHashObject :: DB m => HashId -> ObjectId -> Int -> m ()
saveHashObject hId oId version = execute sql (hId, oId, version) where
  sql = [here|
    INSERT OR IGNORE INTO hash_object (hash_id, object_id, version)
    VALUES (?, ?, ?)
  |]

saveObject :: DB m => HashId -> ObjectType -> ByteString -> m ObjectId
saveObject h t blob =
  execute sql (h, t, blob) >> queryOne (maybeObjectIdPrimaryHashId h)
  where
  sql = [here|
    INSERT OR IGNORE INTO object (primary_hash_id, type_id, bytes)
    VALUES (?, ?, ?)
  |]

loadObjectById :: EDB m => ObjectId -> m ByteString
loadObjectById oId = queryOnly sql (Only oId) >>= orError (UnknownObjectId oId)
  where sql = [here|
  SELECT bytes FROM object WHERE id = ?
|]

loadObjectWithTypeById :: EDB m => ObjectId -> m (ObjectType, ByteString)
loadObjectWithTypeById oId = queryMaybe sql (Only oId) >>= orError (UnknownObjectId oId)
  where sql = [here|
    SELECT type_id, bytes FROM object WHERE id = ?
  |]

-- |Not all hashes have corresponding objects; e.g., hashes of term types
expectObjectIdForPrimaryHashId :: EDB m => HashId -> m ObjectId
expectObjectIdForPrimaryHashId h =
  maybeObjectIdPrimaryHashId h >>= orError (UnknownHashId h)

maybeObjectIdPrimaryHashId :: DB m => HashId -> m (Maybe ObjectId)
maybeObjectIdPrimaryHashId h = queryOnly sql (Only h) where sql = [here|
  SELECT id FROM object WHERE primary_hash_id = ?
|]

expectObjectIdForAnyHashId :: EDB m => HashId -> m ObjectId
expectObjectIdForAnyHashId h =
  maybeObjectIdForAnyHashId h >>= orError (NoObjectForHashId h)

maybeObjectIdForAnyHashId :: DB m => HashId -> m (Maybe ObjectId)
maybeObjectIdForAnyHashId h = queryOnly sql (Only h) where sql = [here|
    SELECT object_id FROM hash_object WHERE hash_id = ?
  |]

-- |All objects have corresponding hashes.
loadPrimaryHashByObjectId :: EDB m => ObjectId -> m Base32Hex
loadPrimaryHashByObjectId oId = queryOnly sql (Only oId) >>= orError (UnknownObjectId oId)
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

updateObjectBlob :: DB m => ObjectId -> ByteString -> m ()
updateObjectBlob oId bs = execute sql (oId, bs) where sql = [here|
  UPDATE object SET bytes = ? WHERE id = ?
|]

-- |Maybe we would generalize this to something other than NamespaceHash if we
-- end up wanting to store other kinds of Causals here too.
saveCausal :: DB m => CausalHashId -> BranchHashId -> m ()
saveCausal self value = execute sql (self, value) where sql = [here|
  INSERT OR IGNORE INTO causal (self_hash_id, value_hash_id)
  VALUES (?, ?)
|]

loadCausalValueHashId :: EDB m => CausalHashId -> m BranchHashId
loadCausalValueHashId id =
  queryOnly sql (Only id) >>= orError (UnknownCausalHashId id) where sql = [here|
  SELECT value_hash_id FROM causal WHERE self_hash_id = ?
|]

loadBranchObjectIdByCausalHashId :: EDB m => CausalHashId -> m (Maybe BranchObjectId)
loadBranchObjectIdByCausalHashId id = queryOnly sql (Only id) where sql = [here|
  SELECT value_object_id FROM causal WHERE self_hash_id = ?
|]

saveCausalOld :: DB m => HashId -> CausalHashId -> m ()
saveCausalOld v1 v2 = execute sql (v1, v2) where sql = [here|
  INSERT OR IGNORE INTO causal_old (old_hash_id, new_hash_id) VALUES (?, ?)
|]

loadCausalHashIdByCausalOldHash :: EDB m => CausalOldHashId -> m CausalHashId
loadCausalHashIdByCausalOldHash id =
  queryOnly sql (Only id) >>= orError (UnknownCausalOldHashId id) where sql = [here|
  SELECT new_hash_id FROM causal_old where old_hash_id = ?
|]

loadOldCausalValueHash :: EDB m => CausalOldHashId -> m BranchHashId
loadOldCausalValueHash id =
 queryOnly sql (Only id) >>= orError (UnknownCausalOldHashId id) where sql = [here|
  SELECT value_hash_id FROM causal
  INNER JOIN causal_old ON self_hash_id = new_hash_id
  WHERE old_hash_id = ?
|]

saveCausalParent :: DB m => CausalHashId -> CausalHashId -> m ()
saveCausalParent child parent = execute sql (child, parent) where
  sql = [here|
    INSERT OR IGNORE INTO causal_parent (causal_id, parent_id) VALUES (?, ?)
  |]

loadCausalParents :: DB m => CausalHashId -> m [CausalHashId]
loadCausalParents h = queryList sql (Only h) where sql = [here|
  SELECT parent_id FROM causal_parent WHERE causal_id = ?
|]

loadNamespaceRoot :: EDB m => m CausalHashId
loadNamespaceRoot = queryList sql () >>= \case
  [] -> throwError NoNamespaceRoot
  [id] -> pure id
  ids -> throwError (MultipleNamespaceRoots ids)
 where sql = "SELECT causal_id FROM namespace_root"

setNamespaceRoot :: DB m => CausalHashId -> m ()
setNamespaceRoot id = execute sql (Only id) where sql = [here|
  INSERT OR REPLACE INTO namespace_root VALUES (?)
|]

saveWatch :: DB m => WatchKind -> Reference.IdH -> ByteString -> m ()
saveWatch k r blob = execute sql (r :. Only blob) >> execute sql2 (r :. Only k)
  where
    sql = [here|
      INSERT OR IGNORE
      INTO watch_result (hash_id, component_index, result)
      VALUES (?, ?, ?)
    |]
    sql2 = [here|
      INSERT OR IGNORE
      INTO watch (hash_id, component_index, watch_kind_id)
      VALUES (?, ?, ?)
    |]

loadWatch :: DB m => WatchKind -> Reference.IdH -> m (Maybe ByteString)
loadWatch k r = queryOnly sql (Only k :. r) where sql = [here|
    SELECT bytes FROM watch
    WHERE watch_kind_id = ?
      AND hash_id = ?
      AND component_index = ?
  |]

loadWatchesByWatchKind :: DB m => WatchKind -> m [Reference.Id]
loadWatchesByWatchKind k = query sql (Only k) where sql = [here|
  SELECT object_id, component_index FROM watch WHERE watch_kind_id = ?
|]

-- * Index-building
addToTypeIndex :: DB m => Reference' TextId HashId -> Referent.Id -> m ()
addToTypeIndex tp tm = execute sql (tp :. tm) where sql = [here|
  INSERT OR IGNORE INTO find_type_index (
    type_reference_builtin,
    type_reference_hash_id,
    type_reference_component_index,
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  ) VALUES (?, ?, ?, ?, ?, ?)
|]

getReferentsByType :: DB m => Reference' TextId HashId -> m [Referent.Id]
getReferentsByType r = query sql r where sql = [here|
  SELECT
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  FROM find_type_index
  WHERE type_reference_builtin = ?
    AND type_reference_hash_id = ?
    AND type_reference_component_index = ?
|]

addToTypeMentionsIndex :: DB m => Reference' TextId HashId -> Referent.Id -> m ()
addToTypeMentionsIndex tp tm = execute sql (tp :. tm) where sql = [here|
  INSERT OR IGNORE INTO find_type_mentions_index (
    type_reference_builtin,
    type_reference_hash_id,
    type_reference_component_index,
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  ) VALUES (?, ?, ?, ?, ?, ?)
|]

getReferentsByTypeMention :: DB m => Reference' TextId HashId -> m [Referent.Id]
getReferentsByTypeMention r = query sql r where sql = [here|
  SELECT
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  FROM find_type_mentions_index
  WHERE type_reference_builtin = ?
    AND type_reference_hash_id = ?
    AND type_reference_component_index = ?
|]

addToDependentsIndex :: DB m => Reference.Reference -> Reference.Id -> m ()
addToDependentsIndex dependency dependent = execute sql (dependency :. dependent)
  where sql = [here|
    INSERT OR IGNORE INTO dependents_index (
      dependency_builtin,
      dependency_object_id,
      dependency_component_index,
      dependent_object_id,
      dependent_component_index
    ) VALUES (?, ?, ?, ?, ?)
  |]

getDependentsForDependency :: DB m => Reference.Reference -> m [Reference.Id]
getDependentsForDependency dependency = query sql dependency where sql = [here|
  SELECT dependent_object_id, dependent_component_index
  FROM dependents_index
  WHERE dependency_builtin = ?
    AND dependency_object_id = ?
    AND dependency_component_index = ?
|]

getDependencyIdsForDependent :: DB m => Reference.Id -> m [Reference.Id]
getDependencyIdsForDependent dependent = query sql dependent where sql = [here|
  SELECT dependency_object_id, dependency_component_index
  FROM dependents_index
  WHERE dependency_builtin = NULL
    AND dependent_object_id = ?
    AND dependen_component_index = ?
|]

objectIdByBase32Prefix :: DB m => ObjectType -> Text -> m [ObjectId]
objectIdByBase32Prefix objType prefix = queryList sql (objType, prefix <> "%") where sql = [here|
  SELECT object.id FROM object
  INNER JOIN hash_object ON hash_object.object_id = object.id
  INNER JOIN hash ON hash_object.hash_id = hash.id
  WHERE object.type_id = ?
    AND hash.base32 LIKE ?
|]

causalHashIdByBase32Prefix :: DB m => Text -> m [CausalHashId]
causalHashIdByBase32Prefix prefix = queryList sql (Only $ prefix <> "%") where sql = [here|
  SELECT self_hash_id FROM causal
  INNER JOIN hash ON id = self_hash_id
  WHERE base32 LIKE ?
|]

namespaceHashIdByBase32Prefix :: DB m => Text -> m [BranchHashId]
namespaceHashIdByBase32Prefix prefix = queryList sql (Only $ prefix <> "%") where sql = [here|
  SELECT value_hash_id FROM causal
  INNER JOIN hash ON id = value_hash_id
  WHERE base32 LIKE ?
|]

-- * helper functions
queryList :: (DB f, ToRow q, FromField b) => SQLite.Query -> q -> f [b]
queryList q r = map fromOnly <$> query q r
queryMaybe :: (DB f, ToRow q, FromRow b) => SQLite.Query -> q -> f (Maybe b)
queryMaybe q r = headMay <$> query q r

queryOnly :: (DB f, ToRow q, FromField b) => SQLite.Query -> q -> f (Maybe b)
queryOnly q r = fmap fromOnly <$> queryMaybe q r

queryOne :: Functor f => f (Maybe b) -> f b
queryOne = fmap fromJust

queryExists :: (DB m, ToRow q) => SQLite.Query -> q -> m Bool
queryExists q r = not . null . map (id @SQLData) <$> queryList q r

query :: (DB m, ToRow q, FromRow r) => SQLite.Query -> q -> m [r]
query q r = do c <- ask; liftIO $ SQLite.query c q r
execute :: (DB m, ToRow q) => SQLite.Query -> q -> m ()
execute q r = do c <- ask; liftIO $ SQLite.execute c q r

-- |transaction that blocks
withImmediateTransaction :: (DB m, MonadUnliftIO m) => m a -> m a
withImmediateTransaction action = do
  c <- ask
  withRunInIO \run -> SQLite.withImmediateTransaction c (run action)

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:_) = Just a

-- * orphan instances
deriving via Text instance ToField Base32Hex
deriving via Text instance FromField Base32Hex

instance ToField WatchKind where
  toField = \case
    WatchKind.RegularWatch -> SQLite.SQLInteger 0
    WatchKind.TestWatch -> SQLite.SQLInteger 1
