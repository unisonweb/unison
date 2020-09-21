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

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (ask))
import Data.ByteString (ByteString)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Maybe (fromJust)
import Data.String.Here.Uninterpolated (here)
import Data.Text (Text)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple ((:.) (..), Connection, FromRow, Only (..), SQLData (SQLNull), ToRow (..))
import Data.Word (Word64)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import U.Codebase.Reference (Reference' (ReferenceBuiltin, ReferenceDerived))
import qualified U.Codebase.Reference as Reference
import qualified U.Codebase.Referent as Referent
import U.Codebase.Sqlite.ObjectType
import U.Util.Base32Hex (Base32Hex (..))
import U.Util.Hashable (Hashable)

-- * types
type DB m = (MonadIO m, MonadReader Connection m)

newtype HashId = HashId Word64 deriving (Eq, Ord) deriving (Hashable, FromField, ToField) via Word64
newtype TextId = TextId Word64 deriving (Eq, Ord) deriving (Hashable, FromField, ToField) via Word64

newtype ObjectId = ObjectId Word64 deriving (Eq, Ord, Hashable, FromField, ToField) via Word64
newtype TypeId = TypeId ObjectId deriving (FromField, ToField) via ObjectId
newtype TermId = TermCycleId ObjectId deriving (FromField, ToField) via ObjectId
newtype DeclId = DeclCycleId ObjectId deriving (FromField, ToField) via ObjectId
newtype CausalHashId = CausalHashId HashId deriving (Hashable, FromField, ToField) via HashId
newtype CausalOldHashId = CausalOldHashId HashId deriving (Hashable, FromField, ToField) via HashId
newtype NamespaceHashId = NamespaceHashId ObjectId deriving (Hashable, FromField, ToField) via ObjectId

type DerivedReferent = Referent.Id' ObjectId ObjectId
type DerivedReference = Reference.Id' ObjectId
-- * main squeeze

saveHash :: DB m => Base32Hex -> m HashId
saveHash base32 = execute sql (Only base32) >> queryOne (loadHash base32)
  where sql = [here| INSERT OR IGNORE INTO hash (base32) VALUES (?) |]

loadHash :: DB m => Base32Hex -> m (Maybe HashId)
loadHash base32 = queryOnly sql (Only base32)
  where sql = [here| SELECT id FROM hash WHERE base32 = ? |]

loadHashById :: DB m => HashId -> m (Maybe Base32Hex)
loadHashById h = queryOnly sql (Only h)
  where sql = [here| SELECT base32 FROM hash WHERE id = ? |]

saveText :: DB m => Text -> m TextId
saveText t = execute sql (Only t) >> queryOne (loadText t)
  where sql = [here| INSERT OR IGNORE INTO text (text) VALUES (?) |]

loadText :: DB m => Text -> m (Maybe TextId)
loadText t = queryOnly sql (Only t)
  where sql = [here| SELECT id FROM text WHERE text = ? |]

loadTextById :: DB m => TextId -> m (Maybe Text)
loadTextById h = queryOnly sql (Only h)
  where sql = [here| SELECT text FROM text WHERE id = ? |]

saveHashObject :: DB m => HashId -> ObjectId -> Int -> m ()
saveHashObject hId oId version = execute sql (hId, oId, version) where
  sql = [here|
    INSERT OR IGNORE INTO hash_object (hash_id, object_id, version)
    VALUES (?, ?, ?)
  |]

saveObject :: DB m => HashId -> ObjectType -> ByteString -> m ObjectId
saveObject h t blob =
  execute sql (h, t, blob) >> queryOne (objectByPrimaryHashId h)
  where
  sql = [here|
    INSERT OR IGNORE INTO object (primary_hash_id, type_id, bytes)
    VALUES (?, ?, ?)
  |]

loadObjectById :: DB m => ObjectId -> m (Maybe ByteString)
loadObjectById oId = queryOnly sql (Only oId) where sql = [here|
  SELECT bytes FROM object WHERE id = ?
|]

objectByPrimaryHashId :: DB m => HashId -> m (Maybe ObjectId)
objectByPrimaryHashId h = queryOnly sql (Only h) where sql = [here|
  SELECT id FROM object WHERE primary_hash_id = ?
|]

updateObjectBlob :: DB m => ObjectId -> ByteString -> m ()
updateObjectBlob oId bs = execute sql (oId, bs) where sql = [here|
  UPDATE object SET bytes = ? WHERE id = ?
|]

-- |Maybe we would generalize this to something other than NamespaceHash if we
-- end up wanting to store other kinds of Causals here too.
saveCausal :: DB m => CausalHashId -> NamespaceHashId -> m ()
saveCausal self value = execute sql (self, value) where sql = [here|
  INSERT OR IGNORE INTO causal (self_hash_id, value_hash_id) VALUES (?, ?)
|]

loadCausalValueHash :: DB m => CausalHashId -> m (Maybe NamespaceHashId)
loadCausalValueHash hash = queryOnly sql (Only hash) where sql = [here|
  SELECT value_hash_id FROM causal WHERE self_hash_id = ?
|]

saveCausalOld :: DB m => HashId -> CausalHashId -> m ()
saveCausalOld v1 v2 = execute sql (v1, v2) where sql = [here|
  INSERT OR IGNORE INTO causal_old (old_hash_id, new_hash_id) VALUES (?, ?)
|]

loadCausalHashIdByCausalOldHash :: DB m => CausalOldHashId -> m (Maybe CausalHashId)
loadCausalHashIdByCausalOldHash id = queryOnly sql (Only id) where sql = [here|
  SELECT new_hash_id FROM causal_old where old_hash_id = ?
|]

loadOldCausalValueHash :: DB m => CausalOldHashId -> m (Maybe NamespaceHashId)
loadOldCausalValueHash id = queryOnly sql (Only id) where sql = [here|
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

-- * Index-building
addToTypeIndex :: DB m => Reference' TextId HashId -> DerivedReferent -> m ()
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

addToTypeMentionsIndex :: DB m => Reference' TextId HashId -> DerivedReferent -> m ()
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

addToDependentsIndex :: DB m => Reference' TextId ObjectId -> DerivedReference -> m ()
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

query :: (DB m, ToRow q, SQLite.FromRow r) => SQLite.Query -> q -> m [r]
query q r = do c <- ask; liftIO $ SQLite.query c q r
execute :: (DB m, ToRow q) => SQLite.Query -> q -> m ()
execute q r = do c <- ask; liftIO $ SQLite.execute c q r

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:_) = Just a

-- * orphan instances
deriving via Text instance ToField Base32Hex
deriving via Text instance FromField Base32Hex

instance ToRow (Reference' TextId HashId) where
  -- | builtinId, hashId, componentIndex
  toRow = \case
    ReferenceBuiltin t -> toRow (Only t) ++ [SQLNull, SQLNull]
    ReferenceDerived (Reference.Id h i) -> SQLNull : toRow (Only h) ++ toRow (Only i)

instance ToRow (Reference' TextId ObjectId) where
  -- | builtinId, hashId, componentIndex
  toRow = \case
    ReferenceBuiltin t -> toRow (Only t) ++ [SQLNull, SQLNull]
    ReferenceDerived (Reference.Id h i) -> SQLNull : toRow (Only h) ++ toRow (Only i)

instance ToRow (Reference.Id' ObjectId) where
  -- | builtinId, hashId, componentIndex
  toRow = \case
    Reference.Id h i -> toRow (Only h) ++ toRow (Only i)

instance ToRow DerivedReferent where
  -- | objectId, componentIndex, constructorIndex
  toRow = \case
    Referent.RefId (Reference.Id h i) -> toRow (Only h) ++ toRow (Only i) ++ [SQLNull]
    Referent.ConId (Reference.Id h i) cid -> toRow (Only h) ++ toRow (Only i) ++ toRow (Only cid)
