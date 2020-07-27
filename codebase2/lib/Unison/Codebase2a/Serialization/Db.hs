{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}

module Unison.Codebase2a.Serialization.Db where

import Unison.Prelude hiding (Map)

import Control.Lens (Traversal)
import Control.Monad.Reader (MonadReader, ask)
import Data.String.Here.Uninterpolated (here)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple (Connection, Only(..), ToRow(..), pattern (:.))
import Database.SQLite.Simple (SQLData(SQLNull,SQLText))
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.Maybe (fromJust)

import Unison.Codebase2a.Base32Hex (Base32Hex)
import qualified Unison.Codebase2a.Base32Hex as Base32Hex
import Unison.Codebase2a.ObjectType (ObjectType)

import Unison.Hash (Hash)
--import Unison.Reference (ReferenceH)
import qualified Unison.Reference as Reference
--import Unison.Referent (Referent')
import qualified Unison.Referent as Referent

newtype HashId = HashId Word64 deriving (Eq, Ord) deriving (FromField, ToField) via Word64
newtype TypeId = TypeId ObjectId deriving (FromField, ToField) via ObjectId
newtype TermId = TermCycleId ObjectId deriving (FromField, ToField) via ObjectId
newtype DeclId = DeclCycleId ObjectId deriving (FromField, ToField) via ObjectId
newtype ObjectId = ObjectId Word64 deriving (Eq, FromField, ToField) via Word64
newtype CausalHashId = CausalHashId HashId deriving (FromField, ToField) via HashId
newtype NamespaceHashId = NamespaceHashId ObjectId deriving (FromField, ToField) via ObjectId
newtype ReferenceId = ReferenceId Word64 deriving FromField via Word64
newtype ReferenceDerivedId = ReferenceDerivedId Word64 deriving (FromField, ToField) via Word64
newtype ReferentDerivedId = ReferentDerivedId Word64 deriving (FromField, ToField) via Word64

saveHash :: DB m => Base32Hex -> m HashId
saveHash base32 = execute sql (Only base32) >> queryOne (loadHash base32)
  where sql = [here| INSERT OR IGNORE INTO hash (base32) VALUES (?) |]

saveHashByteString :: DB m => Hash -> m HashId
saveHashByteString h = saveHash (Base32Hex.fromHash h)

loadHash :: DB m => Base32Hex -> m (Maybe HashId)
loadHash base32 = queryMaybe sql (Only base32) where
  sql = [here| SELECT id FROM hash WHERE base32 = ? |]

loadHashById :: DB m => HashId -> m (Maybe Base32Hex)
loadHashById h = queryMaybe sql (Only h) where
  sql = [here| SELECT base32 FROM hash WHERE id = ? |]

saveHashObject :: DB m => HashId -> ObjectId -> Int -> m ()
saveHashObject hId oId version = execute sql (hId, oId, version) where
  sql = [here|
    INSERT OR IGNORE INTO hash_object (hash_id, object_id, version)
    VALUES (?, ?, ?)
  |]

saveObject :: DB m => HashId -> ObjectType -> ByteString -> m ObjectId
saveObject h t blob =
  execute sql (h, t, blob) >> queryOne (objectByPrimaryHashId h) where
  sql = [here|
    INSERT OR IGNORE INTO object (primary_hash_id, type_id, bytes)
    VALUES (?, ?, ?)
  |]

loadObjectById :: DB m => ObjectId -> m (Maybe ByteString)
loadObjectById oId = queryMaybe sql (Only oId) where sql = [here|
  SELECT bytes FROM object WHERE id = ?
|]

objectByPrimaryHashId :: DB m => HashId -> m (Maybe ObjectId)
objectByPrimaryHashId h = queryMaybe sql (Only h) where sql = [here|
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

saveCausalParent :: DB m => CausalHashId -> CausalHashId -> m ()
saveCausalParent child parent = execute sql (child, parent) where
  sql = [here|
    INSERT OR IGNORE INTO causal_parent (causal_id, parent_id) VALUES (?, ?)
  |]

loadCausalParents :: DB m => HashId -> m [HashId]
loadCausalParents h = queryList sql (Only h) where sql = [here|
  SELECT parent_id FROM causal_parent WHERE causal_id = ?
|]

saveTypeOfReferent :: DB m => Referent2Id -> ByteString -> m ()
saveTypeOfReferent r bs = execute sql (r :. Only bs) where
  sql = [here|
    INSERT OR IGNORE INTO type_of_referent
      (hash_id, component_index, constructor_index, bytes)
    VALUES (?, ?, ?, ?)
  |]

loadTypeOfReferent :: DB m => Referent2Id -> m (Maybe ByteString)
loadTypeOfReferent r = queryMaybe sql r where sql = [here|
  SELECT bytes FROM type_of_referent
  WHERE hash_id = ?
    AND component_index = ?
    AND constructor_index = ?
|]


--   ---- these correspond to a different version of the table
--  -- this returns () because `r` is the PK
--  saveTypeOfReferent :: DB m => ReferentDerivedId -> TypeId -> m ()
--  saveTypeOfReferent r typeId = execute sql (r, typeId) where
--    sql = [here|
--      INSERT OR IGNORE INTO type_of_referent (referent_derived_id, type_object_id)
--      VALUES (?, ?)
--    |]
--  loadTypeOfReferent :: DB m => ReferentDerivedId -> m (Maybe TypeId)
--  loadTypeOfReferent r = queryMaybe sql (Only r) where
--    sql = [here|
--      SELECT type_object_id FROM type_of_referent
--      WHERE referent_derivedId = ?
--    |]

--- find index stuff
addDependencyToIndex :: DB m => Reference2Id -> Reference2 -> m ()
addDependencyToIndex dependent dependency =
  execute sql (dependency :. dependent) where sql = [here|
    INSERT OR IGNORE INTO dependents_index (
      dependency_builtin,
      dependency_hash_id,
      dependency_component_index,
      dependent_hash_id,
      dependent_component_index
    ) VALUES (?, ?, ?, ?, ?)
  |]

addToFindByTypeIndex :: DB m => Referent2Id -> Reference2 -> m ()
addToFindByTypeIndex termReferent typeReference =
  execute sql (typeReference :. termReferent) where sql = [here|
    INSERT OR IGNORE INTO find_type_index (
      type_reference_builtin,
      type_reference_hash_id,
      type_reference_component_index,
      term_referent_hash_id,
      term_referent_component_index,
      term_referent_constructor_index
    ) VALUES (?, ?, ?, ?, ?, ?)
  |]

addToFindByTypeMentionsIndex :: DB m => Referent2Id -> Reference2 -> m ()
addToFindByTypeMentionsIndex termReferent typeReference =
  execute sql (typeReference :. termReferent) where sql = [here|
    INSERT OR IGNORE INTO find_type_index (
      type_reference_builtin,
      type_reference_hash_id,
      type_reference_component_index,
      term_referent_hash_id,
      term_referent_component_index,
      term_referent_constructor_index
    ) VALUES (?, ?, ?, ?, ?, ?)
  |]

--  saveReferenceDerived :: DB m => Reference.IdH HashId -> m ReferenceDerivedId
--  saveReferenceDerived r =
--    execute sql r >> queryOne (loadReferenceDerived r) where
--    sql = [here|
--      INSERT OR IGNORE INTO reference_derived (hash_id, component_index)
--      VALUES (?, ?)
--    |]
--
--  loadReferenceDerived :: DB m => Reference.IdH HashId -> m (Maybe ReferenceDerivedId)
--  loadReferenceDerived r = queryMaybe sql r where sql = [here|
--    SELECT id FROM reference_derived
--    WHERE hash_id = ? AND component_index = ?
--  |]
--
--  saveReference :: DB m => Reference' ReferenceDerivedId -> m ReferenceId
--  saveReference r =
--    execute sql r >> fmap fromJust (loadReference r) where sql = [here|
--      INSERT OR IGNORE INTO reference (builtin, reference_derived_id)
--      VALUES (?, ?)
--    |]
--
--  loadReference :: DB m => Reference' ReferenceDerivedId -> m (Maybe ReferenceId)
--  loadReference r = queryMaybe sql r where sql = [here|
--    SELECT id FROM reference
--    WHERE builtin = ? AND reference_derived_Id = ?
--  |]
--
--  -- We could pass Referent' ReferenceDerivedId once the ConstructorType field is removed
--  saveReferentDerived :: DB m => Referent' ReferenceDerivedId -> m ReferentDerivedId
--  saveReferentDerived r =
--    execute sql r >> fmap fromJust (loadReferentDerived r) where
--    sql = [here|
--      INSERT OR IGNORE INTO referent_derived (reference_derived_id, constructor_id)
--      VALUES (?, ?)
--    |]
--
--  loadReferentDerived :: DB m => Referent' ReferenceDerivedId -> m (Maybe ReferentDerivedId)
--  loadReferentDerived r = queryMaybe sql r where
--    sql = [here|
--      SELECT id FROM referent_derived
--      WHERE reference_derived_id = ? AND constructor_id = ?
--    |]


queryList :: (DB f, ToRow q, FromField b) => SQLite.Query -> q -> f [b]
queryList q r = map fromOnly <$> query q r
queryMaybe :: (DB f, ToRow q, FromField b) => SQLite.Query -> q -> f (Maybe b)
queryMaybe q r = fmap fromOnly . headMay <$> query q r

queryOne :: Functor f => f (Maybe b) -> f b
queryOne = fmap fromJust

queryExists :: (DB m, ToRow q) => SQLite.Query -> q -> m Bool
queryExists q r = not . null . map (id @SQLData) <$> queryList q r

type DB m = (MonadIO m, MonadReader Connection m)

query :: (DB m, ToRow q, SQLite.FromRow r) => SQLite.Query -> q -> m [r]
query q r = do c <- ask; liftIO $ SQLite.query c q r
execute :: (DB m, ToRow q) => SQLite.Query -> q -> m ()
execute q r = do c <- ask; liftIO $ SQLite.execute c q r

--instance ToRow (Reference' ReferenceDerivedId) where
--  toRow r = toRow $ case r of
--    Reference.Builtin t -> (Just t, Nothing)
--    Reference.DerivedId rdId -> (Nothing, Just rdId)
--
--instance ToRow (Referent' ReferenceDerivedId) where
--  toRow r = toRow $ case r of
--    Referent.Ref' r -> (Just r, Nothing)
--    Referent.Con' r i _ct -> (Just r, Just i)

type ComponentIndex = Word64
type ConstructorIndex = Int

data Reference2
  = Reference2Builtin Text | Reference2Derived Reference2Id
  deriving (Eq, Ord)

data Reference2Id
  = Reference2Id HashId ComponentIndex
  deriving (Eq, Ord)

data Referent2
  = Referent2Ref Reference2 | Referent2Con Reference2 ConstructorIndex
  deriving (Eq, Ord)

data Referent2Id
  = Referent2IdRef Reference2Id | Referent2IdCon Reference2Id ConstructorIndex
  deriving (Eq, Ord)

pattern Reference2Id' :: HashId -> ComponentIndex -> Reference2
pattern Reference2Id' h i = Reference2Derived (Reference2Id h i)

pattern Referent2IdRef' :: HashId -> ComponentIndex -> Referent2Id
pattern Referent2IdRef' h i = Referent2IdRef (Reference2Id h i)
--
--reference2idFromIdH :: Reference.IdH HashId -> Reference2Id
--reference2idFromIdH (Reference.IdH h i _n) = Reference2Id h i
--
--ref2idhash :: Lens' Reference2Id HashId
--ref2idhash = lens (\(Reference2Id h _i) -> h) (\(Reference2Id _h i) h -> Reference2Id h i)

referenceTraversal :: Traversal Reference.Reference Reference2 Hash HashId
referenceTraversal f = \case
  Reference.Builtin text -> pure (Reference2Builtin text)
  Reference.DerivedId (Reference.IdH h i _n) ->
    f h <&> \h -> Reference2Derived (Reference2Id h i)

referentTraversal :: Traversal Referent.Referent Referent2 Hash HashId
referentTraversal f = \case
  Referent.Ref r -> Referent2Ref <$> referenceTraversal f r
  Referent.Con r i _ct -> Referent2Con <$> referenceTraversal f r <*> pure i

--reference2fromReferenceH = \case
--  Reference.Builtin text -> Reference2Builtin text
--  Reference.DerivedId idh -> Reference2Derived (reference2idFromIdH idh)


instance ToRow Reference2 where
  toRow = \case
    Reference2Builtin text -> [SQLText text, SQLNull, SQLNull]
    Reference2Derived id  -> SQLNull : toRow id

instance ToRow Referent2 where
  toRow = toRow . \case
    Referent2Ref r -> r :. Only Nothing
    Referent2Con r cid  -> r :. Only (Just cid)

instance ToRow Referent2Id where
  toRow = toRow . \case
    Referent2IdRef id -> id :. Only Nothing
    Referent2IdCon id cid -> id :. Only (Just cid)

instance ToRow Reference2Id where
  toRow (Reference2Id h i) = toRow (Just h, Just i)

--instance ToRow (Referent.IdH HashId) where
--  toRow = toRow . \case
--    Referent.Ref' (Reference.IdH h i _n) -> (h, i, Nothing)
--    Referent.Con' (Reference.IdH h i _n) cid _ct -> (h, i, Just cid)

--instance ToRow (ReferenceH HashId) where
--  toRow = toRow . \case
--    Reference.Builtin text -> (Just text, Nothing, Nothing)
--    Reference.DerivedId (Reference.IdH h i _n) -> (Nothing, Just h, Just i)
--
--instance ToRow (Reference.IdH HashId) where
--  toRow (Reference.IdH oid i _n) = toRow (oid, i)
--
--instance ToRow (Referent.IdH HashId) where
--  toRow = toRow . \case
--    Referent.Ref' (Reference.IdH h i _n) -> (h, i, Nothing)
--    Referent.Con' (Reference.IdH h i _n) cid _ct -> (h, i, Just cid)



