{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Reference where

import U.Codebase.Reference (Id' (Id), Reference' (ReferenceBuiltin, ReferenceDerived))
import U.Codebase.Sqlite.DbId (HashId, ObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalHashId, LocalTextId)
import U.Codebase.Sqlite.Orphans ()
import U.Util.Base32Hex
import Unison.Prelude
import Unison.Sqlite (FromField, FromRow (fromRow), Only (..), RowParser, SQLData (SQLNull), ToField, ToRow (toRow), field)

type Reference = Reference' TextId ObjectId

-- | The name lookup table uses this because normalizing/denormalizing hashes to ids is slower
-- than we'd like when writing/reading the entire name lookup table.
type TextReference = Reference' Text Base32Hex

type Id = Id' ObjectId

type LocalReferenceH = Reference' LocalTextId LocalHashId

type LocalReference = Reference' LocalTextId LocalDefnId

type LocalId = Id' LocalDefnId

type ReferenceH = Reference' TextId HashId

type IdH = Id' HashId

instance ToRow (Reference' Text Base32Hex) where
  toRow = referenceToRow

instance ToRow (Reference' TextId HashId) where
  toRow = referenceToRow

instance ToRow Reference where
  toRow = referenceToRow

referenceToRow :: (ToField t, ToField h) => Reference' t h -> [SQLData]
referenceToRow = \case
  ReferenceBuiltin t -> toRow (Only t) ++ [SQLNull, SQLNull]
  ReferenceDerived (Id h i) -> SQLNull : toRow (Only h) ++ toRow (Only i)

instance FromRow (Reference' TextId HashId) where
  fromRow = referenceFromRow'

instance FromRow (Reference) where
  fromRow = referenceFromRow'

instance FromRow (Reference' Text Base32Hex) where
  fromRow = referenceFromRow'

referenceFromRow' :: (FromField t, FromField h, Show t, Show h) => RowParser (Reference' t h)
referenceFromRow' = liftA3 mkRef field field field
  where
    mkRef (Just t) Nothing Nothing =
      ReferenceBuiltin t
    mkRef Nothing (Just h) (Just componentIdx) =
      ReferenceDerived (Id h componentIdx)
    mkRef t h i =
      error $ "invalid find_type_index type reference: " ++ str
      where
        str = "(" ++ show t ++ ", " ++ show h ++ ", " ++ show i ++ ")"

instance (ToField h) => ToRow (Id' h) where
  toRow = \case
    Id h i -> toRow (Only h) ++ toRow (Only i)

instance (FromField h) => FromRow (Id' h) where
  fromRow = Id <$> field <*> field
