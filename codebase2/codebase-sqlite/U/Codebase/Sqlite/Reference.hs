{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Reference where

import U.Codebase.Sqlite.DbId
import U.Codebase.Reference (Reference'(ReferenceBuiltin, ReferenceDerived), Id'(Id))
import Database.SQLite.Simple (SQLData(..), Only(..), ToRow(toRow))
import Database.SQLite.Simple.FromRow (FromRow(fromRow), field)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalTextId)

type Reference = Reference' TextId ObjectId
type Id = Id' ObjectId

type LocalReference = Reference' LocalTextId LocalDefnId
type LocalId = Id' LocalDefnId

type ReferenceH = Reference' TextId HashId
type IdH = Id' HashId

-- * Orphan instances
instance ToRow (Reference' TextId HashId) where
  -- | builtinId, hashId, componentIndex
  toRow = \case
    ReferenceBuiltin t -> toRow (Only t) ++ [SQLNull, SQLNull]
    ReferenceDerived (Id h i) -> SQLNull : toRow (Only h) ++ toRow (Only i)

instance ToRow (Reference' TextId ObjectId) where
  -- | builtinId, hashId, componentIndex
  toRow = \case
    ReferenceBuiltin t -> toRow (Only t) ++ [SQLNull, SQLNull]
    ReferenceDerived (Id h i) -> SQLNull : toRow (Only h) ++ toRow (Only i)

instance ToField h => ToRow (Id' h) where
  toRow = \case
    Id h i -> toRow (Only h) ++ toRow (Only i)

instance FromField h => FromRow (Id' h) where
  fromRow = Id <$> field <*> field
