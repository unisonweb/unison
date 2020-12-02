{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Reference where

import Database.SQLite.Simple (Only (..), SQLData (..), ToRow (toRow))
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField)
import U.Codebase.Reference (Id' (Id), Reference' (ReferenceBuiltin, ReferenceDerived))
import U.Codebase.Sqlite.DbId (HashId, ObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalHashId, LocalTextId)

type Reference = Reference' TextId ObjectId

type Id = Id' ObjectId

type LocalReferenceH = Reference' LocalTextId LocalHashId

type LocalReference = Reference' LocalTextId LocalDefnId

type LocalId = Id' LocalDefnId

type ReferenceH = Reference' TextId HashId

type IdH = Id' HashId

-- * Orphan instances

instance ToRow (Reference' TextId HashId) where
  toRow = \case
    ReferenceBuiltin t -> toRow (Only t) ++ [SQLNull, SQLNull]
    ReferenceDerived (Id h i) -> SQLNull : toRow (Only h) ++ toRow (Only i)

instance ToRow (Reference' TextId ObjectId) where
  toRow = \case
    ReferenceBuiltin t -> toRow (Only t) ++ [SQLNull, SQLNull]
    ReferenceDerived (Id h i) -> SQLNull : toRow (Only h) ++ toRow (Only i)

instance ToField h => ToRow (Id' h) where
  toRow = \case
    Id h i -> toRow (Only h) ++ toRow (Only i)

instance FromField h => FromRow (Id' h) where
  fromRow = Id <$> field <*> field
