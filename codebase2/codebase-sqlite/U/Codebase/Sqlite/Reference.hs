{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Reference where

import U.Codebase.Sqlite.DbId
import U.Codebase.Reference (Reference'(ReferenceBuiltin, ReferenceDerived), Id'(Id))
import Database.SQLite.Simple (SQLData(..), Only(..), ToRow(toRow))

type Reference = Reference' TextId ObjectId
type Id = Id' ObjectId

type ReferenceH = Reference' TextId HashId

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

instance ToRow Id where
  -- | builtinId, hashId, componentIndex
  toRow = \case
    Id h i -> toRow (Only h) ++ toRow (Only i)
