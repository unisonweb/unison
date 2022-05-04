{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Reference where

import Data.Tuple.Only (Only (..))
import U.Codebase.Reference (Id' (Id), Reference' (ReferenceBuiltin, ReferenceDerived))
import U.Codebase.Sqlite.DbId (HashId, ObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalHashId, LocalTextId)
import Unison.Prelude
import Unison.Sqlite (FromField, FromRow (fromRow), RowParser, SQLData (SQLNull), ToField, ToRow (toRow), field)

type Reference = Reference' TextId ObjectId

type TextReference = Reference' Text Text

type Id = Id' ObjectId

type LocalReferenceH = Reference' LocalTextId LocalHashId

type LocalReference = Reference' LocalTextId LocalDefnId

type LocalId = Id' LocalDefnId

type ReferenceH = Reference' TextId HashId

type IdH = Id' HashId

-- * Orphan instances

instance (ToField t, ToField h) => ToRow (Reference' t h) where
  toRow = \case
    ReferenceBuiltin t -> toRow (Only t) ++ [SQLNull, SQLNull]
    ReferenceDerived (Id h i) -> SQLNull : toRow (Only h) ++ toRow (Only i)

instance (FromField t, FromField h, Show t, Show h) => FromRow (Reference' t h) where
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

instance ToField h => ToRow (Id' h) where
  toRow = \case
    Id h i -> toRow (Only h) ++ toRow (Only i)

instance FromField h => FromRow (Id' h) where
  fromRow = Id <$> field <*> field
