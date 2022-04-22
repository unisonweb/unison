{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Sqlite.Name where

import Unison.Prelude
import Unison.Sqlite (FromRow (..), ToField (..), ToRow (..), field)

data Name ref = Name
  { name :: Text,
    ref :: ref
  }
  deriving stock (Show, Functor, Foldable, Traversable)

instance ToRow ref => ToRow (Name ref) where
  toRow (Name {name, ref}) =
    [toField name] <> toRow ref

instance FromRow ref => FromRow (Name ref) where
  fromRow = do
    name <- field
    ref <- fromRow
    pure (Name {..})
