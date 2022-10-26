{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Referent where

import Control.Applicative (liftA3)
import qualified U.Codebase.Reference as Reference
import U.Codebase.Referent (Id', Referent')
import qualified U.Codebase.Referent as Referent
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.Reference as Sqlite
import Unison.Sqlite (FromRow (..), Only (..), SQLData (..), ToField (toField), ToRow (..), field)

type Referent = Referent' Sqlite.Reference Sqlite.Reference

-- | The name lookup table uses this because normalizing/denormalizing hashes to ids is slower
-- than we'd like when writing/reading the entire name lookup table.
type TextReferent = Referent' Sqlite.TextReference Sqlite.TextReference

type ReferentH = Referent' Sqlite.ReferenceH Sqlite.ReferenceH

type Id = Id' ObjectId ObjectId

type LocalReferent = Referent' Sqlite.LocalReference Sqlite.LocalReference

type LocalReferentH = Referent' Sqlite.LocalReferenceH Sqlite.LocalReferenceH

instance ToRow Id where
  toRow = \case
    Referent.RefId (Reference.Id h i) -> toRow (Only h) ++ toRow (Only i) ++ [SQLNull]
    Referent.ConId (Reference.Id h i) cid -> toRow (Only h) ++ toRow (Only i) ++ toRow (Only cid)

instance FromRow Id where
  fromRow = liftA3 mkId field field field
    where
      mkId h i mayCid = case mayCid of
        Nothing -> Referent.RefId (Reference.Id h i)
        Just cid -> Referent.ConId (Reference.Id h i) cid

instance (ToRow (Reference.Reference' t h)) => ToRow (Referent' (Reference.Reference' t h) (Reference.Reference' t h)) where
  toRow = \case
    Referent.Ref ref -> toRow ref <> [SQLNull]
    Referent.Con ref conId -> toRow ref <> [toField conId]

instance (FromRow (Reference.Reference' t h)) => FromRow (Referent' (Reference.Reference' t h) (Reference.Reference' t h)) where
  fromRow = do
    ref <- fromRow
    mayCid <- field
    case mayCid of
      Nothing -> pure $ Referent.Ref ref
      Just cid -> pure $ Referent.Con ref cid
