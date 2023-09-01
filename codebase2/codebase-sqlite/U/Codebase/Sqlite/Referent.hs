{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Referent where

import Control.Applicative (liftA3)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Id', Referent')
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.DbId (ObjectId)
import U.Codebase.Sqlite.Reference qualified as Sqlite
import U.Codebase.Sqlite.Reference qualified as Sqlite.Reference
import Unison.Sqlite (FromRow (..), Only (..), SQLData (..), ToField (toField), ToRow (..), field)

type Referent = Referent' Sqlite.Reference Sqlite.Reference.Id

-- | The name lookup table uses this because normalizing/denormalizing hashes to ids is slower
-- than we'd like when writing/reading the entire name lookup table.
type TextReferent = Referent' Sqlite.TextReference Sqlite.TextReferenceId

type ReferentH = Referent' Sqlite.ReferenceH Sqlite.IdH

type Id = Id' ObjectId ObjectId

type LocalReferent = Referent' Sqlite.LocalReference Sqlite.LocalId

type LocalReferentH = Referent' Sqlite.LocalReferenceH Sqlite.LocalReferenceIdH

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

instance (ToRow (Reference.Reference' t h)) => ToRow (Referent' (Reference.Reference' t h) (Reference.Id' h)) where
  toRow = \case
    Referent.Ref ref -> toRow ref <> [SQLNull]
    Referent.Con ref conId -> toRow (Reference.ReferenceDerived @t ref) <> [toField conId]

instance (FromRow (Reference.Reference' t h)) => FromRow (Referent' (Reference.Reference' t h) (Reference.Id' h)) where
  fromRow = do
    ref <- fromRow
    mayCid <- field
    case mayCid of
      Nothing -> pure $ Referent.Ref ref
      Just cid -> pure $ Referent.Con (unsafeToId ref) cid
    where
      unsafeToId = \case
        Reference.ReferenceDerived ref -> ref
        Reference.ReferenceBuiltin {} -> error "unexpected builtin constructor"
