{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Referent where

import Database.SQLite.Simple (SQLData(..), Only(..), ToRow(..))

import U.Codebase.Referent (Id', Referent')
import qualified U.Codebase.Sqlite.Reference as Sqlite
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Referent as Referent
import qualified U.Codebase.Reference as Reference

type Referent = Referent' Sqlite.Reference Sqlite.Reference
type ReferentH = Referent' Sqlite.ReferenceH Sqlite.ReferenceH
type Id = Id' ObjectId ObjectId

type LocalReferent = Referent' Sqlite.LocalReference Sqlite.LocalReference

instance ToRow Id where
  -- | objectId, componentIndex, constructorIndex
  toRow = \case
    Referent.RefId (Reference.Id h i) -> toRow (Only h) ++ toRow (Only i) ++ [SQLNull]
    Referent.ConId (Reference.Id h i) cid -> toRow (Only h) ++ toRow (Only i) ++ toRow (Only cid)
