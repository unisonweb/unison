{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Referent where

import Database.SQLite.Simple (SQLData(..), Only(..), ToRow(..))

import U.Codebase.Referent (Id', Referent')
import U.Codebase.Sqlite.Reference (Reference)
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Referent as Referent
import qualified U.Codebase.Reference as Reference

type Referent = Referent' Reference Reference
type Id = Id' ObjectId ObjectId

instance ToRow Id where
  -- | objectId, componentIndex, constructorIndex
  toRow = \case
    Referent.RefId (Reference.Id h i) -> toRow (Only h) ++ toRow (Only i) ++ [SQLNull]
    Referent.ConId (Reference.Id h i) cid -> toRow (Only h) ++ toRow (Only i) ++ toRow (Only cid)
