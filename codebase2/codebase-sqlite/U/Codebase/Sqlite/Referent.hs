{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Referent where

import Control.Applicative (liftA3)
import Database.SQLite.Simple (FromRow (..), Only (..), SQLData (..), ToRow (..), field)
import qualified U.Codebase.Reference as Reference
import U.Codebase.Referent (Id', Referent')
import qualified U.Codebase.Referent as Referent
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.Reference as Sqlite

type Referent = Referent' Sqlite.Reference Sqlite.Reference

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
