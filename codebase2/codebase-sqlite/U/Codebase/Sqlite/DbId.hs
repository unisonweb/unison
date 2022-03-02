{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module U.Codebase.Sqlite.DbId where

import Data.Bits (Bits)
import Data.Word (Word64)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

newtype HashVersion = HashVersion Word64
  deriving stock (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via Word64

newtype ObjectId = ObjectId Word64 deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via Word64

newtype TextId = TextId Word64 deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via Word64

newtype HashId = HashId Word64 deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via Word64

newtype PatchObjectId = PatchObjectId { unPatchObjectId :: ObjectId } deriving (Eq, Ord)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via ObjectId

newtype BranchObjectId = BranchObjectId { unBranchObjectId :: ObjectId } deriving (Eq, Ord)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via ObjectId

newtype BranchHashId = BranchHashId { unBranchHashId :: HashId } deriving (Eq, Ord)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via HashId

newtype CausalHashId = CausalHashId { unCausalHashId :: HashId } deriving (Eq, Ord)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via HashId

newtype SchemaVersion = SchemaVersion Word64 deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, FromField, ToField) via Word64

instance Show PatchObjectId where
  show h = "PatchObjectId (" ++ show (unPatchObjectId h) ++ ")"

instance Show BranchObjectId where
  show h = "BranchObjectId (" ++ show (unBranchObjectId h) ++ ")"

instance Show BranchHashId where
  show h = "BranchHashId (" ++ show (unBranchHashId h) ++ ")"

instance Show CausalHashId where
  show h = "CausalHashId (" ++ show (unCausalHashId h) ++ ")"
