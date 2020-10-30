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
import U.Util.Hashable (Hashable)

newtype ObjectId = ObjectId Word64 deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via Word64

newtype TextId = TextId Word64 deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via Word64

newtype HashId = HashId Word64 deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via Word64

newtype PatchObjectId = PatchObjectId Word64 deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via ObjectId

newtype BranchObjectId = BranchObjectId ObjectId deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via ObjectId

newtype BranchHashId = BranchHashId { unBranchHashId :: HashId } deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via HashId

newtype CausalHashId = CausalId { unCausalHashId :: HashId } deriving (Eq, Ord, Show)
  deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via HashId

newtype TypeId = TypeId ObjectId deriving Show deriving (FromField, ToField) via ObjectId
newtype TermId = TermCycleId ObjectId deriving Show deriving (FromField, ToField) via ObjectId
newtype DeclId = DeclCycleId ObjectId deriving Show deriving (FromField, ToField) via ObjectId
-- newtype CausalHashId = CausalHashId HashId deriving Show deriving (Hashable, FromField, ToField) via HashId
newtype CausalOldHashId = CausalOldHashId HashId deriving Show deriving (Hashable, FromField, ToField) via HashId
newtype NamespaceHashId = NamespaceHashId ObjectId deriving Show deriving (Hashable, FromField, ToField) via ObjectId
