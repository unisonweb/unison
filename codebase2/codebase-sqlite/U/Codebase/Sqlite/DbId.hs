{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

module U.Codebase.Sqlite.DbId where

import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.Word (Word64)
import U.Util.Hashable (Hashable)
import Data.Bits (Bits)

newtype ObjectId = ObjectId Word64 deriving (Eq, Ord, Show) deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via Word64
newtype TextId = TextId Word64 deriving (Eq, Ord, Show) deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via Word64
newtype HashId = HashId Word64 deriving (Eq, Ord, Show) deriving (Hashable, FromField, ToField) via Word64

newtype PatchId = PatchId Word64 deriving (Eq, Ord, Show) deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via ObjectId

newtype BranchId = BranchId Word64 deriving (Eq, Ord, Show) deriving (Num, Real, Enum, Integral, Bits, Hashable, FromField, ToField) via Word64
