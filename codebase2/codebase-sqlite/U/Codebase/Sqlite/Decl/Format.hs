{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.Decl.Format where

import Data.Bits (Bits)
import Data.Vector (Vector)
import Data.Word (Word64)
import U.Codebase.Decl (DeclR)
import U.Codebase.Reference (Reference')
import U.Codebase.Sqlite.LocalIds (LocalIds)
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Type as Type
import qualified U.Core.ABT as ABT

-- | Add new formats here
data DeclFormat = Decl LocallyIndexedComponent

-- | V1: Decls included `Hash`es inline
--   V2: Instead of `Hash`, we use a smaller index.
data LocallyIndexedComponent
  = LocallyIndexedComponent (Vector (LocalIds, Decl Symbol))

type Decl v = DeclR TypeRef v

type Type v = ABT.Term (Type.F' TypeRef) v ()

type TypeRef = Reference' LocalTextId (Maybe LocalTypeId)

newtype LocalTextId = LocalTextId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64

newtype LocalTypeId = LocalTypeId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64
