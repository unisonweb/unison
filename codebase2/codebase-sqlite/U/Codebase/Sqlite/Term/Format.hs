{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.Term.Format where

-- import U.Codebase.Sqlite.DbId

import Data.Bits (Bits)
import Data.Vector (Vector)
import Data.Word (Word64)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.LocalIds
import U.Codebase.Sqlite.Symbol
import qualified U.Codebase.Term as Term
import qualified U.Core.ABT as ABT

newtype LocalTermId = LocalTermId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64
newtype LocalTypeId = LocalTypeId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64
newtype LocalTextId = LocalTextId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64

type TermRef = Reference' LocalTextId (Maybe LocalTermId)

type TypeRef = Reference' LocalTextId LocalTypeId

data LocallyIndexedComponent = 
  LocallyIndexedComponent (Vector (LocalIds, Term))

type F =
  Term.F' LocalTextId TermRef TypeRef (Referent' TermRef TypeRef) TypeRef Symbol

type Term = ABT.Term F Symbol ()

data TermFormat
  = Term LocallyIndexedComponent
