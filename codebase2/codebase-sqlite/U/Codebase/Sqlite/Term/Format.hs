{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.Term.Format where

import Data.Bits (Bits)
import Data.Vector (Vector)
import Data.Word (Word64)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.LocalIds
import U.Codebase.Sqlite.Symbol
import qualified U.Codebase.Term as Term
import qualified U.Core.ABT as ABT
import qualified U.Codebase.Type as Type

newtype LocalDefnId = LocalDefnId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64
newtype LocalTextId = LocalTextId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64

type TermRef = Reference' LocalTextId (Maybe LocalDefnId)

type TypeRef = Reference' LocalTextId LocalDefnId

newtype LocallyIndexedComponent =
  LocallyIndexedComponent (Vector (LocalIds, Term))

type F =
  Term.F' LocalTextId TermRef TypeRef (Referent' TermRef TypeRef) TypeRef Symbol

type FT = Type.F' TypeRef

type Term = ABT.Term F Symbol ()
type Type = ABT.Term FT Symbol ()

data TermFormat
  = Term LocallyIndexedComponent
