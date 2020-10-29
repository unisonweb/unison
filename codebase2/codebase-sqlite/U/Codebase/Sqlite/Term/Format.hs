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
import qualified U.Codebase.Sqlite.Reference as Sqlite
import U.Codebase.Sqlite.DbId (ObjectId, TextId)

newtype LocalDefnId = LocalDefnId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64
newtype LocalTextId = LocalTextId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64

type TermRef = Reference' LocalTextId (Maybe LocalDefnId)

type TypeRef = Reference' LocalTextId LocalDefnId

type TermLink = Referent' TermRef TypeRef
type TypeLink = TypeRef

type LocallyIndexedComponent = LocallyIndexedComponent' TextId ObjectId
newtype LocallyIndexedComponent' t d =
  LocallyIndexedComponent (Vector (LocalIds' t d, Term))

type F =
  Term.F' LocalTextId TermRef TypeRef TermLink TypeLink Symbol

type FT = Type.F' TypeRef

type Term = ABT.Term F Symbol ()
type Type = ABT.Term FT Symbol ()

-- * Type of Term
-- Maybe these should have a LocalIds index too; or share one with the term?
type FTT = Type.F' Sqlite.Reference
type TypeOfTerm = ABT.Term FTT Symbol ()

data TermFormat
  = Term LocallyIndexedComponent
