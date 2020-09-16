{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.Term.Format where

-- import U.Codebase.Sqlite.DbId

import Data.Text (Text)
import Data.Bits (Bits)
import Data.Vector (Vector)
import Data.Word (Word64)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.LocalIds
import qualified U.Codebase.Term as Term
import qualified U.Core.ABT as ABT

-- Int, because that's what Data.Vector.(!) takes
newtype LocalTermId = LocalTermId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64

newtype LocalTypeId = LocalTypeId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64

newtype LocalTextId = LocalTextId Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Bits) via Word64

type TermRef = Reference' LocalTextId (Maybe LocalTermId)

type TypeRef = Reference' LocalTextId LocalTypeId

data LocallyIndexedComponent = LocallyIndexedComponent
  { lookup :: LocalIds,
    component :: Vector Term
  }

type F =
  Term.F' LocalTextId TermRef TypeRef (Referent' TermRef TypeRef) TypeRef Symbol

type Term = ABT.Term F Symbol ()

data TermFormat
  = Term LocallyIndexedComponent

data Symbol = Symbol !Word64 !Text deriving (Eq, Ord, Show)