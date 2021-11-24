{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.Decl.Format where

import Data.Vector (Vector)
import U.Codebase.Decl (DeclR)
import U.Codebase.Reference (Reference')
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalIds, LocalTextId)
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Type as Type
import qualified U.Core.ABT as ABT

-- | Add new formats here
data DeclFormat = Decl LocallyIndexedComponent
  deriving Show

-- | V1: Decls included `Hash`es inline
--   V2: Instead of `Hash`, we use a smaller index.
data LocallyIndexedComponent
  = LocallyIndexedComponent (Vector (LocalIds, Decl Symbol))
  deriving Show

type Decl v = DeclR TypeRef v

type Type v = ABT.Term F v ()

type F = Type.F' TypeRef

type TypeRef = Reference' LocalTextId (Maybe LocalDefnId)
