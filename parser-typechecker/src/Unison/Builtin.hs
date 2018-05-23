{-# LANGUAGE OverloadedStrings #-}
module Unison.Builtin where

import Unison.Parser (penv0)
import Unison.Parsers (unsafeParseType, unsafeParseTerm)
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.Term (Term)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.Reference as R
import qualified Unison.Type as Type
import qualified Unison.Term as Term
import qualified Unison.ABT as ABT
import qualified Unison.Var as Var

t :: String -> Type Symbol
t s = resolveBuiltinTypes $ unsafeParseType s penv0

resolveBuiltinTypes :: Type Symbol -> Type Symbol
resolveBuiltinTypes t =
  let free = Set.intersection (ABT.freeVars t) builtinTypes
  in ABT.substs [(v, Type.builtin (Var.name v)) | v <- Set.toList free ] t

tm :: String -> Term Symbol
tm s = let
  t = unsafeParseTerm s penv0
  free = Set.intersection (ABT.freeVars t) builtinTerms
  in ABT.substs [(v, Term.builtin (Var.name v)) | v <- Set.toList free ] t

builtinTypes :: Set Symbol
builtinTypes = Set.fromList . map Var.named $ [
  "Int64", "UInt64", "Float", "Boolean", "Sequence", "Text", "Stream"]

builtinTerms :: Set Symbol
builtinTerms = Set.map toSymbol (Map.keysSet builtins) where
  toSymbol (R.Builtin txt) = Var.named txt
  toSymbol _ = error "unpossible"

builtins :: Map.Map R.Reference (Type Symbol)
builtins = Map.fromList $
  [ (R.Builtin name, t typ) |
    (name, typ) <-
      [ ("Int64.+", "Int64 -> Int64 -> Int64")
      , ("Int64.-", "Int64 -> Int64 -> Int64")
      , ("Int64.*", "Int64 -> Int64 -> Int64")
      , ("Int64./", "Int64 -> Int64 -> Int64")
      , ("Int64.<", "Int64 -> Int64 -> Boolean")
      , ("Int64.>", "Int64 -> Int64 -> Boolean")
      , ("Int64.<=", "Int64 -> Int64 -> Boolean")
      , ("Int64.>=", "Int64 -> Int64 -> Boolean")
      , ("Int64.==", "Int64 -> Int64 -> Boolean")

      , ("UInt64.+", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.drop", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.*", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64./", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.<", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.>", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.<=", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.>=", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.==", "UInt64 -> UInt64 -> Boolean")

      , ("Float.+", "Float -> Float -> Float")
      , ("Float.-", "Float -> Float -> Float")
      , ("Float.*", "Float -> Float -> Float")
      , ("Float./", "Float -> Float -> Float")
      , ("Float.<", "Float -> Float -> Boolean")
      , ("Float.>", "Float -> Float -> Boolean")
      , ("Float.<=", "Float -> Float -> Boolean")
      , ("Float.>=", "Float -> Float -> Boolean")
      , ("Float.==", "Float -> Float -> Boolean")
      ]
  ]
