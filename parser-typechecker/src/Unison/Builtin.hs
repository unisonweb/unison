{-# LANGUAGE OverloadedStrings #-}
module Unison.Builtin where

import           Control.Arrow ((&&&))
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Unison.ABT as ABT
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers -- remove this dependency on Parsers
import qualified Unison.Reference as R
import           Unison.Symbol (Symbol)
import           Unison.Term (Term)
import qualified Unison.Term as Term
import           Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Var as Var
import           Unison.Var (Var)

-- todo: to update these, just inline definition of Parsers.{unsafeParseType, unsafeParseTerm}
-- then merge Parsers2 back into Parsers (and GC and unused functions)
-- parse a type, hard-coding the builtins defined in this file
t :: Var v => String -> Type v
t s = resolveBuiltins builtinTypes Type.builtin $
        Parsers.unsafeParseType s Parser.penv0

-- parse a term, hard-coding the builtins defined in this file
tm :: Var v => String -> Term v
tm s = resolveBuiltins builtinTerms Term.builtin $
        Parsers.unsafeParseTerm s Parser.penv0

-- substitute free vars from `t` intersect `b` using `mkTerm` on those vs Text names
resolveBuiltins :: (Foldable f, Functor f, Var v) =>
                 Set v
                 -> (Text -> ABT.Term f v a)
                 -> ABT.Term f v a
                 -> ABT.Term f v a
resolveBuiltins b mkTerm t = let
  free = Set.intersection (ABT.freeVars t) b -- Arya asks: does this do anything?  what if we just used `b` without calling `freeVars`?
  in ABT.substs [(v, mkTerm (Var.name v)) | v <- Set.toList free] t

builtinTypes :: Var v => Set v
builtinTypes = Set.fromList . map Var.named $ [
  "Int64", "UInt64", "Float", "Boolean", "Sequence", "Text", "Stream"]

builtinTerms :: Var v => Set v
builtinTerms = Set.map toSymbol (Map.keysSet builtins) where

builtinEnv :: Var v => [(v, Term v)]
builtinEnv = (toSymbol &&& Term.ref) <$> Map.keys builtins

toSymbol :: Var v => R.Reference -> v
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
      , ("Int64.increment", "Int64 -> Int64")
      , ("Int64.is-even", "Int64 -> Boolean")
      , ("Int64.is-odd", "Int64 -> Boolean")
      , ("Int64.signum", "Int64 -> Int64")
      , ("Int64.negate", "Int64 -> Int64")

      , ("UInt64.+", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.drop", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.sub", "UInt64 -> UInt64 -> Int64")
      , ("UInt64.*", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64./", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.<", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.>", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.<=", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.>=", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.==", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.increment", "UInt64 -> UInt64")
      , ("UInt64.is-even", "UInt64 -> Boolean")
      , ("UInt64.is-odd", "UInt64 -> Boolean")

      , ("Float.+", "Float -> Float -> Float")
      , ("Float.-", "Float -> Float -> Float")
      , ("Float.*", "Float -> Float -> Float")
      , ("Float./", "Float -> Float -> Float")
      , ("Float.<", "Float -> Float -> Boolean")
      , ("Float.>", "Float -> Float -> Boolean")
      , ("Float.<=", "Float -> Float -> Boolean")
      , ("Float.>=", "Float -> Float -> Boolean")
      , ("Float.==", "Float -> Float -> Boolean")

      , ("Boolean.not", "Boolean -> Boolean")

      , ("Text.empty", "Text")
      , ("Text.concatenate", "Text -> Text -> Text")
      , ("Text.take", "UInt64 -> Text -> Text")
      , ("Text.drop", "UInt64 -> Text -> Text")
      , ("Text.size", "Text -> UInt64")
      , ("Text.==", "Text -> Text -> Boolean")
      , ("Text.!=", "Text -> Text -> Boolean")
      , ("Text.<=", "Text -> Text -> Boolean")
      , ("Text.>=", "Text -> Text -> Boolean")
      , ("Text.<", "Text -> Text -> Boolean")
      , ("Text.>", "Text -> Text -> Boolean")

      , ("Stream.empty", "forall a . Stream a")
      , ("Stream.from-int64", "Int64 -> Stream Int64")
      , ("Stream.cons", "forall a . a -> Stream a -> Stream a")
      , ("Stream.take", "forall a . UInt64 -> Stream a -> Stream a")
      , ("Stream.drop", "forall a . UInt64 -> Stream a -> Stream a")
      , ("Stream.map", "forall a b . (a -> b) -> Stream a -> Stream b")
      , ("Stream.fold-left", "forall a b . b -> (b -> a -> b) -> Stream a -> b")

      , ("Sequence.empty", "forall a . Sequence a")
      , ("Sequence.cons", "forall a . a -> Sequence a -> Sequence a")
      , ("Sequence.snoc", "forall a . Sequence a -> a -> Sequence a")
      , ("Sequence.take", "forall a . UInt64 -> Sequence a -> Sequence a")
      , ("Sequence.size", "forall a . Sequence a -> UInt64")
      ]
  ]
