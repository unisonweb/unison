{-# LANGUAGE OverloadedStrings #-}

module Unison.Parsers where

import Control.Arrow ((***))
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Parser (Result(..), run, unsafeGetSucceed)
import Unison.View (DFO)
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Type as Type
import qualified Unison.Reference as R
import qualified Unison.Var as Var

type V = Symbol DFO

parseTerm :: String -> Result (Term V)
parseTerm = parseTerm' termBuiltins typeBuiltins

parseType :: String -> Result (Type V)
parseType = parseType' typeBuiltins

parseTerm' :: [(V, Term V)] -> [(V, Type V)] -> String -> Result (Term V)
parseTerm' termBuiltins typeBuiltins s = case run TermParser.term s of
  Succeed e n b ->
    Succeed (Term.typeMap (ABT.substs typeBuiltins) (ABT.substs termBuiltins e)) n b
  fail -> fail

parseType' :: [(V, Type V)] -> String -> Result (Type V)
parseType' typeBuiltins s = case run TypeParser.type_ s of
  Succeed t n b -> Succeed (ABT.substs typeBuiltins t) n b
  fail -> fail

unsafeParseTerm :: String -> Term V
unsafeParseTerm = unsafeGetSucceed . parseTerm

unsafeParseType :: String -> Type V
unsafeParseType = unsafeGetSucceed . parseType

unsafeParseTerm' :: [(V, Term V)] -> [(V, Type V)] -> String -> Term V
unsafeParseTerm' er tr = unsafeGetSucceed . parseTerm' er tr

unsafeParseType' :: [(V, Type V)] -> String -> Type V
unsafeParseType' tr = unsafeGetSucceed . parseType' tr

termBuiltins :: [(V, Term V)]
termBuiltins = (Var.named *** Term.ref) <$> (
    [ alias "+" "Number.plus"
    , alias "-" "Number.minus"
    , alias "*" "Number.times"
    , alias "/" "Number.divide"
    , builtin "()"
    -- optional
    , alias "some" "Optional.Some"
    , alias "none" "Optional.None"
    -- vector
    , alias "single" "Vector.single"
    , alias "prepend" "Vector.prepend"
    , alias "map" "Vector.map"
    , alias "fold-left" "Vector.fold-left"
    , alias "empty" "Vector.empty"
    , alias "concatenate" "Vector.concatenate"
    , alias "append" "Vector.append"
    -- Text
    , alias "concatenate" "Text.concatenate"
    , alias "left" "Text.left"
    , alias "right" "Text.right"
    , alias "center" "Text.center"
    , alias "justify" "Text.justify"
    -- Remote
    , alias "fork" "Remote.fork"
    , alias "receive" "Remote.receive"
    , alias "receiveAsync" "Remote.receiveAsync"
    , alias "pure" "Remote.pure"
    , alias "bind" "Remote.bind"
    , alias "channel" "Remote.channel"
    , alias "send" "Remote.send"
    , alias "here" "Remote.here"
    , alias "at" "Remote.at"
    -- Color
    , alias "rgba" "Color.rgba"
    -- Symbol
    , alias "Symbol" "Symbol.Symbol"
    ] >>= unpackAliases)
    where
      alias new known = (new, R.Builtin known)
      builtin t = (t, R.Builtin t)
      unpackAliases p@(t1, R.Builtin t2) =
        if t1 == t2 then [p] else [p, builtin t2]
      unpackAliases p = [p]

typeBuiltins :: [(V, Type V)]
typeBuiltins = (Var.named *** Type.lit) <$>
  [ ("Number", Type.Number)
  , builtin "Unit"
  , ("Optional", Type.Optional)
  -- ???
  , builtin "Symbol"
  , builtin "Alignment"
  , builtin "Color"
  , builtin "Fixity"
  -- distributed
  , builtin "Channel"
  , builtin "Future"
  , builtin "Remote"
  , builtin "Remote!"
  ]
  where builtin t = (t, Type.Ref $ R.Builtin t)
