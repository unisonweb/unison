{-# LANGUAGE OverloadedStrings #-}

module Unison.Parsers where

import Control.Arrow ((***))
import Data.Text (Text)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Parser (Result(..), run, unsafeGetSucceed)
import Unison.View (DFO)
import qualified Data.Text as Text
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
    , aliasFromModule "Vector" "single"
    , aliasFromModule "Vector" "prepend"
    , aliasFromModule "Vector" "map"
    , aliasFromModule "Vector" "fold-left"
    , aliasFromModule "Vector" "empty"
    , aliasFromModule "Vector" "concatenate"
    , aliasFromModule "Vector" "append"
    -- Text
    , aliasFromModule "Text" "concatenate"
    , aliasFromModule "Text" "left"
    , aliasFromModule "Text" "right"
    , aliasFromModule "Text" "center"
    , aliasFromModule "Text" "justify"
    -- Remote
    , aliasFromModule "Remote" "fork"
    , aliasFromModule "Remote" "receive"
    , aliasFromModule "Remote" "receiveAsync"
    , aliasFromModule "Remote" "pure"
    , aliasFromModule "Remote" "bind"
    , aliasFromModule "Remote" "channel"
    , aliasFromModule "Remote" "send"
    , aliasFromModule "Remote" "here"
    , aliasFromModule "Remote" "at"
    -- Color
    , aliasFromModule "Color" "rgba"
    -- Symbol
    , aliasFromModule "Symbol" "Symbol"
    -- KeyValueStore
    ] >>= unpackAliases)
    where
      aliasFromModule m sym = alias sym (Text.intercalate "." [m, sym])
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
