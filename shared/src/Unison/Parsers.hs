{-# LANGUAGE OverloadedStrings #-}

module Unison.Parsers where

import Control.Arrow ((***))
import Data.Text (Text)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Parser (Result(..), run, unsafeGetSucceed)
import Unison.Var (Var)
import qualified Unison.Parser as Parser
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Type as Type
import qualified Unison.Reference as R
import qualified Unison.Var as Var

type S v = TypeParser.S v

s0 :: S v
s0 = TypeParser.s0

parseTerm :: Var v => String -> Result (S v) (Term v)
parseTerm = parseTerm' termBuiltins typeBuiltins

parseType :: Var v => String -> Result (S v) (Type v)
parseType = parseType' typeBuiltins

parseTerm' :: Var v => [(v, Term v)] -> [(v, Type v)] -> String -> Result (S v) (Term v)
parseTerm' termBuiltins typeBuiltins s =
  bindBuiltins termBuiltins typeBuiltins <$> run (Parser.root TermParser.term) s s0

bindBuiltins :: Var v => [(v, Term v)] -> [(v, Type v)] -> Term v -> Term v
bindBuiltins termBuiltins typeBuiltins =
   Term.typeMap (ABT.substs typeBuiltins) . ABT.substs termBuiltins

parseType' :: Var v => [(v, Type v)] -> String -> Result (S v) (Type v)
parseType' typeBuiltins s =
  ABT.substs typeBuiltins <$> run (Parser.root TypeParser.type_) s s0

unsafeParseTerm :: Var v => String -> Term v
unsafeParseTerm = unsafeGetSucceed . parseTerm

unsafeParseType :: Var v => String -> Type v
unsafeParseType = unsafeGetSucceed . parseType

unsafeParseTerm' :: Var v => [(v, Term v)] -> [(v, Type v)] -> String -> Term v
unsafeParseTerm' er tr = unsafeGetSucceed . parseTerm' er tr

unsafeParseType' :: Var v => [(v, Type v)] -> String -> Type v
unsafeParseType' tr = unsafeGetSucceed . parseType' tr

-- Alias <alias> <fully-qualified-name>
  -- will import the builtin <fully-qualified-name>, and once more as the alias
-- AliasFromModule
--   <modulename> e.g. "Number"
--   <aliases import modulename.alias as alias> e.g. "plus"
--   <ids import as qualified modulename.id> e.g. "minus" will import builtin "Number.plus" only
data Builtin = Builtin Text -- e.g. Builtin "()"
             | Alias Text Text
             | AliasFromModule Text [Text] [Text]

-- aka default imports
termBuiltins :: Var v => [(v, Term v)]
termBuiltins = (Var.named *** Term.ref) <$> (
    [ Builtin "()"
    , Builtin "Either.Right"
    , Builtin "Either.Left"
    , Builtin "Greater"
    , Builtin "Less"
    , Builtin "Equal"
    , Builtin "True"
    , Builtin "False"
    , Alias "unit" "()"
    , Alias "Unit" "()"
    , Alias "Some" "Optional.Some"
    , Alias "None" "Optional.None"
    , Alias "+" "Number.+"
    , Alias "-" "Number.-"
    , Alias "*" "Number.*"
    , Alias "/" "Number./"
    , AliasFromModule "Vector" ["single"] []
    , AliasFromModule "Remote" ["pure", "bind", "pure", "fork"] []
    ] >>= unpackAliases)
    where
      unpackAliases :: Builtin -> [(Text, R.Reference)]
      unpackAliases (Builtin t) = [builtin t]
      unpackAliases (Alias a sym) = [alias a sym, builtin sym]
      unpackAliases (AliasFromModule m toAlias other) =
        (aliasFromModule m <$> toAlias) ++ (builtinInModule m <$> toAlias)
          ++ (builtinInModule m <$> other)

      builtin t = (t, R.Builtin t)
      alias new known = (new, R.Builtin known)
      aliasFromModule m sym = alias sym (Text.intercalate "." [m, sym])
      builtinInModule m sym = builtin (Text.intercalate "." [m, sym])

typeBuiltins :: Var v => [(v, Type v)]
typeBuiltins = (Var.named *** Type.lit) <$>
  [ ("Number", Type.Number)
  , builtin "Unit"
  , builtin "Boolean"
  , ("Optional", Type.Optional)
  , builtin "Either"
  , builtin "Pair"
  , builtin "Order"
  , builtin "Comparison"
  , builtin "Order.Key"
  -- kv store
  , builtin "Index"
  -- html
  , builtin "Html.Link"
  -- distributed
  , builtin "Channel"
  , builtin "Duration"
  , builtin "Remote"
  , builtin "Node"
  -- hashing
  , builtin "Hash"
  ]
  where builtin t = (t, Type.Ref $ R.Builtin t)
