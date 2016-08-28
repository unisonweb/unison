{-# LANGUAGE OverloadedStrings #-}

module Unison.Parsers where

import Control.Arrow ((***))
import Data.Text (Text)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Parser (Result(..), run, unsafeGetSucceed)
import Unison.Var (Var)
import Unison.View (DFO)
import qualified Unison.Parser as Parser
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Type as Type
import qualified Unison.Reference as R
import qualified Unison.Var as Var

type V = Symbol DFO
type S = TypeParser.S V

s0 :: S
s0 = TypeParser.s0

parseTerm :: String -> Result S (Term V)
parseTerm = parseTerm' termBuiltins typeBuiltins

parseType :: String -> Result S (Type V)
parseType = parseType' typeBuiltins

parseTerm' :: [(V, Term V)] -> [(V, Type V)] -> String -> Result S (Term V)
parseTerm' termBuiltins typeBuiltins s =
  bindBuiltins termBuiltins typeBuiltins <$> run (Parser.root TermParser.term) s s0

bindBuiltins :: Var v => [(v, Term v)] -> [(v, Type v)] -> Term v -> Term v
bindBuiltins termBuiltins typeBuiltins =
   Term.typeMap (ABT.substs typeBuiltins) . ABT.substs termBuiltins

parseType' :: [(V, Type V)] -> String -> Result S (Type V)
parseType' typeBuiltins s =
  ABT.substs typeBuiltins <$> run (Parser.root TypeParser.type_) s s0

unsafeParseTerm :: String -> Term V
unsafeParseTerm = unsafeGetSucceed . parseTerm

unsafeParseType :: String -> Type V
unsafeParseType = unsafeGetSucceed . parseType

unsafeParseTerm' :: [(V, Term V)] -> [(V, Type V)] -> String -> Term V
unsafeParseTerm' er tr = unsafeGetSucceed . parseTerm' er tr

unsafeParseType' :: [(V, Type V)] -> String -> Type V
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
    [ Builtin "True"
    , Builtin "False"
    , Builtin "()"
    , Builtin "Either.Right"
    , Builtin "Either.Left"
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
  -- kv store
  , builtin "Index"
  -- html
  , builtin "Html.Link"
  -- distributed
  , builtin "Channel"
  , builtin "Duration"
  , builtin "Future"
  , builtin "Remote"
  , builtin "Node"
  -- hashing
  , builtin "Hash"
  ]
  where builtin t = (t, Type.Ref $ R.Builtin t)
