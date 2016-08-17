{-# LANGUAGE OverloadedStrings #-}

module Unison.Parsers where

import Control.Arrow ((***))
import Data.Text (Text)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Parser (Result(..), run, unsafeGetSucceed)
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

parseTerm :: String -> Result (Term V)
parseTerm = parseTerm' termBuiltins typeBuiltins

parseType :: String -> Result (Type V)
parseType = parseType' typeBuiltins

parseTerm' :: [(V, Term V)] -> [(V, Type V)] -> String -> Result (Term V)
parseTerm' termBuiltins typeBuiltins s = case run (Parser.root TermParser.term) s of
  Succeed e n b ->
    Succeed (Term.typeMap (ABT.substs typeBuiltins) (ABT.substs termBuiltins e)) n b
  fail -> fail

parseType' :: [(V, Type V)] -> String -> Result (Type V)
parseType' typeBuiltins s = case run (Parser.root TypeParser.type_) s of
  Succeed t n b -> Succeed (ABT.substs typeBuiltins t) n b
  fail -> fail

prelude' :: String -> String
prelude' body = unlines
  [ "let"
  , "  Index.empty : forall k v . Remote (Index k v);"
  , "  Index.empty = Remote.map Index.unsafeEmpty Remote.here"
  , "in", "(", prelude body, ")"]

prelude :: String -> String
prelude body = unlines
  [ "let"
  , "  Remote.transfer : Node -> Remote Unit;"
  , "  Remote.transfer node = Remote.at node unit;"
  , ""
  , "  then : forall a b c . (a -> b) -> (b -> c) -> a -> c;"
  , "  then f1 f2 x = f2 (f1 x);"
  , ""
  , "  Optional.map : forall a b . (a -> b) -> Optional a -> Optional b;"
  , "  Optional.map f = Optional.fold None (f `then` Some);"
  , ""
  , "  Optional.bind : forall a b . (a -> Optional b) -> Optional a -> Optional b;"
  , "  Optional.bind f = Optional.fold None f;"
  , ""
  , "  Optional.pure : forall a . a -> Optional a;"
  , "  Optional.pure = Some;"
  , ""
  , "  Either.map : forall a b c . (b -> c) -> Either a b -> Either a c;"
  , "  Either.map f = Either.fold Left (f `then` Right);"
  , ""
  , "  Either.pure : forall a b . b -> Either a b;"
  , "  Either.pure = Right;"
  , ""
  , "  Either.bind : forall a b c . (b -> Either a c) -> Either a b -> Either a c;"
  , "  Either.bind = Either.fold Left;"
  , ""
  , "  Either.swap : forall a b . Either a b -> Either b a;"
  , "  Either.swap e = Either.fold Right Left e;"
  , ""
  , "  const x y = x;"
  , ""
  , "  first : forall a b . Pair a b -> a;"
  , "  first p = Pair.fold const p;"
  , ""
  , "  rest : forall a b . Pair a b -> b;"
  , "  rest p = Pair.fold (x y -> y) p;"
  , ""
  , "  1st = first;"
  , "  2nd = rest `then` first;"
  , "  3rd = rest `then` (rest `then` first);"
  , "  4th = rest `then` (rest `then` (rest `then` first));"
  , "  5th = rest `then` (rest `then` (rest `then` (rest `then` first)))"
  , ""
  , "in" , "(", body, ")"]

unsafeParseTermWithPrelude' :: String -> Term V
unsafeParseTermWithPrelude' prog = unsafeParseTerm (prelude' prog)

unsafeParseTermWithPrelude :: String -> Term V
unsafeParseTermWithPrelude prog = unsafeParseTerm (prelude prog)

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
termBuiltins :: [(V, Term V)]
termBuiltins = (Var.named *** Term.ref) <$> (
    [ Alias "+" "Number.plus"
    , Alias "-" "Number.minus"
    , Alias "*" "Number.times"
    , Alias "/" "Number.divide"
    , Alias ">" "Number.greaterThan"
    , Alias "<" "Number.lessThan"
    , Alias ">=" "Number.greaterThanOrEqual"
    , Alias "<=" "Number.lessThanOrEqual"
    , Alias "==" "Number.equal"
    , Alias "if" "Boolean.if"
    , Builtin "True"
    , Builtin "False"
    , Builtin "()"
    , Alias "unit" "()"
    , Alias "Some" "Optional.Some"
    , Alias "None" "Optional.None"
    , Alias "Left" "Either.Left"
    , Alias "Right" "Either.Right"
    , Builtin "Either.fold"
    , Builtin "Optional.fold"
    , Builtin "Pair.fold"
    , Builtin "Pair"
    , AliasFromModule "Vector"
        ["single", "prepend", "map", "fold-left", "concatenate", "append"] ["empty"]
    , AliasFromModule "Text"
        ["concatenate", "left", "right", "center", "justify"] []
    , AliasFromModule "Remote"
        ["fork", "receive", "receiveAsync", "pure", "bind", "map", "channel", "send", "here", "at", "spawn"] []
    , AliasFromModule "Color" ["rgba"] []
    , AliasFromModule "Symbol" ["Symbol"] []
    , AliasFromModule "Index" ["lookup", "unsafeLookup", "insert", "unsafeInsert", "empty", "unsafeEmpty"] []
    , AliasFromModule "Html" ["getLinks", "getHref", "getDescription"] []
    , AliasFromModule "Http" ["getURL", "unsafeGetURL"] []
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

typeBuiltins :: [(V, Type V)]
typeBuiltins = (Var.named *** Type.lit) <$>
  [ ("Number", Type.Number)
  , builtin "Unit"
  , builtin "Boolean"
  , ("Optional", Type.Optional)
  , builtin "Either"
  , builtin "Pair"
  -- ???
  , builtin "Symbol"
  , builtin "Alignment"
  , builtin "Color"
  , builtin "Fixity"
  -- kv store
  , builtin "Index"
  -- html
  , builtin "Link"
  -- distributed
  , builtin "Channel"
  , builtin "Future"
  , builtin "Remote"
  , builtin "Node"
  ]
  where builtin t = (t, Type.Ref $ R.Builtin t)
