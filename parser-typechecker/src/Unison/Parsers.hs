{-# LANGUAGE OverloadedStrings #-}

module Unison.Parsers where

import Control.Arrow ((***))
import Data.Text (Text)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Parser (run)
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

unsafeGetRight :: Either String a -> a
unsafeGetRight (Right a) = a
unsafeGetRight (Left err) = error err

parseTerm :: Var v => String -> Either String (Term v)
parseTerm = parseTerm' [] []

parseType :: Var v => String -> Either String (Type v)
parseType = parseType' []

parseTerm' :: Var v => [(v, Term v)] -> [(v, Type v)] -> String -> Either String (Term v)
parseTerm' termBuiltins typeBuiltins s =
  bindBuiltins termBuiltins typeBuiltins <$> run (Parser.root TermParser.term) s s0

bindBuiltins :: Var v => [(v, Term v)] -> [(v, Type v)] -> Term v -> Term v
bindBuiltins termBuiltins typeBuiltins =
   Term.typeMap (ABT.substs typeBuiltins) . ABT.substs termBuiltins

parseType' :: Var v => [(v, Type v)] -> String -> Either String (Type v)
parseType' typeBuiltins s =
  ABT.substs typeBuiltins <$> run (Parser.root TypeParser.type_) s s0

unsafeParseTerm :: Var v => String -> Term v
unsafeParseTerm = unsafeGetRight . parseTerm

unsafeParseType :: Var v => String -> Type v
unsafeParseType = unsafeGetRight . parseType

unsafeParseTerm' :: Var v => [(v, Term v)] -> [(v, Type v)] -> String -> Term v
unsafeParseTerm' er tr = unsafeGetRight . parseTerm' er tr

unsafeParseType' :: Var v => [(v, Type v)] -> String -> Type v
unsafeParseType' tr = unsafeGetRight . parseType' tr
