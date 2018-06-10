{-# LANGUAGE OverloadedStrings #-}

module Unison.Parsers where

import qualified Data.Text as Text
import           Data.Text.IO (readFile)
import           Prelude hiding (readFile)
import qualified Unison.ABT as ABT
import qualified Unison.FileParser as FileParser
import           Unison.Parser (PEnv)
import qualified Unison.Parser as Parser
import           Unison.Symbol (Symbol)
import           Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import           Unison.Type (Type)
import qualified Unison.TypeParser as TypeParser
import           Unison.UnisonFile (UnisonFile)
import           Unison.Var (Var)

type S v = TypeParser.S v

s0 :: S v
s0 = TypeParser.s0

unsafeGetRight :: Either String a -> a
unsafeGetRight (Right a) = a
unsafeGetRight (Left err) = error err

parseTerm :: Var v => String -> PEnv -> Either String (Term v)
parseTerm = parseTerm' [] []

parseType :: Var v => String -> PEnv -> Either String (Type v)
parseType = parseType' []

parseFile :: Var v => FilePath -> [(v, Term v)] -> String -> PEnv -> Either String (UnisonFile v)
parseFile filename builtinEnv s =
  Parser.run' (Parser.root (FileParser.file builtinEnv)) s s0 filename

parseTerm' :: Var v
           => [(v, Term v)]
           -> [(v, Type v)]
           -> String
           -> PEnv
           -> Either String (Term v)
parseTerm' termBuiltins typeBuiltins s =
  fmap (bindBuiltins termBuiltins typeBuiltins) <$>
    Parser.run (Parser.root TermParser.term) s s0

bindBuiltins :: Var v => [(v, Term v)] -> [(v, Type v)] -> Term v -> Term v
bindBuiltins termBuiltins typeBuiltins =
   Term.typeMap (ABT.substs typeBuiltins) . ABT.substs termBuiltins

parseType' :: Var v => [(v, Type v)] -> String -> PEnv -> Either String (Type v)
parseType' typeBuiltins s =
  fmap (ABT.substs typeBuiltins) <$> Parser.run (Parser.root TypeParser.valueType) s s0


unsafeParseTerm :: Var v => String -> PEnv -> Term v
unsafeParseTerm = fmap unsafeGetRight . parseTerm

unsafeParseType :: Var v => String -> PEnv -> Type v
unsafeParseType = fmap unsafeGetRight . parseType

unsafeParseTerm' :: Var v => [(v, Term v)] -> [(v, Type v)] -> String -> PEnv -> Term v
unsafeParseTerm' termBuiltins typeBuiltins =
  fmap unsafeGetRight . parseTerm' termBuiltins typeBuiltins

unsafeParseType' :: Var v => [(v, Type v)] -> String -> PEnv -> Type v
unsafeParseType' tr = fmap unsafeGetRight . parseType' tr

unsafeParseFile :: [(Symbol, Term Symbol)] -> String -> PEnv -> UnisonFile Symbol
unsafeParseFile b s pEnv = unsafeGetRight $ parseFile "" b s pEnv

unsafeParseFile' :: [(Symbol, Term Symbol)] -> String -> UnisonFile Symbol
unsafeParseFile' b s = unsafeGetRight $ parseFile "" b s Parser.penv0

unsafeReadAndParseFile' :: [(Symbol, Term Symbol)]
                        -> String -> IO (UnisonFile Symbol)
unsafeReadAndParseFile' b = unsafeReadAndParseFile b Parser.penv0

unsafeReadAndParseFile :: [(Symbol, Term Symbol)]
                       -> PEnv -> String -> IO (UnisonFile Symbol)
unsafeReadAndParseFile b penv filename = do
  txt <- readFile filename
  let str = Text.unpack txt
  pure $ unsafeGetRight (parseFile filename b str penv)
