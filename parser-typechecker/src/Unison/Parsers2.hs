module Unison.Parsers2 where

import Unison.Type (AnnotatedType)
import Unison.Term (AnnotatedTerm)
import Unison.Parser2 (PEnv, Ann)
import Unison.UnisonFile (UnisonFile)
import Unison.Var (Var)
import qualified Unison.Builtin as Builtin
import qualified Unison.Parser2 as Parser
import qualified Unison.FileParser2 as FileParser
import qualified Unison.TypeParser2 as TypeParser
import qualified Unison.TermParser2 as TermParser

unsafeGetRight :: Show v => Either (Parser.Err v Parser.Input) a -> a
unsafeGetRight = either (error . show) id

parseTerm :: Var v => String -> PEnv -> Either (Parser.Err v Parser.Input) (AnnotatedTerm v Ann)
parseTerm s env = Parser.run (Parser.root $ Parser.withinBlock TermParser.term) s env

parseType :: Var v => String -> PEnv -> Either (Parser.Err v Parser.Input) (AnnotatedType v Ann)
parseType s = Parser.run (Parser.root $ Parser.withinBlock TypeParser.valueType) s

parseFile :: Var v => FilePath -> String -> PEnv -> Either (Parser.Err v Parser.Input) (UnisonFile v Ann)
parseFile filename s = Parser.run'
  (Parser.root $ FileParser.file Builtin.builtinTerms Builtin.builtinTypes) s filename

unsafeParseTerm :: Var v => String -> PEnv -> AnnotatedTerm v Ann
unsafeParseTerm = fmap unsafeGetRight . parseTerm
