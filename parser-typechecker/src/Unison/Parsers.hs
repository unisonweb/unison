module Unison.Parsers where

import qualified Data.Text as Text
import           Data.Text.IO (readFile)
import           Prelude hiding (readFile)
import qualified Unison.Builtin as Builtin
import qualified Unison.FileParser as FileParser
import           Unison.Parser (PEnv, Ann)
import qualified Unison.Parser as Parser
import           Unison.Symbol (Symbol)
import           Unison.Term (AnnotatedTerm)
import qualified Unison.TermParser as TermParser
import           Unison.Type (AnnotatedType)
import qualified Unison.TypeParser as TypeParser
import           Unison.UnisonFile (UnisonFile)
import           Unison.Var (Var)

unsafeGetRight :: Show v => Either (Parser.Err v) a -> a
unsafeGetRight = either (error . show) id

parse :: Var v => Parser.P v a -> String -> PEnv -> Either (Parser.Err v) a
parse p s env = Parser.run (Parser.root $ Parser.withinBlock p) s env

parseTerm :: Var v => String -> PEnv -> Either (Parser.Err v) (AnnotatedTerm v Ann)
parseTerm s env = parse TermParser.term s env

parseType :: Var v => String -> PEnv -> Either (Parser.Err v) (AnnotatedType v Ann)
parseType s = Parser.run (Parser.root $ Parser.withinBlock TypeParser.valueType) s

parseFile :: Var v => FilePath -> String -> PEnv -> Either (Parser.Err v) (UnisonFile v Ann)
parseFile filename s = Parser.run'
  (Parser.root $ FileParser.file Builtin.builtinTerms Builtin.builtinTypes) s filename

unsafeParseTerm :: Var v => String -> PEnv -> AnnotatedTerm v Ann
unsafeParseTerm = fmap unsafeGetRight . parseTerm

unsafeReadAndParseFile :: PEnv -> String -> IO (UnisonFile Symbol Ann)
unsafeReadAndParseFile penv fileName = do
  txt <- readFile fileName
  let str = Text.unpack txt
  pure . unsafeGetRight $ parseFile fileName str penv

unsafeReadAndParseFile' :: String -> IO (UnisonFile Symbol Ann)
unsafeReadAndParseFile' = unsafeReadAndParseFile Parser.penv0

unsafeParseFile :: String -> PEnv -> UnisonFile Symbol Ann
unsafeParseFile s pEnv = unsafeGetRight $ parseFile "" s pEnv

unsafeParseFile' :: String -> UnisonFile Symbol Ann
unsafeParseFile' s = unsafeParseFile s Parser.penv0
