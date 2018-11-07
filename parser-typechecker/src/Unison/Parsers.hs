module Unison.Parsers where

import qualified Data.Text as Text
import           Data.Text.IO (readFile)
import           Prelude hiding (readFile)
import qualified Unison.Builtin as Builtin
import qualified Unison.FileParser as FileParser
import           Unison.Parser (Ann)
import qualified Unison.Parser as Parser
import           Unison.PrintError (prettyParseError)
import           Unison.Symbol (Symbol)
import           Unison.Term (AnnotatedTerm)
import qualified Unison.TermParser as TermParser
import           Unison.Type (AnnotatedType)
import qualified Unison.TypeParser as TypeParser
import           Unison.UnisonFile (UnisonFile)
import qualified Unison.Util.ColorText as Color
import           Unison.Var (Var)
import Unison.Names (Names(..))
import qualified Unison.PrettyPrintEnv as PPE

unsafeGetRightFrom :: (Var v, Show v) => String -> Either (Parser.Err v) a -> a
unsafeGetRightFrom src =
  either (error . show . Color.renderText . prettyParseError src) id

parse :: Var v => Parser.P v a -> String -> Names v Ann -> Either (Parser.Err v) a
parse p s env = Parser.run (Parser.root p) s env

parseTerm :: Var v => String -> Names v Ann -> Either (Parser.Err v) (AnnotatedTerm v Ann)
parseTerm s env = parse TermParser.term s env

parseType :: Var v => String -> Names v Ann -> Either (Parser.Err v) (AnnotatedType v Ann)
parseType s = Parser.run (Parser.root TypeParser.valueType) s

parseFile :: Var v
  => FilePath -> String -> Names v Ann
  -> Either (Parser.Err v) (PPE.PrettyPrintEnv, UnisonFile v Ann)
parseFile filename s =
  Parser.run' (Parser.rootFile FileParser.file) s filename

readAndParseFile :: Var v => Names v Ann -> FilePath -> IO (Either (Parser.Err v) (PPE.PrettyPrintEnv, UnisonFile v Ann))
readAndParseFile penv fileName = do
  txt <- readFile fileName
  let src = Text.unpack txt
  pure $ parseFile fileName src penv

unsafeParseTerm :: Var v => String -> Names v Ann -> AnnotatedTerm v Ann
unsafeParseTerm s = fmap (unsafeGetRightFrom s) . parseTerm $ s

unsafeReadAndParseFile :: Names Symbol Ann -> FilePath -> IO (PPE.PrettyPrintEnv, UnisonFile Symbol Ann)
unsafeReadAndParseFile penv fileName = do
  txt <- readFile fileName
  let str = Text.unpack txt
  pure . unsafeGetRightFrom str $ parseFile fileName str penv

unsafeReadAndParseFile' :: String -> IO (PPE.PrettyPrintEnv, UnisonFile Symbol Ann)
unsafeReadAndParseFile' = unsafeReadAndParseFile Builtin.names

unsafeParseFile :: String -> Names Symbol Ann -> (PPE.PrettyPrintEnv, UnisonFile Symbol Ann)
unsafeParseFile s pEnv = unsafeGetRightFrom s $ parseFile "" s pEnv

unsafeParseFile' :: String -> (PPE.PrettyPrintEnv, UnisonFile Symbol Ann)
unsafeParseFile' s = unsafeParseFile s Builtin.names
