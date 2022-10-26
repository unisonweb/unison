module Unison.Parsers where

import qualified Data.Text as Text
import qualified Unison.Builtin as Builtin
import qualified Unison.NamesWithHistory as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrintError
  ( defaultWidth,
    prettyParseError,
  )
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.FileParser as FileParser
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Syntax.TermParser as TermParser
import qualified Unison.Syntax.TypeParser as TypeParser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile (UnisonFile)
import qualified Unison.Util.Pretty as Pr
import Unison.Var (Var)

unsafeGetRightFrom :: (Var v, Show v) => String -> Either (Parser.Err v) a -> a
unsafeGetRightFrom src =
  either (error . Pr.toANSI defaultWidth . prettyParseError src) id

parse ::
  Var v =>
  Parser.P v a ->
  String ->
  Parser.ParsingEnv ->
  Either (Parser.Err v) a
parse p = Parser.run (Parser.root p)

parseTerm ::
  Var v =>
  String ->
  Parser.ParsingEnv ->
  Either (Parser.Err v) (Term v Ann)
parseTerm = parse TermParser.term

parseType ::
  Var v =>
  String ->
  Parser.ParsingEnv ->
  Either (Parser.Err v) (Type v Ann)
parseType = Parser.run (Parser.root TypeParser.valueType)

parseFile ::
  Var v =>
  FilePath ->
  String ->
  Parser.ParsingEnv ->
  Either (Parser.Err v) (UnisonFile v Ann)
parseFile filename s = Parser.run' (Parser.rootFile FileParser.file) s filename

readAndParseFile ::
  Var v =>
  Parser.ParsingEnv ->
  FilePath ->
  IO (Either (Parser.Err v) (UnisonFile v Ann))
readAndParseFile penv fileName = do
  txt <- readUtf8 fileName
  let src = Text.unpack txt
  pure $ parseFile fileName src penv

unsafeParseTerm :: Var v => String -> Parser.ParsingEnv -> Term v Ann
unsafeParseTerm s = fmap (unsafeGetRightFrom s) . parseTerm $ s

unsafeReadAndParseFile ::
  Parser.ParsingEnv -> FilePath -> IO (UnisonFile Symbol Ann)
unsafeReadAndParseFile penv fileName = do
  txt <- readUtf8 fileName
  let str = Text.unpack txt
  pure . unsafeGetRightFrom str $ parseFile fileName str penv

unsafeParseFileBuiltinsOnly ::
  FilePath -> IO (UnisonFile Symbol Ann)
unsafeParseFileBuiltinsOnly =
  unsafeReadAndParseFile $
    Parser.ParsingEnv
      mempty
      (Names.NamesWithHistory Builtin.names0 mempty)

unsafeParseFile ::
  String -> Parser.ParsingEnv -> UnisonFile Symbol Ann
unsafeParseFile s pEnv = unsafeGetRightFrom s $ parseFile "" s pEnv
