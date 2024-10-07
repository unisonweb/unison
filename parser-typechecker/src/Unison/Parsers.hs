module Unison.Parsers where

import Unison.Parser.Ann (Ann)
import Unison.Syntax.FileParser qualified as FileParser
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.TypeParser qualified as TypeParser
import Unison.Type (Type)
import Unison.UnisonFile (UnisonFile)
import Unison.Var (Var)

parseType ::
  (Monad m, Var v) =>
  String ->
  Parser.ParsingEnv m ->
  m (Either (Parser.Err v) (Type v Ann))
parseType = Parser.run (Parser.root TypeParser.valueType)

parseFile ::
  (Monad m, Var v) =>
  FilePath ->
  String ->
  Parser.ParsingEnv m ->
  m (Either (Parser.Err v) (UnisonFile v Ann))
parseFile filename s = Parser.run' (Parser.rootFile FileParser.file) s filename
