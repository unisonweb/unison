{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Parsers where

import Unison.Prelude

import qualified Data.Text                     as Text
import           Data.Text.IO                   ( readFile )
import           Prelude                 hiding ( readFile )
import qualified Unison.NamesWithHistory                 as Names
import qualified Unison.Builtin                as Builtin
import qualified Unison.FileParser             as FileParser
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser                 as Parser
import           Unison.PrintError              ( prettyParseError
                                                , defaultWidth )
import           Unison.Symbol                  ( Symbol )
import           Unison.Term                    ( Term )
import qualified Unison.TermParser             as TermParser
import           Unison.Type                    ( Type )
import qualified Unison.TypeParser             as TypeParser
import           Unison.UnisonFile              ( UnisonFile )
import qualified Unison.Util.Pretty            as Pr
import           Unison.Var                     ( Var )

unsafeGetRightFrom :: (Var v, Show v) => String -> Either (Parser.Err v) a -> a
unsafeGetRightFrom src =
  either (error . Pr.toANSI defaultWidth . prettyParseError src) id

parse
  :: Var v
  => Parser.P v a
  -> String
  -> Parser.ParsingEnv
  -> Either (Parser.Err v) a
parse p = Parser.run (Parser.root p)

parseTerm
  :: Var v
  => String
  -> Parser.ParsingEnv
  -> Either (Parser.Err v) (Term v Ann)
parseTerm = parse TermParser.term

parseType
  :: Var v
  => String
  -> Parser.ParsingEnv
  -> Either (Parser.Err v) (Type v Ann)
parseType = Parser.run (Parser.root TypeParser.valueType)

parseFile
  :: Var v
  => FilePath
  -> String
  -> Parser.ParsingEnv
  -> Either (Parser.Err v) (UnisonFile v Ann)
parseFile filename s = Parser.run' (Parser.rootFile FileParser.file) s filename

readAndParseFile
  :: Var v
  => Parser.ParsingEnv
  -> FilePath
  -> IO (Either (Parser.Err v) (UnisonFile v Ann))
readAndParseFile penv fileName = do
  txt <- readFile fileName
  let src = Text.unpack txt
  pure $ parseFile fileName src penv

unsafeParseTerm :: Var v => String -> Parser.ParsingEnv -> Term v Ann
unsafeParseTerm s = fmap (unsafeGetRightFrom s) . parseTerm $ s

unsafeReadAndParseFile
  :: Parser.ParsingEnv -> FilePath -> IO (UnisonFile Symbol Ann)
unsafeReadAndParseFile penv fileName = do
  txt <- readFile fileName
  let str = Text.unpack txt
  pure . unsafeGetRightFrom str $ parseFile fileName str penv

unsafeParseFileBuiltinsOnly
  :: FilePath -> IO (UnisonFile Symbol Ann)
unsafeParseFileBuiltinsOnly =
  unsafeReadAndParseFile $ Parser.ParsingEnv
    mempty
    (Names.NamesWithHistory Builtin.names0 mempty)

unsafeParseFile
  :: String -> Parser.ParsingEnv -> UnisonFile Symbol Ann
unsafeParseFile s pEnv = unsafeGetRightFrom s $ parseFile "" s pEnv

