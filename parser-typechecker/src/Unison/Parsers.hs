module Unison.Parsers where

import Unison.Prelude

import qualified Data.Text                     as Text
import           Data.Text.IO                   ( readFile )
import           Prelude                 hiding ( readFile )
import qualified Unison.Names3                 as Names
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
import qualified Unison.PrettyPrintEnv as PPE

unsafeGetRightFrom
  :: (Var v, Show v)
  => PPE.PrettyPrintEnv
  -> String
  -> Either (Parser.Err v) a
  -> a
unsafeGetRightFrom ppe src =
  either (error . Pr.toANSI defaultWidth . prettyParseError ppe src) id

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

unsafeParseTerm
    :: Var v
    => PPE.PrettyPrintEnv
    -> String
    -> Parser.ParsingEnv
    -> Term v Ann
unsafeParseTerm ppe s = fmap (unsafeGetRightFrom ppe s) . parseTerm $ s

unsafeReadAndParseFile
  :: PPE.PrettyPrintEnv
  -> Parser.ParsingEnv
  -> FilePath
  -> IO (UnisonFile Symbol Ann)
unsafeReadAndParseFile ppe penv fileName = do
  txt <- readFile fileName
  let str = Text.unpack txt
  pure . unsafeGetRightFrom ppe str $ parseFile fileName str penv

unsafeParseFileBuiltinsOnly
  :: PPE.PrettyPrintEnv
  -> FilePath
  -> IO (UnisonFile Symbol Ann)
unsafeParseFileBuiltinsOnly ppe fp =
  let parseEnv = Parser.ParsingEnv mempty (Names.Names Builtin.names0 mempty)
   in unsafeReadAndParseFile ppe parseEnv fp

unsafeParseFile
  :: String
  -> PPE.PrettyPrintEnv
  -> Parser.ParsingEnv
  -> UnisonFile Symbol Ann
unsafeParseFile s ppe pEnv = unsafeGetRightFrom ppe s $ parseFile "" s pEnv

