module Unison.Parsers where

import Unison.Prelude

import           Prelude
import qualified Unison.FileParser             as FileParser
import           Unison.Parser                  ( Ann )
import qualified Unison.Parser                 as Parser
import           Unison.Type                    ( Type )
import qualified Unison.TypeParser             as TypeParser
import           Unison.UnisonFile              ( UnisonFile )
import           Unison.Var                     ( Var )

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
