module Unison.FileParser where

-- import qualified Text.Parsec.Layout as L
-- import           Text.Parsec.Prim (ParsecT)
-- import           Unison.Parser
-- import qualified Unison.TypeParser as TypeParser
import Unison.Parsers (unsafeGetRight)
import Unison.DataDeclaration (DataDeclaration)
import Unison.EffectDeclaration (EffectDeclaration)
import Unison.Parser (PEnv)
import Unison.Term (Term)
import Unison.Symbol (Symbol)
import Data.Map (Map)

data UnisonFile = UnisonFile {
  dataDeclarations :: Map Symbol (DataDeclaration Symbol),
  effectDeclaration :: Map Symbol (EffectDeclaration Symbol),
  term :: Term Symbol
} deriving (Show)

unsafeParseFile :: String -> PEnv -> UnisonFile
unsafeParseFile s env = unsafeGetRight $ parseFile s env

parseFile :: String -> PEnv -> Either String UnisonFile
parseFile = error ""
