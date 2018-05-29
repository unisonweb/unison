module Unison.FileParser where

import qualified Text.Parsec.Layout as L
-- import           Text.Parsec.Prim (ParsecT)
import           Unison.Parser
-- import qualified Unison.TypeParser as TypeParser
import Control.Applicative
import Data.Either (partitionEithers)
import Unison.Parsers (unsafeGetRight)
import Unison.DataDeclaration (DataDeclaration(..))
import Unison.EffectDeclaration (EffectDeclaration(..))
import Unison.Parser (PEnv)
import Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
import Unison.Var (Var)
import Unison.Symbol (Symbol)
import Data.Map (Map)
import qualified Data.Map as Map
import Unison.TypeParser (S)

data UnisonFile v = UnisonFile {
  dataDeclarations :: Map v (DataDeclaration v),
  effectDeclarations :: Map v (EffectDeclaration v),
  term :: Term v
} deriving (Show)

unsafeParseFile :: String -> PEnv -> UnisonFile Symbol
unsafeParseFile s env = unsafeGetRight $ parseFile s env

parseFile :: String -> PEnv -> Either String (UnisonFile Symbol)
parseFile = error ""

file :: Var v => Parser (S v) (UnisonFile v)
file = traced "file" $ do
  (dataDecls, effectDecls) <- traced "declarations" declarations
  term <- TermParser.block
  pure $ UnisonFile dataDecls effectDecls term

declarations :: Var v => Parser (S v)
                         (Map v (DataDeclaration v),
                          Map v (EffectDeclaration v))
declarations = do
  declarations <- many ((Left <$> dataDeclaration) <|> Right <$> effectDeclaration)
  let (dataDecls, effectDecls) = partitionEithers declarations
  pure (Map.fromList dataDecls, Map.fromList effectDecls)


dataDeclaration :: Var v => Parser (S v) (v, DataDeclaration v)
dataDeclaration = traced "data declaration" $ do
  token_ $ string "type"
  (name, typeArgs) <- --L.withoutLayout "type introduction" $
    (,) <$> TermParser.prefixVar <*> traced "many prefixVar" (many TermParser.prefixVar)
  traced "=" . token_ $ string "="
  traced "vblock" $ L.vblockIncrement $ do
    constructors <- traced "constructors" $ sepBy (token_ $ string "|") dataConstructor
    pure $ (name, DataDeclaration typeArgs constructors)
  where
    dataConstructor = traced "data contructor" $ (,) <$> TermParser.prefixVar
                          <*> (traced "many typeLeaf" $ many TypeParser.valueTypeLeaf)

effectDeclaration :: Var v => Parser (S v) (v, EffectDeclaration v)
effectDeclaration = traced "effect declaration" $ do
  token_ $ string "effect"
  name <- TermParser.prefixVar
  typeArgs <- many TermParser.prefixVar
  token_ $ string "where"
  L.vblockNextToken $ do
    constructors <- sepBy L.vsemi constructor
    pure $ (name, EffectDeclaration typeArgs constructors)
  where
    constructor = (,) <$> (TermParser.prefixVar <* token_ (string ":")) <*> traced "computation type" TypeParser.computationType
