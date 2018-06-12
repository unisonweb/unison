{-# LANGUAGE OverloadedStrings #-}

module Unison.Parsers where

import qualified Data.Text as Text
import           Data.Text.IO (readFile)
import           Prelude hiding (readFile)
import qualified Unison.ABT as ABT
import qualified Unison.FileParser as FileParser
import           Unison.Parser (PEnv)
import qualified Unison.Parser as Parser
import           Unison.Symbol (Symbol)
import           Unison.Term (Term)
import qualified Unison.TermParser as TermParser
import           Unison.Type (Type)
import qualified Unison.TypeParser as TypeParser
import           Unison.UnisonFile (UnisonFile)
import           Unison.Var (Var)

type S v = TypeParser.S v

s0 :: S v
s0 = TypeParser.s0

unsafeGetRight :: Either String a -> a
unsafeGetRight (Right a) = a
unsafeGetRight (Left err) = error err

parseTerm :: Var v => String -> PEnv -> Either String (Term v)
parseTerm s = Parser.run (Parser.root TermParser.term) s s0

parseType :: Var v => String -> PEnv -> Either String (Type v)
parseType = parseType' []

parseFile :: Var v => FilePath -> String -> PEnv -> Either String (UnisonFile v)
parseFile filename s = Parser.run' (Parser.root FileParser.file) s s0 filename

parseType' :: Var v => [(v, Type v)] -> String -> PEnv -> Either String (Type v)
parseType' typeBuiltins s =
  fmap (ABT.substs typeBuiltins) <$> Parser.run (Parser.root TypeParser.valueType) s s0


unsafeParseTerm :: Var v => String -> PEnv -> Term v
unsafeParseTerm = fmap unsafeGetRight . parseTerm

unsafeParseType :: Var v => String -> PEnv -> Type v
unsafeParseType = fmap unsafeGetRight . parseType

unsafeParseFile ::String -> PEnv -> UnisonFile Symbol
unsafeParseFile s pEnv = unsafeGetRight $ parseFile "" s pEnv

unsafeParseFile' :: String -> UnisonFile Symbol
unsafeParseFile' s = unsafeGetRight $ parseFile "" s Parser.penv0

unsafeReadAndParseFile' :: String -> IO (UnisonFile Symbol)
unsafeReadAndParseFile' = unsafeReadAndParseFile Parser.penv0

unsafeReadAndParseFile :: PEnv -> String -> IO (UnisonFile Symbol)
unsafeReadAndParseFile penv filename = do
  txt <- readFile filename
  let str = Text.unpack txt
  pure $ unsafeGetRight (parseFile filename str penv)
