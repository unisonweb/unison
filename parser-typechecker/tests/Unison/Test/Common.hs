{-# LANGUAGE PatternSynonyms #-}

module Unison.Test.Common
  ( hqLength
  , t
  , tm
  , parseAndSynthesizeAsFile
  , parsingEnv
  ) where

import           Data.Sequence (Seq)
import qualified Data.Text as Text
import qualified Unison.Builtin as B
import qualified Unison.FileParsers as FP
import           Unison.Parser (Ann(..))
import           Unison.PrintError              ( prettyParseError )
import           Unison.Result (Result, Note)
import           Unison.Symbol (Symbol)
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import           Unison.Var (Var)
import           Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.ABT                    as ABT
import qualified Unison.Lexer                  as L
import qualified Unison.Parser                 as Parser
import qualified Unison.TermParser             as TermParser
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.TypeParser             as TypeParser
import qualified Unison.Util.ColorText         as Color
import qualified Text.Megaparsec.Error         as MPE


type Term v = AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann

hqLength :: Int
hqLength = 10

t :: String -> Type Symbol
t s = ABT.amap (const Intrinsic)
  . either (error . show ) id
  . Type.bindNames mempty B.names0
  . either (error . showParseError s) tweak
  $ Parser.run (Parser.root TypeParser.valueType) s parsingEnv
  where tweak = Type.generalizeLowercase mempty

tm :: String -> Term Symbol
tm s = either (error . show) id
     . Term.bindNames mempty B.names0
     . either (error . showParseError s) id
     $ Parser.run (Parser.root TermParser.term) s parsingEnv

showParseError :: Var v
               => String
               -> MPE.ParseError (L.Token L.Lexeme) (Parser.Error v)
               -> String
showParseError s = Color.toANSI . prettyParseError s

parseAndSynthesizeAsFile
  :: Var v
  => [Type v]
  -> FilePath
  -> String
  -> Result
       (Seq (Note v Ann))
       (Maybe (TypecheckedUnisonFile v Ann))
parseAndSynthesizeAsFile ambient filename s = FP.parseAndSynthesizeFile
  ambient
  (\_deps -> pure B.typeLookup)
  parsingEnv
  filename
  (Text.pack s)

parsingEnv :: Parser.ParsingEnv
parsingEnv = Parser.ParsingEnv mempty B.names
