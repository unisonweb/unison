{-# LANGUAGE PatternSynonyms #-}

module Unison.Test.Common
  (
    t
  , tm
  , parseAndSynthesizeAsFile
  ) where

import Data.Maybe (fromJust)
import           Data.Sequence (Seq)
import qualified Data.Text as Text
import qualified Unison.Builtin as B
import qualified Unison.Names as Names
import qualified Unison.FileParsers as FP
import           Unison.Parser (Ann(..))
import qualified Unison.PrettyPrintEnv as PPE
import           Unison.PrintError              ( prettyParseError )
import           Unison.Result (Result, Note)
import           Unison.Symbol (Symbol)
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import           Unison.Var (Var)
import           Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.ABT                    as ABT
import qualified Unison.ConstructorType        as CT
import qualified Unison.Lexer                  as L
import qualified Unison.Parser                 as Parser
import qualified Unison.Reference              as R
import qualified Unison.TermParser             as TermParser
import qualified Unison.Type                   as Type
import qualified Unison.TypeParser             as TypeParser
import qualified Unison.Util.ColorText         as Color
import qualified Text.Megaparsec.Error         as MPE


type Term v = AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann

t :: String -> Type Symbol
t = undefined
--t s = ABT.amap (const Intrinsic)
--  . Names.bindType (Names.fromNames2 B.names)
--  . either (error . showParseError s) tweak
--  $ Parser.run (Parser.root TypeParser.valueType) s mempty
--  where tweak = Type.generalizeLowercase mempty

tm :: String -> Term Symbol
tm = undefined
--tm s = Names.bindTerm constructorType (Names.fromNames2 B.names)
--     . either (error . showParseError s) id
--     $ Parser.run (Parser.root TermParser.term) s (mempty, Names.fromNames2 B.names)
--  where
--  constructorType :: R.Reference -> CT.ConstructorType
--  constructorType r =
--    if any f (B.builtinDataDecls) then CT.Data
--    else if any f (B.builtinEffectDecls) then CT.Effect
--    else error "a builtin term referenced a constructor for a non-builtin type"
--    where
--    f :: (Symbol, (R.Reference, a)) -> Bool
--    f = (==r) . fst . snd

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
  (Parser.ParsingEnv mempty B.names (fromJust . B.constructorType))
  filename
  (Text.pack s)
