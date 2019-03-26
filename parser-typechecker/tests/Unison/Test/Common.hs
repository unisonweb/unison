{-# LANGUAGE PatternSynonyms #-}

module Unison.Test.Common where

import           Data.Functor.Identity (runIdentity)
import           Data.Sequence (Seq)
import qualified Data.Text as Text
import qualified Unison.Builtin as B
import qualified Unison.FileParsers as FP
import           Unison.Parser (Ann(..))
import qualified Unison.PrettyPrintEnv as PPE
import           Unison.Result (Result)
import qualified Unison.Result as Result
import           Unison.Result (Note)
import           Unison.Symbol (Symbol)
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import qualified Unison.Typechecker as Typechecker
import           Unison.Var (Var)
import           Unison.UnisonFile (TypecheckedUnisonFile)

type Term v = AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann

tm :: String -> Term Symbol
tm = B.tm

file
  :: String
  -> Result
       (Seq (Note Symbol Ann))
       (PPE.PrettyPrintEnv, Maybe (TypecheckedUnisonFile Symbol Ann))
file = parseAndSynthesizeAsFile [] ""

t :: String -> Type Symbol
t = B.t

typechecks :: String -> Bool
typechecks = runIdentity . Result.isSuccess . file

env :: Typechecker.Env Symbol Ann
env = Typechecker.Env Intrinsic [] B.typeLookup mempty

parseAndSynthesizeAsFile
  :: Var v
  => [Type v]
  -> FilePath
  -> String
  -> Result
       (Seq (Note v Ann))
       (PPE.PrettyPrintEnv, Maybe (TypecheckedUnisonFile v Ann))
parseAndSynthesizeAsFile ambient filename s = FP.parseAndSynthesizeFile
  ambient
  (\_deps -> pure B.typeLookup)
  B.names
  filename
  (Text.pack s)
