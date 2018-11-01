{-# LANGUAGE PatternSynonyms #-}

module Unison.Test.Common where

import           Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Unison.Builtin as B
import qualified Unison.FileParsers as FP
import           Unison.Parser (Ann(..))
import qualified Unison.Parsers as Parsers
import qualified Unison.PrettyPrintEnv as PPE
import           Unison.Result (Result)
import qualified Unison.Result as Result
import           Unison.Result (pattern Result, Note)
import           Unison.Symbol (Symbol)
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import qualified Unison.Typechecker as Typechecker
import           Unison.Var (Var)
import           Unison.UnisonFile (TypecheckedUnisonFile')

type Term v = AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann

tm :: String -> Term Symbol
tm = B.tm

file
  :: String
  -> Result
       (Seq (Note Symbol Ann))
       (PPE.PrettyPrintEnv, Maybe (TypecheckedUnisonFile' Symbol Ann))
file = parseAndSynthesizeAsFile ""

t :: String -> Type Symbol
t = B.t

typechecks :: String -> Bool
typechecks = runIdentity . Result.isSuccess . file

env :: Monad m => Typechecker.Env m Symbol Ann
env = Typechecker.Env Intrinsic [] typeOf dd ed Map.empty where
  typeOf r = error $ "no type for: " ++ show r
  dd r = error $ "no data declaration for: " ++ show r
  ed r = error $ "no effect declaration for: " ++ show r

parseAndSynthesizeAsFile
  :: Var v
  => FilePath
  -> String
  -> Result (Seq (Note v Ann))
            (PPE.PrettyPrintEnv, Maybe (TypecheckedUnisonFile' v Ann))
parseAndSynthesizeAsFile filename s = do
  (errorEnv, file) <- Result.fromParsing
    $ Parsers.parseFile filename s B.names
  let (Result notes' r) = FP.synthesizeFile B.names file
  Result notes' $ Just (errorEnv, r)

