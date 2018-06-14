module Unison.Test.Common where

import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Symbol (Symbol)
import Unison.FileParsers (parseAndSynthesizeAsFile)
import Control.Arrow (second)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Unison.Builtin as B
import qualified Unison.Note as N
import qualified Unison.Typechecker as Typechecker

tm :: String -> Term Symbol
tm = B.tm

file :: String -> (Term Symbol, Either String (Type Symbol))
file = parseAndSynthesizeAsFile ""

fileTerm :: String -> Term Symbol
fileTerm = fst . parseAndSynthesizeAsFile ""

fileTermType :: String -> (Term Symbol, Maybe (Type Symbol))
fileTermType = second (either (const Nothing) Just) . parseAndSynthesizeAsFile ""

t :: String -> Type Symbol
t = B.t

typechecks :: String -> Bool
typechecks = typechecks' . fst . file

typechecks' :: Term Symbol -> Bool
typechecks' term = let
  typeOf r = maybe (fail $ "no type for: " ++ show r) pure $ Map.lookup r B.builtins
  declFor r = fail $ "no data declaration for: " ++ show r
  ok = Typechecker.synthesize typeOf declFor term
  in case N.run ok of
    Left _e -> False
    Right _ -> True

fileTypeChecks :: String -> Bool
fileTypeChecks = isJust . snd . fileTermType

check' :: Term Symbol -> Type Symbol -> Bool
check' term typ = let
  typeOf r = maybe (fail $ "no type for: " ++ show r) pure $ Map.lookup r B.builtins
  declFor r = fail $ "no data declaration for: " ++ show r
  ok = Typechecker.check typeOf declFor term typ
  in case N.run ok of
    Left _e -> False
    Right _ -> True

check :: String -> String -> Bool
check terms typs = check' (tm terms) (t typs)
