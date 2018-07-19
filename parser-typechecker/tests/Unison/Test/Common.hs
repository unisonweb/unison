module Unison.Test.Common where

import           Control.Error (hush, isRight)
import qualified Data.Map as Map
import qualified Unison.Builtin as B
import qualified Unison.FileParsers as FP
import           Unison.Symbol (Symbol)
import           Unison.Term (Term)
import           Unison.Type (Type)
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Result as Result

tm :: String -> Term Symbol
tm = B.tm

file :: String -> Either String (Term Symbol, Type Symbol)
file = FP.parseAndSynthesizeAsFile ""

fileTerm :: String -> Either String (Term Symbol)
fileTerm = fmap fst . FP.parseAndSynthesizeAsFile "<test>"

fileTermType :: String -> Maybe (Term Symbol, Type Symbol)
fileTermType = hush . FP.parseAndSynthesizeAsFile "<test>"

t :: String -> Type Symbol
t = B.t

typechecks :: String -> Bool
typechecks = isRight . file

env :: Monad m => Typechecker.Env m v ()
env = Typechecker.Env () [] typeOf dd ed where
  typeOf r = maybe (error $ "no type for: " ++ show r) pure $ Map.lookup r B.builtins
  dd r = error $ "no data declaration for: " ++ show r
  ed r = error $ "no effect declaration for: " ++ show r

typechecks' :: Term Symbol -> Bool
typechecks' term = Result.isSuccess $ Typechecker.synthesize env term

check' :: Term Symbol -> Type Symbol -> Bool
check' term typ = Result.isSuccess $ Typechecker.check env term typ

check :: String -> String -> Bool
check terms typs = check' (tm terms) (t typs)
