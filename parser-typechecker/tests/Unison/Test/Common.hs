module Unison.Test.Common where

import Unison.Parsers (unsafeParseType, unsafeParseTerm, unsafeParseFile')
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Symbol (Symbol)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.Builtin as B
import qualified Unison.FileParser as FileParser
import qualified Unison.Note as N
import qualified Unison.Typechecker as Typechecker
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Var as Var
import qualified Unison.Reference as R

tm :: String -> Term Symbol
tm = B.tm

t :: String -> Type Symbol
t = B.t

typechecks :: String -> Bool
typechecks = typechecks' . tm

typechecks' :: Term Symbol -> Bool
typechecks' term = let
  typeOf r = maybe (fail $ "no type for: " ++ show r) pure $ Map.lookup r B.builtins
  declFor r = fail $ "no data declaration for: " ++ show r
  ok = Typechecker.synthesize typeOf declFor term
  in case N.run ok of
    Left e -> False
    Right _ -> True

check' :: Term Symbol -> Type Symbol -> Bool
check' term typ = let
  typeOf r = maybe (fail $ "no type for: " ++ show r) pure $ Map.lookup r B.builtins
  declFor r = fail $ "no data declaration for: " ++ show r
  ok = Typechecker.check typeOf declFor term typ
  in case N.run ok of
    Left e -> False
    Right _ -> True

check :: String -> String -> Bool
check terms typs = check' (tm terms) (t typs)
