module Unison.Codebase.TermEdit.Typing where

import Unison.Codebase.TermEdit (Typing (Different, Same, Subtype))
import Unison.Type (Type)
import qualified Unison.Typechecker as Typechecker
import Unison.Var (Var)

typing :: (Var v) => Type v loc -> Type v loc -> Typing
typing newType oldType
  | Typechecker.isEqual newType oldType = Same
  | Typechecker.isSubtype newType oldType = Subtype
  | otherwise = Different
