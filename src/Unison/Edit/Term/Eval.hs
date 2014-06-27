module Unison.Edit.Term.Eval where

import Unison.Note (Noted)
import Unison.Syntax.Term

data Eval m = Eval {
  whnf :: Term -> Noted m Term, -- ^ Simplify to weak head normal form
  hnf :: Term -> Noted m Term,  -- ^ Simplify to head normal form
  step :: Term -> Noted m Term  -- ^ Perform one beta reduction
}
