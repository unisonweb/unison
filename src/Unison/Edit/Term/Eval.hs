module Unison.Edit.Term.Eval where

import Unison.Syntax.Term

data Eval m = Eval {
  whnf :: Term -> m Term, -- ^ Simplify to weak head normal form
  hnf :: Term -> m Term,  -- ^ Simplify to head normal form
  step :: Term -> m Term  -- ^ Perform one beta reduction
}
