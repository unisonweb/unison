module Unison.Edit.Term.Eval where

import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Term (Term)
import Unison.Syntax.Reference (Reference)

data Eval m = Eval {
  whnf :: (Hash -> m Term) -> Term -> m Term, -- ^ Simplify to weak head normal form
  step :: (Hash -> m Term) -> Term -> m Term  -- ^ Perform one beta reduction
}
