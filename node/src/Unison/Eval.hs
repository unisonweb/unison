module Unison.Eval where

import Unison.Hash (Hash)
import Unison.Term (Term)

data Eval m = Eval {
  whnf :: (Hash -> m Term) -> Term -> m Term, -- ^ Simplify to weak head normal form
  step :: (Hash -> m Term) -> Term -> m Term  -- ^ Perform one beta reduction
}
