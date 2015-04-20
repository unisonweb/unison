module Unison.A_Eval where

import Unison.A_Hash (Hash)
import Unison.A_Term (Term)

data Eval m = Eval {
  whnf :: (Hash -> m Term) -> Term -> m Term, -- ^ Simplify to weak head normal form
  step :: (Hash -> m Term) -> Term -> m Term  -- ^ Perform one beta reduction
}
