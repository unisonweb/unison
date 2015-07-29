module Unison.Eval where

import Unison.Hash (Hash)
import Unison.Term (Term)

data Eval m v = Eval {
  whnf :: (Hash -> m (Term v)) -> Term v -> m (Term v), -- ^ Simplify to weak head normal form
  step :: (Hash -> m (Term v)) -> Term v -> m (Term v)  -- ^ Perform one beta reduction
}
