module Unison.Edit.Term.Eval where

import Unison.Note (Noted)
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Term (Term)
import Unison.Syntax.Type (Type)

data Eval m = Eval {
  whnf :: (Hash -> Noted m Term)
       -> Term
       -> Noted m Term,  -- ^ Simplify to weak head normal form
  step :: (Hash -> Noted m Term)
       -> Term
       -> Noted m Term  -- ^ Perform one beta reduction
}
