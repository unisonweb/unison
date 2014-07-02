module Unison.Edit.Term.Eval where

import Unison.Note (Noted)
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Term (Term)
import Unison.Syntax.Type (Type)

data Eval m = Eval {
  whnf :: Term -> Noted m Term,  -- ^ Simplify to weak head normal form
  step :: Term -> Noted m Term,  -- ^ Perform one beta reduction
  term :: Hash -> Noted m Term,  -- ^ Lookup the source of a given term
  typ :: Hash -> Noted m Type    -- ^ Lookup the source of a given type
}
