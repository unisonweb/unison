module Unison.Syntax.Term.Examples where

import Unison.Syntax.Term

identity :: Term l t
identity = lam1 $ \x -> x
