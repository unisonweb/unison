module Unison.Syntax.Term.Examples where

import Unison.Syntax.Term

identity :: ClosedTerm k t
identity = lam1 $ \x -> x
