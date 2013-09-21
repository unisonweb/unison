module Unison.Syntax.Term.Examples where

import Unison.Syntax.Term

identity :: ClosedTerm
identity = lam1 $ \x -> x
