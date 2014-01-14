module Unison.Edit.Term.Edit where

import Control.Applicative
import Unison.Edit.Term.Action
import qualified Unison.Edit.Term.Path as P
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Var as V

-- data Edit e = Edit [(Path, Action e)]
-- edits are context free
-- but when applying an edit, have to pick a context
-- context is just a function, which editing target must be applied to

apply :: Action (E.Term l t) -> P.Path -> E.Term l t -> Maybe (E.Term l t)
apply f loc e = undefined

abstract :: P.Path -> E.Term l t -> Maybe (E.Term l t)
abstract loc e =
  let v = V.decr V.bound1 -- unused
  in do
    arg <- P.at loc e
    f <- E.Lam . E.abstract v <$> P.set (E.Var v) loc e
    pure $ E.App f arg
