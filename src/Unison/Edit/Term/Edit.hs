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

apply :: (l -> Either (Primop l t) (E.Term l t))
      -> Action (E.Term l t) -> P.Path -> E.Term l t -> Maybe (E.Term l t)
apply expandLit f loc e = go f where
  go Abstract = abstract loc e
  go Step = step expandLit loc e
  go _ = undefined

abstract :: P.Path -> E.Term l t -> Maybe (E.Term l t)
abstract loc e =
  let v = V.decr V.bound1 -- unused
  in do
    arg <- P.at loc e
    f <- E.Lam . E.abstract v <$> P.set (E.Var v) loc e
    pure $ E.App f arg


-- data Eval l = Eval {
--   step :: forall t. l -> Either (Primop l) (E.Term l t),
--   whnf :: forall t. E.Term l t -> Maybe (E.Term l t), -- fail if expr not closed
--   hnf :: forall t. E.Term l t -> Maybe (E.Term l t), -- ditto
-- }
data Primop l t = Primop !Int ([E.Term l t] -> E.Term l t)

step :: (l -> Either (Primop l t) (E.Term l t))
     -> P.Path
     -> E.Term l t
     -> Maybe (E.Term l t)
step expandLit loc e = do
  (e', wrap) <- E.stripAnn <$> P.at loc e
  pure . wrap $ case e' of
    E.Lit l -> case expandLit l of
      Right e'' -> e''
      Left _ -> E.Lit l
    E.App (E.Lit f) x -> case expandLit f of
      Right f' -> E.betaReduce (E.App f' x)
      Left (Primop n f') -> let args = E.arguments e' in
        if length args <= n then E.applyN (f' (take n args)) (drop n args)
        else e'
    _ -> E.betaReduce e'
