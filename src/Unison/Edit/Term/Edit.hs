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

apply :: (E.Literal -> Either Primop E.Term)
      -> Action E.Term -> P.Path -> E.Term -> Maybe E.Term
apply expandLit f loc e = go f where
  go Abstract = abstract loc e
  go Step = step expandLit loc e
  go _ = undefined

abstract :: P.Path -> E.Term -> Maybe E.Term
abstract loc e =
  let v = V.decr V.bound1 -- unused
  in do
    arg <- P.at loc e
    f <- E.Lam . E.abstract v <$> P.set (E.Var v) loc e
    pure $ E.App f arg


-- data Eval l = Eval {
--   step :: forall t. l -> Either (Primop l) Term,
--   whnf :: forall t. E.Term -> Maybe Term, -- fail if expr not closed
--   hnf :: forall t. E.Term -> Maybe Term, -- ditto
-- }
data Primop = Primop !Int ([E.Term] -> E.Term)

step :: (E.Literal -> Either Primop E.Term)
     -> P.Path
     -> E.Term
     -> Maybe E.Term
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
